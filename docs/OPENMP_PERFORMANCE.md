# OpenMP Performance — AQUABC Pelagic Kinetics

Benchmark of the OpenMP parallelization of `AQUABC_PELAGIC_KINETICS` (the `!$omp
parallel` region over the `nkn` spatial nodes, `aquabc_II_pelagic_model.f90`).
Covers backlog **TODO 4.1** (benchmarking), **4.2** (CO2SYS parallelization), and
**4.3** (thread-affinity guidance).

## TL;DR

- OpenMP speedup **grows with `nkn`** and, after parallelizing CO2SYS (4.2),
  reaches **6.55× at `nkn=1000` on 8 threads** (82% efficiency) — up from 2.84×
  when CO2SYS was still serial.
- **Parallelizing CO2SYS (4.2) roughly doubled the large-`nkn` speedup**: the
  serial CO2SYS carbonate-chemistry solver (called once per timestep) was the
  dominant Amdahl serial fraction (~15% of kinetics time, ~26% effective serial
  fraction). Chunking it across threads removed that ceiling.
- **Recommendation:** enable OpenMP (`make OPENMP=1 …`) for runs with `nkn ≳ 500`;
  4–8 threads now scale well at large `nkn`. Still not worth it for small networks
  (`nkn ≲ 100`, e.g. the default 25-box / CL29 29-box cases).
- **Caveat:** the full-model `ESTAS_II` executable currently **hangs at high thread
  counts** (≥8) — a **pre-existing** issue in the ESTAS solver/transport path,
  independent of the kinetics/CO2SYS parallelization measured here (see §Full-model).

## Method

The full-model examples are too small to exercise the parallel region (default = 25
boxes, CL29 = 29; with static block scheduling `chunk = ceil(nkn/threads)`, at
`nkn=25` on 8 threads only 7 threads get work). So the benchmark reuses the
**node-agnostic 0D interface** (`aquabc_init` / `aquabc_run`) to call the **real**
`AQUABC_PELAGIC_KINETICS` — its real per-thread bundles, the real `!$omp parallel`
region, and (after 4.2) the parallel CO2SYS — across `nkn` replicated nodes.

- **Driver:** `SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/aquabc_II_pelagic_benchmark.f90`
  (`BENCH_NKN`/`BENCH_STEPS` env-driven; fixed-IC reset per step → reproducible;
  timed with `omp_get_wtime()`; one untimed warm-up call).
- **What is timed:** one full kinetics step, `aquabc_run` = CO2SYS **+** the
  parallel kinetics region — the real per-timestep cost.
- **Build:** `make OPENMP=1 FC=gfortran BUILD_TYPE=release build-lib` (`-O3
  -march=native -ffast-math -flto -fopenmp`), driver `-fopenmp -O3 -march=native`.
- **Baseline:** the same OpenMP binary at `OMP_NUM_THREADS=1`. Speedup = T₁/Tₙ.
- **Hardware:** Intel Core i9-10940X (14 physical cores / 28 threads, single
  socket), gfortran 13.3.0, Ubuntu 24.04.
- **Reproduce:** `make benchmark-openmp` (or `tools/benchmark_openmp.sh [--quick]`).

## Results — strong scaling (with CO2SYS parallelized, 4.2)

| `nkn` | threads | µs / step | speedup | efficiency |
|------:|--------:|----------:|--------:|-----------:|
| 100   | 1 | 578.5  | 1.00 | 100 % |
| 100   | 2 | 387.5  | 1.49 |  75 % |
| 100   | 4 | 295.4  | 1.96 |  49 % |
| 100   | 8 | 240.1  | 2.41 |  30 % |
| 500   | 1 | 3515.4 | 1.00 | 100 % |
| 500   | 2 | 1846.7 | 1.90 |  95 % |
| 500   | 4 | 1079.4 | 3.26 |  81 % |
| 500   | 8 | 777.1  | 4.52 |  57 % |
| 1000  | 1 | 9371.8 | 1.00 | 100 % |
| 1000  | 2 | 4063.5 | **2.31** | 115 % |
| 1000  | 4 | 2132.9 | **4.39** | 110 % |
| 1000  | 8 | 1430.4 | **6.55** |  82 % |

The **super-linear** efficiency at `nkn=1000` (2 & 4 threads > 100%) is a cache
effect: chunking shrinks each thread's working set enough to fit better in L2/L3.

### Before vs after parallelizing CO2SYS (4.2), `nkn=1000`

| threads | CO2SYS serial | **CO2SYS parallel** |
|--------:|--------------:|--------------------:|
| 2 | 1.87× | **2.31×** |
| 4 | 2.51× | **4.39×** |
| 8 | 2.84× | **6.55×** |

Parallelizing CO2SYS lifted the 8-thread ceiling from 2.84× to 6.55× — confirming
the profile (below) that CO2SYS was the serial bottleneck.

## CO2SYS parallelization (TODO 4.2)

**Profile (gprof, serial, nkn=1000):** `aquabc_pelagic_kinetics` 78% self-time,
**CO2SYS family ~15%** (`calculatephfromtatc` 5.5%, `constants` 5%, `casolubility`
2%, …), misc ~7%. CO2SYS was **>10%** of kinetics time → parallelization warranted.

**Change** (`aquabc_II_pelagic_model.f90`, the `RUN_CO2SYS` block): CO2SYS is a
**pure** function of its arguments (no `SAVE`/`COMMON`/module state), so each thread
runs it on its own `[ns:ne]` node-slice with **private output buffers**, then
scatters into the disjoint slice of the shared output arrays — the same static
block schedule the kinetics region uses. `co2sys.f90` itself is unchanged. (Scope:
the pelagic call site; the 4 sediment-model CO2SYS call sites are a follow-up.)

**Correctness — verified numerically negligible drift.** CO2SYS's pH solver is a
whole-vector Newton iteration that runs until the *slowest* element in the passed
vector converges, updating every element each iteration — so chunking changes how
many refinement iterations each node gets. It is therefore **not bit-identical**,
but the drift is far below the solver's own `pHTol = 1e-4` physical tolerance:

- **0D golden regression (`nkn=1`): bit-identical, passes** — a single node is
  always one chunk, so there is no chunking difference. No golden regeneration
  needed.
- **Full model, `nkn=25` (heterogeneous boxes), full 365-day run, 1 vs 2 threads,
  68.5M output values:** max absolute diff **1e-6** (= the output files' ~6-sig-fig
  print precision, not the computation), max relative diff **7.8e-9** — i.e. at or
  below output precision and **~1000× below the physical tolerance**. Physically
  negligible.
- **Micro-benchmark (`nkn=1000`, replicated nodes), 1 vs 8 threads: bit-identical**
  (identical nodes converge in lockstep, so chunking cannot change iteration count).

## Full-model OpenMP status (pre-existing hang at high thread counts)

The **micro-benchmark** (real kinetics + CO2SYS) scales cleanly to 8 threads. The
**full `ESTAS_II` executable**, however, **hangs at `OMP_NUM_THREADS=8`** on the
default input (1 and 2 threads complete fine). This is **pre-existing and unrelated
to 4.2** — the stock model (with the CO2SYS change stashed) hangs identically at 8
threads, and the micro-benchmark that exercises the exact kinetics+CO2SYS path does
*not* hang. The problem is therefore in the ESTAS-specific serial/transport/solver
path (e.g. `mod_SOLVER.f90`'s separate `!$omp` region or the box-network transport),
not the kinetics. It is why performance work uses the micro-benchmark. **Tracked as
a new backlog item** (investigate/fix the full-model OpenMP hang before recommending
`ESTAS_II` at ≥8 threads for production).

## Recommendations

| Network size | Guidance |
|---|---|
| `nkn ≲ 100` (default 25-box, CL29 29-box) | Leave OpenMP off; <2.5× even at 8 threads, and the full-model hang makes it moot. |
| `nkn ≈ 500` | OpenMP worthwhile: ~3.3× at 4 threads, ~4.5× at 8 (via the kinetics path). |
| `nkn ≳ 1000` | Enable OpenMP: 4.4× at 4 threads, 6.55× at 8. Best efficiency at 2–4 threads. |
| any | Do not exceed physical cores (14 here); and until the full-model hang is fixed, cap `ESTAS_II` at ≤2 threads (the micro-benchmark path is unaffected). |

## Thread affinity (TODO 4.3)

Recommended for cache/NUMA locality: `export OMP_PROC_BIND=close; export
OMP_PLACES=cores`. **Measured effect on the single-socket test machine: none** (at
`nkn=1000`/8 threads, default `3287,3291,3249` vs bound `3317,3388,3327` µs/step —
within noise, marginally worse). Expected: no NUMA on a single socket, so binding
only removes the scheduler's freedom. Keep the settings documented for
**multi-socket / NUMA** hardware, and re-measure on the target deployment machine.

## Reproducing

```bash
make benchmark-openmp                 # full sweep (~1 min): threads {1,2,4,8} x nkn {100,500,1000} + affinity
tools/benchmark_openmp.sh --quick     # fast smoke run
```

Raw results + the speedup/efficiency table are written to
`tools/benchmark_openmp_results.txt`.
