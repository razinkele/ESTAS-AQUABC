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
- **Fixed (TODO 4.4):** a **pre-existing empty-chunk barrier deadlock** hung the full
  model whenever `nthreads` did not evenly divide `nkn` (e.g. `nkn=25`/8 threads left
  the last thread with 0 nodes, which skipped the region's collective `!$omp barrier`s
  and deadlocked the team). Fixed by a **balanced chunk split** + capping the team to
  `min(nkn, threads)` so every thread gets ≥1 node — `ESTAS_II` now scales to 8 threads.

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

## Full-model empty-chunk barrier deadlock (TODO 4.4 — fixed)

The full `ESTAS_II` executable used to **hang whenever `nthreads` did not evenly
divide `nkn`** — e.g. the default 25-box network on 8 threads (`nkn=25` → the old
`chunk = ceil(25/8) = 4` scheme covered nodes 1–25 with 7 threads and left **thread 7
with an empty chunk**, `nkn_local = -3`). A thread-by-thread checkpoint trace showed
all 8 threads enter the kinetics `!$omp parallel` region, but the empty-chunk thread
never reaches the region's first collective `!$omp barrier`, so the other 7 wait for
it forever (an active-spin deadlock — hence high CPU, no progress). It only bit when
`nkn < nthreads` or `nkn` was not a multiple of `nthreads`, which is why `nkn=1000`/8
(all threads busy) worked while `nkn=25`/8 hung. It was **pre-existing** (the Phase-4
OpenMP work; independent of the 4.2 CO2SYS change — the stock code hung identically),
and reproduced in the micro-benchmark at `nkn=25`/8.

**Fix:** replace the `ceil`-based split with a **balanced split** (`base = nkn /
nthreads`, the first `mod(nkn, nthreads)` threads get one extra node) and cap the team
with `num_threads(min(nkn, omp_get_max_threads()))`, so **every thread always gets ≥1
node** — no empty chunk, no missed barrier. Applied to both the kinetics and the
CO2SYS regions (`aquabc_II_pelagic_model.f90`). Verified: `ESTAS_II` completes at 8
threads (default input); 0D golden bit-identical; the benchmark speedup is unchanged
(6.4× at nkn=1000/8); @1-vs-@8 output drift ≤1e-6 absolute (output-precision floor).

## Recommendations

| Network size | Guidance |
|---|---|
| `nkn ≲ 100` (default 25-box, CL29 29-box) | Little benefit (<2.5× even at 8 threads) — the parallel work per node is too small. |
| `nkn ≈ 500` | OpenMP worthwhile: ~3.3× at 4 threads, ~4.5× at 8. |
| `nkn ≳ 1000` | Enable OpenMP: 4.4× at 4 threads, 6.55× at 8. Best efficiency at 2–4 threads. |
| any | Do not exceed physical cores (14 here). `ESTAS_II` now scales correctly to 8 threads (the empty-chunk hang was fixed — TODO 4.4). |

## Thread affinity (TODO 4.3)

Recommended for cache/NUMA locality: `export OMP_PROC_BIND=close; export
OMP_PLACES=cores`. **Measured effect on the single-socket test machine: none** (at
`nkn=1000`/8 threads, default `3287,3291,3249` vs bound `3317,3388,3327` µs/step —
within noise, marginally worse). Expected: no NUMA on a single socket, so binding
only removes the scheduler's freedom. Keep the settings documented for
**multi-socket / NUMA** hardware, and re-measure on the target deployment machine.

## Cross-compiler comparison — gfortran vs ifx (2026-07-21)

Same kinetics micro-benchmark, same 28-core host, run with both toolchains at their optimized
release+OpenMP flags: **gfortran 13.3.0** (driver `-fopenmp -O3 -march=native`) vs **ifx 2026.1.0**
(driver `-qopenmp -O3 -xHost`). Reproduce with `THREADS="1 2 4 8 16" FC=<compiler> tools/benchmark_openmp.sh`.

**Single-thread throughput (µs/step) — pure code generation:**

| nkn | gfortran | ifx | ifx faster |
|---|---:|---:|---:|
| 100 | 555 | 417 | 1.33× |
| 500 | 3633 | 2021 | 1.80× |
| 1000 | 9639 | 4923 | **1.96×** |

**ifx generates up to ~2× faster serial code** — its auto-vectorization of the per-node kinetics
loop is markedly better, and the advantage grows with the workload.

**Strong scaling, `nkn=1000` (speedup vs each compiler's own 1-thread):**

| threads | gfortran | ifx |
|---|---:|---:|
| 1 | 1.00× | 1.00× |
| 2 | 2.38× | 1.91× |
| 4 | 4.47× | 2.93× |
| 8 | **7.03×** | 3.71× |
| 16 | 6.65× | **3.83×** |

gfortran scales *further* (7× vs 3.7×) — but only because its slower serial baseline leaves more
headroom; ifx is already ~2× ahead at 1 thread. The two converge in **absolute** time: at 8
threads both sit near 1350–1370 µs/step. gfortran plateaus/regresses past 8 threads (memory-bound),
while ifx keeps a slight gain to 16.

**Best absolute (nkn=1000):** ifx **1284 µs/step @16t** (fastest overall) vs gfortran 1371 @8t — a
~6 % edge to ifx, at **~2× the compile time** (release+OpenMP library: gfortran ~32 s, ifx ~60 s).

**Takeaways.** For serial or few-core runs, ifx's codegen is a clear win (~2×). With ≥8 cores the
gap closes and gfortran+OpenMP matches ifx in absolute throughput. ifx is marginally fastest at high
thread counts but costs ~2× the build. (Building the library with the stricter local ifx 2026.1.0
also surfaced a latent `intent(in)` vs `inout` mismatch on the FIX_CYN light-saturation argument —
fixed in the same change, matching the DIA/CYN/OPA declarations, so the ifx library builds cleanly.)

## Reproducing

```bash
make benchmark-openmp                 # full sweep (~1 min): threads {1,2,4,8} x nkn {100,500,1000} + affinity
THREADS="1 2 4 8 16" FC=ifx tools/benchmark_openmp.sh   # cross-compiler / more threads
tools/benchmark_openmp.sh --quick     # fast smoke run
```

Raw results + the speedup/efficiency table are written to
`tools/benchmark_openmp_results.txt`.
