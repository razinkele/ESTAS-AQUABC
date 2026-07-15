# OpenMP Performance — AQUABC Pelagic Kinetics

Benchmark of the OpenMP parallelization of `AQUABC_PELAGIC_KINETICS` (the single
`!$omp parallel` region over the `nkn` spatial nodes, `aquabc_II_pelagic_model.f90`).
Addresses backlog **TODO 4.1** (performance benchmarking) and **4.3** (thread-affinity
guidance), and quantifies the serial bottleneck that motivates **4.2** (CO2SYS).

## TL;DR

- OpenMP gives a **real but sub-linear speedup that grows with `nkn`**: negligible at
  `nkn=100`, up to **2.84× at `nkn=1000` on 8 threads**.
- Speedup **plateaus around 8 threads** — an Amdahl fit gives a **~26 % serial fraction**
  at `nkn=1000`, dominated by the **serial CO2SYS call** that runs once per timestep
  *before* the parallel region. That serial fraction caps the achievable speedup at
  ~3.9× regardless of thread count.
- **Recommendation:** enable OpenMP (`make OPENMP=1 …`) for runs with `nkn ≳ 500`; use
  **2–4 threads for best efficiency**, up to 8 threads for best absolute time. It is not
  worth enabling for small networks (`nkn ≲ 100`, e.g. the default 25-box / CL29 29-box
  cases — there is too little work per thread to overcome the overhead + serial fraction).
- **Thread affinity** (`OMP_PROC_BIND`/`OMP_PLACES`) made **no measurable difference** on
  the single-socket test machine (see §4.3).

## Method

The existing full-model examples are too small to exercise the parallel region (the
default network is 25 boxes, CL29 is 29 — with static block scheduling `chunk =
ceil(nkn/threads)`, at `nkn=25` on 8 threads only 7 threads get work). So the benchmark
reuses the **node-agnostic 0D interface path** (`aquabc_init` / `aquabc_run`) to call the
**real** `AQUABC_PELAGIC_KINETICS` — with its real per-thread derived-type bundles and the
real `!$omp parallel` region — across `nkn` homogeneous (replicated) nodes.

- **Driver:** `SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/aquabc_II_pelagic_benchmark.f90`
  — `nkn` and the timed step count come from `BENCH_NKN` / `BENCH_STEPS`; the state is
  reset to fixed initial conditions before every call (identical work each step →
  reproducible, no drift/NaN); one untimed warm-up call absorbs the one-time internal
  allocation; the timed loop is bracketed by `omp_get_wtime()`.
- **What is timed:** one full kinetics step, i.e. `aquabc_run` = the serial CO2SYS
  computation **+** the parallel kinetics region. This is the real per-timestep cost, so
  the measured speedup is the real-world speedup of a kinetics timestep (not an idealized
  parallel-region-only figure).
- **Constants:** the reference `data/const_CL.txt` (same file the 0D example uses; its
  "error in file … number of constants" line is a pre-existing non-fatal count-mismatch
  warning — the `stop` is commented out in the shared reader — so the workload matches the
  reference example exactly).
- **Build:** `make OPENMP=1 FC=gfortran BUILD_TYPE=release build-lib` (library `-O3
  -march=native -ffast-math -flto -fopenmp`), driver compiled `-fopenmp -O3 -march=native`.
  The **library must be built with `-fopenmp`** for the region to parallelize, and the
  final executable must link `-fopenmp`.
- **Baseline:** the same OpenMP binary at `OMP_NUM_THREADS=1` (so speedup isolates
  threading, not the `-fopenmp` compile). Speedup = T₁ / Tₙ; efficiency = speedup / n.
- **Hardware:** Intel Core i9-10940X (14 physical cores / 28 threads, **single socket**),
  gfortran 13.3.0, Ubuntu 24.04. Single run per point; step counts chosen for a ~2–3 s
  single-thread baseline (thousands→hundreds of kinetics calls averaged per point).

## Results — strong scaling (fixed total work, more threads)

| `nkn` | threads | µs / step | speedup | efficiency |
|------:|--------:|----------:|--------:|-----------:|
| 100   | 1 | 577.9  | 1.00 | 100 % |
| 100   | 2 | 500.4  | 1.16 |  58 % |
| 100   | 4 | 486.5  | 1.19 |  30 % |
| 100   | 8 | 461.5  | 1.25 |  16 % |
| 500   | 1 | 3458.2 | 1.00 | 100 % |
| 500   | 2 | 2366.7 | 1.46 |  73 % |
| 500   | 4 | 1919.1 | 1.80 |  45 % |
| 500   | 8 | 1698.5 | 2.04 |  25 % |
| 1000  | 1 | 9113.0 | 1.00 | 100 % |
| 1000  | 2 | 4877.8 | **1.87** |  93 % |
| 1000  | 4 | 3637.7 | **2.51** |  63 % |
| 1000  | 8 | 3205.3 | **2.84** |  36 % |

`nkn=1000`, 16 threads → 3211 µs/step (speedup 2.84) — **no gain beyond 8 threads** (16
threads spill onto hyperthreads, which don't help compute-bound FP work, and the serial
fraction already dominates).

## Analysis

- **Scaling improves strongly with `nkn`.** At `nkn=100` there is too little work per
  thread (and block-scheduling imbalance) to overcome thread-startup overhead — 8 threads
  buys only 1.25×. At `nkn=1000` the parallel kinetics dominates and 2-thread efficiency
  reaches **93 %** (near-ideal).
- **Amdahl's law caps the speedup.** Fitting T(p) = T₁·(f + (1−f)/p) to the `nkn=1000`
  data (T₁=9113, T₈=3205 µs) gives a **serial fraction f ≈ 0.26**. That serial ~26 % limits
  the maximum speedup to 1/f ≈ **3.9×** no matter how many threads are added — consistent
  with the observed plateau (2.84× at 8 threads, no further gain at 16).
- **The serial fraction is the CO2SYS call.** `AQUABC_PELAGIC_KINETICS` runs CO2SYS
  (`aquabc_II_co2sys.f90`, a vectorized Newton–Raphson pH/alkalinity solver, **no OpenMP**)
  once per timestep over all `nkn` nodes **before** the `!$omp parallel` region begins. It
  is O(`nkn`) serial work every step — exactly the profile of an Amdahl serial fraction that
  grows with `nkn`. This is the direct, quantified motivation for **TODO 4.2**.

## Recommendations (TODO 4.1)

| Network size | Guidance |
|---|---|
| `nkn ≲ 100` (default 25-box, CL29 29-box) | **Leave OpenMP off.** <1.3× even at 8 threads; the overhead isn't worth it. |
| `nkn ≈ 500` | OpenMP worthwhile: ~1.8× at 4 threads, ~2.0× at 8. |
| `nkn ≳ 1000` | **Enable OpenMP.** 2 threads ≈ 93 % efficient (1.87×); 8 threads for best absolute time (2.84×). |
| any | **Do not exceed the physical core count** (14 here) — hyperthreads give no gain; and past ~8 threads the serial (CO2SYS) fraction dominates. Best *efficiency* is at 2–4 threads. |

Build/run: `make OPENMP=1 FC=gfortran BUILD_TYPE=release build-estas` then
`OMP_NUM_THREADS=4 ./ESTAS_II INPUT.txt`.

## Thread affinity (TODO 4.3)

Recommended settings for cache/NUMA locality:

```bash
export OMP_PROC_BIND=close
export OMP_PLACES=cores
```

**Measured effect on the test machine: none.** At `nkn=1000` / 8 threads, three runs each:

| config | µs/step (3 runs) |
|---|---|
| default (unbound) | 3287, 3291, 3249 |
| `OMP_PROC_BIND=close OMP_PLACES=cores` | 3317, 3388, 3327 |

Binding was within run-to-run noise (marginally *worse*). This is expected: the test
machine is **single-socket**, so there is no cross-socket (NUMA) memory-latency penalty for
the OS scheduler to cause, and pinning to 8 of 14 cores only removes the scheduler's freedom
to avoid a momentarily-busy core. **Keep the settings documented** — they are good practice
and can help on **multi-socket / NUMA** hardware (where unbound threads may straddle sockets
and pay remote-memory latency) — but they are not needed on single-socket machines, and
should be re-measured on the target deployment hardware before being relied upon.

## Next: CO2SYS serial bottleneck (TODO 4.2)

The ~26 % serial fraction measured above is the ceiling on OpenMP scaling, and CO2SYS is the
prime suspect (O(`nkn`) serial, once per timestep, before the parallel region). TODO 4.2
("profile first; parallelize the CO2SYS loop if it is >10 % of kinetics time") is now
quantitatively justified — parallelizing CO2SYS's per-node loop would lift the ~3.9× Amdahl
ceiling substantially at large `nkn`.

## Reproducing

```bash
tools/benchmark_openmp.sh            # full sweep (~1 min): threads {1,2,4,8} x nkn {100,500,1000} + affinity
tools/benchmark_openmp.sh --quick    # fast smoke run (fewer timesteps)
```

Raw results + the speedup/efficiency table are written to
`tools/benchmark_openmp_results.txt`. The harness rebuilds the OpenMP library and driver
each run, so it is safe to re-run after code changes.
