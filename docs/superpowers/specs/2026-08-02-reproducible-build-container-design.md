# Reproducible-build container — design

**Motivation:** `BACKLOG.md` §2 (Calibration & reproducibility) — "containerized build." Completes the
reproducibility trio (constants file done; fail-loud parked; this).
**Date:** 2026-08-02
**Status:** Design — pending user review, then implementation plan.
**Scope tier:** A lean build + run + validate container (Dockerfile + `.dockerignore` + a run wrapper +
docs). Excludes the Shiny front end.

---

## 1. Problem & goal

AQUABC has **no containerization** today, and its default build is **not portable**: the release
`FFLAGS` are `-O2 -march=native -mtune=native` (verified via `make show-config`), so the binary is
compiled for the *host* CPU and floating-point results can differ across machines. "Reproducible build"
therefore needs two things: a **pinned toolchain** (a fixed base image + compiler) and a **fixed
`-march`** (not `native`).

**Goal:** a container in which anyone can, deterministically, (a) build `libaquabc.a` + `ESTAS_II`, (b)
run and self-verify the committed 0D example, and (c) — by mounting the external CL29 inputs + obs — run
CL29 and reproduce the EPA/KM validation. Lean: the Fortran toolchain + Python **numpy** (the only
non-stdlib dep of `tools/validate_cl29_vs_epa.py`); no Shiny front end.

## 2. What reproduces out-of-the-box vs. needs mounted data

| Artifact | In the image? | Reproducible how |
|---|---|---|
| `libaquabc.a` + `ESTAS_II` build | source is in the repo | **fully** — pinned toolchain + fixed `-march` |
| 0D example run | data committed (`AQUABC_PELAGIC_0D/data/`, 6 files) | **fully** — deterministic; checked vs the committed golden |
| CL29 run | `INPUTS_CL29/` is **gitignored** (converter-generated from EUTROPY) | mount `INPUTS_CL29/` (or the EUTROPY data + run the converter) |
| EPA validation | `epa_observations_out/` is **untracked** | mount the obs; `tools/validate_cl29_vs_epa.py` (numpy-only) is in the image |
| KM validation | `pest/km_observations_tidy.csv` **is committed** | works once CL29 has run |

Honest framing: the container guarantees the **build** and a **deterministic model run (0D)** reproduce
anywhere; full **CL29 + EPA-validation** reproduction is one `-v` mount away, matching how the repo
actually ships its (external) application data.

## 3. Reproducible build flags — verified

Build with the arch pinned by **overriding the `GF_NATIVE_FLAGS` make variable** (it is `$(...)`-expanded
into the release `FFLAGS`, so a command-line override wins):

```sh
make GF_NATIVE_FLAGS="-march=x86-64-v2 -mtune=generic" build-estas
```

- `x86-64-v2` is a portable ~2009+ baseline → the image is bit-reproducible on any modern x86-64 host.
- **Verified (2026-08-02):** the override changes `FFLAGS` to `-O2 -march=x86-64-v2 -mtune=generic …`,
  the build succeeds, and the pinned-arch 0D run **still passes the committed golden** at `rtol=1e-6`
  (`compare_0D.py`). ⟹ **no new golden is needed** — the existing regression is the container self-test.
- Release (`-O2`, IEEE-safe) is used, never `fast` (`-ffast-math` breaks IEEE 754 — Makefile's own
  warning). Reproducibility scope is **x86-64**; ARM would use a different baseline (out of scope, noted).

## 4. Architecture

**`Dockerfile`** (multi-stage optional; single-stage is fine for this size):
- **Base:** `ubuntu:24.04` (matches CI's `ubuntu-latest`), pinned by digest for stability.
- **Toolchain:** `apt-get install --no-install-recommends gfortran-13 make binutils coreutils python3
  python3-numpy` (pin `gfortran-13`, the 24.04 default, for a stable compiler; `python3-numpy` from apt
  avoids a pip toolchain). Record versions in the image (a `gfortran --version` layer).
- **Source:** `COPY` the repo in (respecting `.dockerignore`).
- **Build:** `make link-data && make GF_NATIVE_FLAGS="-march=x86-64-v2 -mtune=generic" build-estas`.
- **Self-test (build-time, fails the image build if broken):**
  `make -C SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D run` then
  `python3 tests/regression/compare_0D.py … --rtol 1e-6 --atol 1e-9`.
- **Entrypoint:** a small `docker/entrypoint.sh` — default action prints the build/version banner and
  runs the 0D self-test; accepts a passthrough command (e.g. run CL29 against a mounted `INPUTS_CL29/`).

**`.dockerignore`** — exclude `OUTPUTS_*/`, `.git`, the scratch/untracked artifacts, existing binaries,
`INPUTS_CL29*/` (mounted, not baked), so the build context is small and hermetic.

**Run wrapper** `docker/run.sh` (optional convenience): `docker build` + `docker run` with the right
`-v` mounts for CL29 + obs, documented.

## 5. Files to change / add

1. `Dockerfile` (repo root).
2. `.dockerignore` (repo root).
3. `docker/entrypoint.sh` (+ optional `docker/run.sh`).
4. **Docs:** a "Reproducible container" section in `README.md` (build/run/mount commands) and, per the
   reproducibility theme, a short reference in the docs table (or fold into an existing reproducibility
   doc). Cross-link `docs/CL29_Calibration_PEST_Workflow.md`.
5. **CI (recommended):** a lightweight `docker build` job in `.github/workflows/ci.yml` (or a dedicated
   workflow) that builds the image and lets its build-time 0D self-test run — so the container can't
   bit-rot. Decision point (§8): include now vs. defer.

## 6. Testing

- **Image builds** from a clean context (CI or local).
- **0D self-test passes inside the image** (the build-time `compare_0D.py` step — the image build fails
  if it doesn't).
- **Reproducibility check:** build the image twice (or run the 0D self-test on two different x86-64
  hosts) → identical `OUTPUT.csv`. This is the actual "bit-reproducible" proof, beyond the tolerant
  golden compare.
- **Mounted-data smoke (documented, not automated):** `docker run -v $PWD/INPUTS_CL29:/app/INPUTS_CL29
  … run_cl29.sh` completes and `validate_cl29_vs_epa.py` scores — proving the mount path works.

## 7. Risks

- **Reproducibility is x86-64-scoped** (the pinned baseline). ARM/Apple-Silicon would need a different
  `-march` and its own golden — explicitly out of scope, stated in the docs.
- **Base-image / apt drift:** `ubuntu:24.04` + `apt` can still pull newer package point-releases over
  time. Pinning the base by **digest** and `gfortran-13` mitigates; full apt-version pinning is possible
  but heavier — note the residual limitation honestly (the fixed `-march` + digest gets ~99% of the way).
- **Data is external** (`INPUTS_CL29/` gitignored, EPA obs untracked) — the container cannot bake the CL29
  application data; the mount path is the honest contract (§2).
- **Image size / CI time** if CI builds it every push — mitigate with layer caching or a scheduled/tag
  trigger rather than every-push.

## 8. Decisions log

- **Scope:** build + run(0D) + validate(numpy); **no** Shiny front end.
- **Arch:** pinned `-march=x86-64-v2 -mtune=generic` (portable, verified passes the golden) — never
  `-march=native` (host-specific) and never `fast`/`-ffast-math` (IEEE-unsafe).
- **Golden:** reuse the committed 0D golden as the self-test (verified tolerant to the arch change) — no
  new golden.
- **Data:** build + 0D out-of-box; CL29 + validation via `-v` mounted data (external by design).
- **Open decision (for the plan / user):** wire a `docker build` job into CI now (prevents rot, adds CI
  time) vs. ship the Dockerfile documented-but-not-CI-built.
