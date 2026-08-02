# Reproducible AQUABC build + run + validation environment.
# Design: docs/superpowers/specs/2026-08-02-reproducible-build-container-design.md
#
# Reproducibility is scoped to x86-64: the library/ESTAS build pins a fixed -march
# (NOT -march=native, which is host-specific; NOT the `fast`/-ffast-math build, which
# breaks IEEE 754). The 0D example itself builds with the Makefile's portable -O.
#
#   docker build -t aquabc .                 # builds + runs the 0D self-test (fails if it diverges)
#   docker run --rm aquabc                   # prints the banner + re-runs the 0D self-test
#   docker run --rm \                        # reproduce CL29 (mount the external, gitignored data):
#     -v "$PWD/INPUTS_CL29:/app/INPUTS_CL29" aquabc run_cl29.sh
#   docker run --rm \                        # + reproduce the EPA validation (mount the obs):
#     -v "$PWD/INPUTS_CL29:/app/INPUTS_CL29" \
#     -v "$PWD/epa_observations_out:/app/epa_observations_out" aquabc \
#     python3 tools/validate_cl29_vs_epa.py --outputs OUTPUTS_CL29 --obs epa_observations_out/epa_observations_tidy.csv
FROM ubuntu:24.04

# Pinned toolchain: gfortran-13 (the Ubuntu 24.04 default) + make + binutils (ar) + coreutils (ln),
# and python3 + numpy — numpy is the only non-stdlib dependency of tools/validate_cl29_vs_epa.py.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      gfortran-13 make binutils coreutils python3 python3-numpy ca-certificates \
 && ln -sf /usr/bin/gfortran-13 /usr/local/bin/gfortran \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Fixed architecture baseline for cross-machine (x86-64) bit-reproducibility.
ARG ARCHFLAGS="-march=x86-64-v2 -mtune=generic"

# Build libaquabc.a + ESTAS_II with the pinned arch, then run the committed 0D example and verify it
# against the golden — this fails the image build if the toolchain produces divergent numerics.
RUN gfortran --version | head -1 \
 && make link-data \
 && make GF_NATIVE_FLAGS="$ARCHFLAGS" build-estas \
 && make -C SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D run \
 && python3 tests/regression/compare_0D.py \
      SOURCE_CODE/AQUABC/AQUABC_EXAMPLES/AQUABC_PELAGIC_0D/OUTPUT.csv \
      tests/regression/pelagic_0D_golden.csv --rtol 1e-6 --atol 1e-9 \
 && chmod +x docker/entrypoint.sh

ENTRYPOINT ["/app/docker/entrypoint.sh"]
CMD ["selftest"]
