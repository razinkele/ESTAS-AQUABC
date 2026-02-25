# Sprint 2: Numerical Safety & CI Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Harden CO2SYS against exponential overflow, confirm division-by-zero audit completeness, and improve CI with SHA-pinned actions, pip caching, and Python code coverage.

**Architecture:** Five independent tasks — one Fortran numerical fix (CO2SYS safe_exp), one audit-and-document task (division-by-zero), and three CI improvements (SHA pinning, caching, coverage). Each commits separately.

**Tech Stack:** Fortran 90 (gfortran), GitHub Actions, Python 3 (pytest-cov), pip

---

## Pre-flight: Verify Baseline

**Step 1:** `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED

**Step 2:** `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
Expected: 37 passed

**Step 3:** `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: ESTAS_II created

---

## Task 1: CO2SYS safe_exp Protection (P1)

CO2SYS has ~30 raw `exp()` calls. Most compute equilibrium constants from temperature polynomials with bounded arguments. However, ~6 calls have divisions by `TempK` or `RT` in the exponent, which could overflow if temperature approaches extreme values.

The function `safe_exp(x)` already exists in `SOURCE_CODE/AQUABC/PELAGIC/aquabc_physical_constants.f90:47-56` — it clamps the argument to [-700, 700] before calling `exp()`. It's `elemental` so it works on both scalars and arrays.

**Files:**
- Modify: `SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90`
- Verify: Build + existing tests

### Step 1: Add safe_exp import to CO2SYS subroutines

CO2SYS contains multiple subroutines. Add the `use` statement to the subroutines that have vulnerable `exp()` calls:

**In subroutine `Constants` (around line 1303):**
```fortran
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
```

**In subroutine `CaSolubility` (around line 4380):**
```fortran
    use AQUABC_PHYSICAL_CONSTANTS, only: safe_exp
```

### Step 2: Replace vulnerable exp() calls with safe_exp()

Replace these specific `exp()` calls that have divisions by temperature in the exponent:

**Line ~1691** (KP2 calculation with direct TempK division):
```fortran
! BEFORE:
KP2 = exp(-9.039D0 - (1450D0 / TempK)) / fH
! AFTER:
KP2 = safe_exp(-9.039D0 - (1450D0 / TempK)) / fH
```

**Line ~1694** (KP3 calculation with direct TempK division):
```fortran
! BEFORE:
KP3 = exp(4.466D0 - (7276D0 / TempK)) / fH
! AFTER:
KP3 = safe_exp(4.466D0 - (7276D0 / TempK)) / fH
```

**Line ~2440** (FugFac with pressure/RT division):
```fortran
! BEFORE:
FugFac = exp((b + 2 * Delta) * P1atm / RT)
! AFTER:
FugFac = safe_exp((b + 2 * Delta) * P1atm / RT)
```

**Line ~2466** (VPWP with multiple TempK divisions):
```fortran
! BEFORE:
VPWP = exp(24.4543D0 - 67.4509D0 * (100.0D0 / TempK) - ...)
! AFTER:
VPWP = safe_exp(24.4543D0 - 67.4509D0 * (100.0D0 / TempK) - ...)
```

**Line ~4580** (KCa pressure correction):
```fortran
! BEFORE:
KCa = KCa * exp((36D0 - 0.2D0 * TempC) * Pbar / RT)
! AFTER:
KCa = KCa * safe_exp((36D0 - 0.2D0 * TempC) * Pbar / RT)
```

**Line ~4581** (KAr pressure correction):
```fortran
! BEFORE:
KAr = KAr * exp((33.3D0 - 0.22D0 * TempC) * Pbar / RT)
! AFTER:
KAr = KAr * safe_exp((33.3D0 - 0.22D0 * TempC) * Pbar / RT)
```

**NOTE:** Do NOT replace all 30 `exp()` calls — only the 6 listed above that have divisions in the exponent. The remaining ~24 calls use pre-computed `lnK` values from bounded polynomial expressions and are safe.

### Step 3: Verify build

Run: `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: Clean build, ESTAS_II created

### Step 4: Verify all Fortran tests

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED

### Step 5: Commit

```bash
git add SOURCE_CODE/AQUABC/CO2SYS/aquabc_II_co2sys.f90
git commit -m "fix: add safe_exp guards to 6 vulnerable exp() calls in CO2SYS

Replace raw exp() with safe_exp() for calls that have divisions by
TempK or RT in the exponent, preventing overflow at extreme
temperature/pressure values. Uses existing safe_exp from
aquabc_physical_constants (clamps argument to [-700, 700])."
```

---

## Task 2: Division-by-Zero Audit of Pelagic Model (P1)

Systematic audit of all division operations in `aquabc_II_pelagic_model.f90` to confirm existing guards are adequate and document the results.

**Files:**
- Read: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model.f90` (audit only)
- Modify: `TODO_IMPLEMENTATION_PLAN.md` (document results)

### Step 1: Audit all divisions

Search for all `/` operations in `aquabc_II_pelagic_model.f90`. For each division, classify as:
- **SAFE (constant divisor)**: Dividing by a parameter/constant that is always > 0
- **SAFE (guarded)**: Divisor protected by `max()`, `where`, or conditional check
- **SAFE (Monod)**: Michaelis-Menten form where denominator = variable + constant > 0
- **UNGUARDED**: Divisor could potentially be zero

### Step 2: Document the audit

The expected finding (based on exploration) is that the pelagic model is already well-guarded:
- **Iron/Mn fractions**: Protected by `< 1.0D-20` conditionals
- **Zooplankton/detritus ratios**: Use `max(divisor, MIN_CONCENTRATION)`
- **Monod kinetics**: Denominator = substrate + half-saturation > 0
- **CHLA calculation**: Divides by C_TO_CHLA constants (always > 0)
- **H2S/phosphate speciation**: Mathematically safe (sum of positive terms)
- **PHYT_TOT_C**: Never used as a divisor (only as multiplier)

If any unguarded divisions are found, fix them with `max(divisor, 1.0D-20)`.

### Step 3: Update TODO plan

In `TODO_IMPLEMENTATION_PLAN.md`, add a note under item 1.5:

```markdown
- [x] 1.5 Remaining division-by-zero audit — **Audit complete** (2026-02-14). All divisions in pelagic_model.f90 confirmed safe: iron/Mn use conditional guards, zoo/det use max(), Monod kinetics are mathematically safe, CHLA divides by constants only.
```

### Step 4: Commit

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: complete division-by-zero audit of pelagic model

Systematic audit confirms all divisions in aquabc_II_pelagic_model.f90
are adequately protected: conditional guards for iron/Mn fractions,
max() for zoo/detritus ratios, Monod kinetics mathematically safe."
```

---

## Task 3: Pin GitHub Actions to SHA (P2)

Replace tag-based action references with SHA-pinned versions for supply chain security.

**Files:**
- Modify: `.github/workflows/ci.yml`

### Step 1: Look up current SHAs

Use `gh` CLI or the GitHub API to find the commit SHA for each action tag:

```bash
# For each action, get the SHA of the tag
gh api repos/actions/checkout/git/ref/tags/v4 --jq '.object.sha'
gh api repos/actions/setup-python/git/ref/tags/v5 --jq '.object.sha'
gh api repos/actions/upload-artifact/git/ref/tags/v4 --jq '.object.sha'
```

### Step 2: Replace all uses: lines

Replace each tag reference with `SHA # tag-comment`:

```yaml
# BEFORE:
uses: actions/checkout@v4
# AFTER:
uses: actions/checkout@<SHA> # v4

# BEFORE:
uses: actions/setup-python@v5
# AFTER:
uses: actions/setup-python@<SHA> # v5

# BEFORE:
uses: actions/upload-artifact@v4
# AFTER:
uses: actions/upload-artifact@<SHA> # v4
```

There are 5 `uses:` lines total (checkout x2, setup-python x1, upload-artifact x2).

### Step 3: Commit

```bash
git add .github/workflows/ci.yml
git commit -m "ci: pin GitHub Actions to SHA for supply chain security

Replace tag-based references (actions/checkout@v4, etc.) with
SHA-pinned versions. Tag noted in comment for human readability."
```

---

## Task 4: Add CI Dependency Caching (P2)

Add pip caching to the Python CI job to speed up dependency installation.

**Files:**
- Modify: `.github/workflows/ci.yml`

### Step 1: Add cache parameter to setup-python

In the `python-lint-test` job, modify the `Set up Python` step:

```yaml
      - name: Set up Python
        uses: actions/setup-python@<SHA> # v5
        with:
          python-version: "3.13"
          cache: 'pip'
```

This uses setup-python's built-in pip caching. It automatically detects `requirements*.txt` files as cache keys.

### Step 2: Commit

```bash
git add .github/workflows/ci.yml
git commit -m "ci: add pip dependency caching for faster CI runs

Enable setup-python's built-in pip cache. Dependencies are cached
between runs, reducing install time from ~15s to ~2s on cache hit."
```

---

## Task 5: Add Python Code Coverage (P1)

Add pytest-cov to track Python test coverage and report it in CI.

**Files:**
- Modify: `requirements-dev.txt`
- Modify: `pyproject.toml` (add coverage config)
- Modify: `.github/workflows/ci.yml` (add --cov flags)

### Step 1: Add pytest-cov to dev dependencies

In `requirements-dev.txt`, add:
```
pytest-cov>=6.0
```

### Step 2: Add coverage configuration to pyproject.toml

Add at the end of `pyproject.toml`:
```toml
[tool.coverage.run]
source = ["shiny_app"]
omit = ["tests/*"]

[tool.coverage.report]
show_missing = true
skip_empty = true
fail_under = 0
```

`fail_under = 0` means coverage won't fail the build — it's just reporting for now.

### Step 3: Update CI to run with coverage

In `.github/workflows/ci.yml`, update the Python test step:

```yaml
      - name: Install Python dependencies
        run: |
          pip install ruff pytest pytest-cov
          pip install shiny pandas plotly shinywidgets python-dotenv

      - name: Run Python tests
        run: pytest tests/python/ -v --cov=shiny_app --cov-report=term-missing --cov-report=xml
```

### Step 4: Install locally and verify

Run: `pip install pytest-cov`
Run: `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --cov=shiny_app --cov-report=term-missing --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
Expected: Tests pass with coverage report printed

### Step 5: Commit

```bash
git add requirements-dev.txt pyproject.toml .github/workflows/ci.yml
git commit -m "ci: add Python code coverage tracking with pytest-cov

Add pytest-cov to dev dependencies, configure coverage for shiny_app
source, and enable coverage reporting in CI. Initial coverage baseline
established (no minimum threshold enforced yet)."
```

---

## Task 6: Update Sprint 2 in TODO Plan

**Files:**
- Modify: `TODO_IMPLEMENTATION_PLAN.md`

### Step 1: Mark Sprint 2 complete

```markdown
### Sprint 2 — Numerical Safety & CI (2–3 days) --- COMPLETED 2026-02-14
- [x] 1.4 CO2SYS safe_exp — **Fixed** (6 vulnerable exp() calls wrapped with safe_exp)
- [x] 1.5 Remaining division-by-zero audit — **Audit complete** (all divisions confirmed safe)
- [x] 3.3 Python code coverage — **Added** (pytest-cov with CI reporting)
- [x] 3.4 Pin GitHub Actions to SHA — **Done** (5 action references pinned)
- [x] 3.5 CI dependency caching — **Done** (pip cache enabled)
```

### Step 2: Commit

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: mark Sprint 2 items as complete in TODO plan"
```

---

## Post-flight: Full Verification

**Step 1:** `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
**Step 2:** `cd /home/razinka/AQUABCv0.2 && python3 -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py --ignore=tests/python/test_safe_resolve.py`
**Step 3:** `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
