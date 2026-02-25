# Sprint 1: Critical Fixes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix division-by-zero risks in K_E computations, add thread-safety documentation for SAVE variables, and replace bare except blocks in the Shiny app.

**Architecture:** Three independent fix tracks — Fortran numerical safety (guard K_E divisions in 3 library files), Fortran thread-safety audit (document assumptions on 25 SAVE variables across 2 files), and Python exception handling (replace 5 bare excepts with specific types). Each track has its own tests and commit.

**Tech Stack:** Fortran 90 (gfortran), Python 3.11 (Shiny app), pytest, custom Fortran test harness

---

## Pre-flight: Verify Baseline

Before any changes, confirm all existing tests pass.

**Step 1: Run Fortran tests**

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: "All tests PASSED"

**Step 2: Run Python tests**

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py`
Expected: All tests pass

**Step 3: Verify Fortran library builds**

Run: `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: Clean build with zero errors

---

## Audit Correction: ALLELOPATHY Memory Leaks (Item 1.1)

The TODO plan listed `aquabc_II_pelagic_lib_ALLELOPATHY.f90` as having 44 leaked allocatable arrays. **This file does not exist.** The actual file is `SOURCE_CODE/ALLELOPATHY/mod_ALLELOPATHY.f90`, which has proper `ALLOC_ALLEOPATHY()` and `DEALLOC_ALLEOPATHY()` subroutines covering all 44 arrays. Existing test `test_allelopathy` validates this.

**Action:** Mark TODO item 1.1 as resolved (no fix needed) in the commit for Task 1.

---

## Task 1: Guard K_E Division-by-Zero in Euphotic Depth (P0)

The TODO plan listed this in `aquabc_II_pelagic_model.f90`, but the actual locations are in 3 library subroutines. When `K_E = 0` (no particles, no background extinction), `4.61D0 / K_E` produces `Inf` that propagates through light limitation and depth calculations.

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_CYANOBACTERIA.f90:398`
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90:453`
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_NOSTACALES.f90:195`
- Test: `tests/fortran/test_cyanobacteria.f90` (add K_E=0 test)
- Test: `tests/fortran/test_fix_cyn.f90` (add K_E=0 test)
- Test: `tests/fortran/test_nostocales.f90` (add K_E=0 test)

### Step 1: Write failing test for CYANOBACTERIA with K_E=0

Open `tests/fortran/test_cyanobacteria.f90` and add a new test subroutine that calls CYANOBACTERIA with `K_E = 0.0` for all nodes. The test should verify:
- No NaN values in any output derivative array
- No Inf values in output arrays

```fortran
! Add this subroutine inside the "contains" section of the test program,
! before the final "end program":

subroutine test_zero_ke()
    ! Test that K_E = 0 does not produce NaN/Inf
    ! (guards division in EUPHOTIC_DEPTH = 4.61 / K_E)
    use AQUABC_II_GLOBAL, only: DBL_PREC
    implicit none

    ! Use same setup as existing tests but set K_E = 0
    integer, parameter :: nkn = 4
    integer :: i
    logical :: has_nan_or_inf

    ! Reuse existing parameter/state setup from test defaults
    ! but override K_E to zero
    ! ... (copy the existing test setup pattern, set env%K_E = 0.0D0)

    write(*,'(A)') '  Test: CYANOBACTERIA with K_E = 0'

    ! After calling CYANOBACTERIA, check outputs:
    has_nan_or_inf = .false.
    do i = 1, nkn
        if (R_CYN_GROWTH(i) /= R_CYN_GROWTH(i)) has_nan_or_inf = .true.  ! NaN check
        if (abs(R_CYN_GROWTH(i)) > 1.0D30) has_nan_or_inf = .true.       ! Inf check
    end do

    if (has_nan_or_inf) then
        write(*,'(A)') '  [FAIL] K_E=0 produced NaN/Inf in growth rate'
        n_fail = n_fail + 1
    else
        write(*,'(A)') '  [PASS] K_E=0 handled safely'
        n_pass = n_pass + 1
    end if
end subroutine
```

Also add a call to `test_zero_ke()` in the main program body.

Apply the same pattern for `test_fix_cyn.f90` and `test_nostocales.f90`.

### Step 2: Run tests to verify they fail

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test_cyanobacteria && ./test_cyanobacteria`
Expected: FAIL — K_E=0 produces NaN/Inf in growth rate

Repeat for test_fix_cyn and test_nostocales.

### Step 3: Apply the K_E guard in all 3 files

In each file, replace:
```fortran
EUPHOTIC_DEPTH(:) = 4.61D0 / K_E(:)
```
with:
```fortran
EUPHOTIC_DEPTH(:) = 4.61D0 / max(K_E(:), 1.0D-20)
```

**File 1:** `aquabc_II_pelagic_lib_CYANOBACTERIA.f90` line 398
**File 2:** `aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90` line 453
**File 3:** `aquabc_II_pelagic_lib_NOSTACALES.f90` line 195

### Step 4: Run tests to verify they pass

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test_cyanobacteria test_fix_cyn test_nostocales && ./test_cyanobacteria && ./test_fix_cyn && ./test_nostocales`
Expected: All PASS, including the new K_E=0 tests

### Step 5: Run full Fortran test suite

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED (no regressions)

### Step 6: Verify full library build

Run: `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: Clean build, zero errors

### Step 7: Commit

```bash
git add \
  SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_CYANOBACTERIA.f90 \
  SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_FIX_CYANOBACTERIA.f90 \
  SOURCE_CODE/AQUABC/PELAGIC/AQUABC_PELAGIC_LIBRARY/aquabc_II_pelagic_lib_NOSTACALES.f90 \
  tests/fortran/test_cyanobacteria.f90 \
  tests/fortran/test_fix_cyn.f90 \
  tests/fortran/test_nostocales.f90
git commit -m "fix: guard EUPHOTIC_DEPTH division by K_E to prevent Inf when K_E=0

Add max(K_E, 1e-20) guard in CYANOBACTERIA, FIX_CYANOBACTERIA, and
NOSTOCALES subroutines. Add K_E=0 regression tests for all three."
```

---

## Task 2: SAVE Variable Thread-Safety Audit (P0)

22 module-level SAVE variables in `aquabc_II_pelagic_interface.f90` and 3 in `STRING_UTILS.f90`. These are shared across threads if the subroutines are ever called from within an OpenMP parallel region.

**Current safety:** The interface is called BEFORE the OpenMP parallel region in `aquabc_II_pelagic_model.f90`, and STRING_UTILS is only used at initialization. So there is **no active data race today**. But this assumption is undocumented and fragile.

**Files:**
- Modify: `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90:48-86`
- Modify: `SOURCE_CODE/CORE_UTILS/STRING_UTILS.f90:13-15`

### Step 1: Add thread-safety documentation to interface module

In `aquabc_II_pelagic_interface.f90`, add a comment block before the SAVE declarations (after `implicit none`, around line 50):

```fortran
    ! -----------------------------------------------------------------------
    ! THREAD-SAFETY NOTE (2026-02-14):
    ! All SAVE variables below are module-level persistent state. They are
    ! initialized in aquabc_init() and read (never written) during the
    ! OpenMP parallel region in AQUABC_PELAGIC_KINETICS. This is safe
    ! because:
    !   1. aquabc_init() runs BEFORE any parallel region
    !   2. The parallel region only READS these variables (never writes)
    !   3. The allocatable arrays (DERIVATIVES, PROCESS_RATES, etc.) are
    !      written only via array-section indexing [ns:ne] where each
    !      thread owns a disjoint chunk
    !
    ! WARNING: Do NOT call aquabc_init() or aquabc_set_flags() from
    ! within an OpenMP parallel region. Do NOT write to any of these
    ! variables from parallel code without synchronization.
    ! -----------------------------------------------------------------------
```

### Step 2: Add thread-safety documentation to STRING_UTILS

In `STRING_UTILS.f90`, add a comment before the SAVE declarations (around line 12):

```fortran
    ! THREAD-SAFETY NOTE: nfill, values, and names have SAVE attribute.
    ! These are only used during sequential initialization (parameter loading).
    ! Do NOT call para_add_value or para_get_value from OpenMP parallel regions.
```

### Step 3: Verify build still works

Run: `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: Clean build

### Step 4: Run all Fortran tests

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED

### Step 5: Commit

```bash
git add \
  SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_interface.f90 \
  SOURCE_CODE/CORE_UTILS/STRING_UTILS.f90
git commit -m "docs: add thread-safety annotations for SAVE variables

Document that 22 SAVE variables in pelagic_interface and 3 in
STRING_UTILS are safe under current OpenMP usage (read-only during
parallel region). Add warnings against calling init routines from
parallel code."
```

---

## Task 3: Replace Bare Except Blocks in Python (P1)

5 bare `except:` blocks in `shiny_app/app.py` catch all exceptions including `SystemExit` and `KeyboardInterrupt`, making debugging difficult and hiding real errors.

**Files:**
- Modify: `shiny_app/app.py` (lines 6436, 7132, 7265, 7291, 7555)
- Test: `tests/python/` (run existing tests for regression)

### Step 1: Fix bare except at line 6436 (file enumeration)

Replace:
```python
                    except:
                        pass
```
with:
```python
                    except OSError:
                        pass
```

**Rationale:** `os.path.getsize()` can only raise `OSError` (file deleted between listdir and getsize). This is the only expected failure.

### Step 2: Fix bare except at line 7132 (CSV row counting)

Replace:
```python
                    except:
                        file_info["rows"] = "?"
```
with:
```python
                    except (OSError, UnicodeDecodeError):
                        file_info["rows"] = "?"
```

**Rationale:** Opening and iterating a file can fail with `OSError` (permission, missing) or `UnicodeDecodeError` (binary file mistakenly treated as text).

### Step 3: Fix bare except at line 7265 (binary time range)

Replace:
```python
                except:
                    pass
```
with:
```python
                except (OSError, ValueError, IndexError):
                    pass
```

**Rationale:** `np.fromfile` can raise `OSError`; `float()` conversion can raise `ValueError`; empty/short arrays can raise `IndexError`.

### Step 4: Fix bare except at line 7291 (CSV time range)

Replace:
```python
                except:
                    pass
```
with:
```python
                except (OSError, ValueError, IndexError, KeyError):
                    pass
```

**Rationale:** `pd.read_csv` can raise `OSError`/`ValueError`; `.iloc` can raise `IndexError`; `float()` can raise `ValueError`; missing columns raise `KeyError`.

### Step 5: Fix bare except at line 7555 (CSV delimiter detection)

Replace:
```python
                except:
                    df = pd.read_csv(file_path, comment='#', nrows=5)
```
with:
```python
                except (pd.errors.ParserError, pd.errors.EmptyDataError, ValueError, OSError):
                    df = pd.read_csv(file_path, comment='#', nrows=5)
```

**Rationale:** `pd.read_csv` with wrong separator raises `ParserError` or `EmptyDataError`; malformed data raises `ValueError`; file access raises `OSError`.

### Step 6: Verify no bare excepts remain

Run: `grep -n 'except:' /home/razinka/AQUABCv0.2/shiny_app/app.py`
Expected: Zero matches (all replaced with specific types)

### Step 7: Run Python tests

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py`
Expected: All tests pass

### Step 8: Run ruff lint

Run: `cd /home/razinka/AQUABCv0.2 && python -m ruff check shiny_app/app.py --select E722`
Expected: Zero E722 (bare except) violations

### Step 9: Commit

```bash
git add shiny_app/app.py
git commit -m "fix: replace 5 bare except blocks with specific exception types

Lines 6436, 7132, 7265, 7291, 7555 in app.py now catch only expected
exceptions (OSError, ValueError, etc.) instead of suppressing all
errors including SystemExit and KeyboardInterrupt."
```

---

## Task 4: Update TODO Plan — Mark Resolved Items

**Files:**
- Modify: `TODO_IMPLEMENTATION_PLAN.md`

### Step 1: Update Sprint 1 checklist

Mark the completed items:
```markdown
### Sprint 1 — Critical Fixes (1–2 days)
- [x] 1.1 ALLELOPATHY memory leaks — **No fix needed** (file doesn't exist; mod_ALLELOPATHY.f90 has proper dealloc)
- [x] 1.2 K_E division guard — **Fixed** in CYANOBACTERIA, FIX_CYANOBACTERIA, NOSTOCALES (actual location: library files, not pelagic_model.f90)
- [x] 1.3 SAVE variable thread safety audit — **Documented** (no active race; annotations added)
- [x] 2.2 Bare except blocks — **Fixed** (5 blocks replaced with specific exception types)
```

### Step 2: Commit

```bash
git add TODO_IMPLEMENTATION_PLAN.md
git commit -m "docs: mark Sprint 1 items as complete in TODO plan"
```

---

## Post-flight: Full Verification

### Step 1: Run all Fortran tests

Run: `cd /home/razinka/AQUABCv0.2/tests/fortran && make clean && make test`
Expected: All tests PASSED

### Step 2: Run all Python tests

Run: `cd /home/razinka/AQUABCv0.2 && python -m pytest tests/python/ -v --ignore=tests/python/test_app_playwright.py --ignore=tests/python/test_app_selenium.py`
Expected: All tests pass

### Step 3: Full library build

Run: `cd /home/razinka/AQUABCv0.2 && make clean-all && make build-estas`
Expected: Clean build
