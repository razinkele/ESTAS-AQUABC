# Fail-loud positional constants reader — design

**Motivation:** `BACKLOG.md` §2 "fail-loud on missing constants." This is the **correctly-targeted**
retarget of the parked `2026-08-02-constants-fail-loud-design.md` (which hardened the wrong, 0D-only
reader — see its §12 and memory `aquabc-parallel-code-paths`).
**Date:** 2026-08-02
**Status:** Design — pending user review, then implementation.
**Scope tier:** One localized change to the shared ESTAS positional reader + a test.

---

## 1. Problem

The ESTAS box-model path (CL29, standard, sediment) reads its constants through
**`READ_MODEL_CONSTANTS`** (`SOURCE_CODE/ESTAS/mod_UTILS_01.f90:7-26`, module `UTILS_1`) — a
**positional, name-blind** reader shared by **both** the pelagic `WCONST_04.txt`
(`sub_READ_PELAGIC_INPUTS.f90:284`) and the sediment `W_SED_CONST.txt`
(`mod_BOTTOM_SEDIMENTS.f90:649`):

```fortran
real(kind = DBL), dimension(:), intent(out) :: MODEL_CONSTANTS
do
    read(unit = INPUT_NO, fmt = *, end = 200) CONSTANT_NO, CONSTANT_NAME, CONSTANT_VALUE
    MODEL_CONSTANTS(CONSTANT_NO) = CONSTANT_VALUE      ! by index; NAME ignored
end do
200 continue
```

Failure modes, all silent or catastrophic:
- **Dropped constant** — `MODEL_CONSTANTS` is `intent(out)` with **no initialisation**, so an index not
  present in the file keeps an **undefined (garbage)** value.
- **Out-of-range index** — `CONSTANT_NO < 1` or `> size` writes **out of bounds** → memory corruption /
  crash (no bounds check).
- **Duplicate index** — silently last-wins.
- **Malformed line** — only EOF (`end=200`) is handled; a read error is unhandled.
- **Wrong count** — never checked.

The name column is read but ignored, so a misnamed line with the *right* index is numerically harmless
(the value still lands in the correct slot).

## 2. Goal & scope

**Goal:** make `READ_MODEL_CONSTANTS` **fail loud and specific** on the dangerous cases, so a run's
constants are fully and correctly specified by its input file. Because the reader is shared, this
protects **both** the pelagic and sediment constant files in one change.

**In scope**
- Detect and name: **dropped** (unfilled) indices, **out-of-range** indices, **duplicate** indices,
  **malformed** lines, and **wrong count**.
- **Fail-loud by default** (`error stop`, listing the offending index numbers); env var
  `AQUABC_LENIENT_CONSTANTS=1` restores warn-and-continue.
- Zero-initialise `MODEL_CONSTANTS` so a dropped slot is deterministic (0), not garbage.
- A test covering complete / dropped / out-of-range / lenient.

**Out of scope**
- **Misnamed-with-correct-index detection** — numerically harmless (§1) and would need a canonical
  index→name table the ESTAS path lacks; deferred (noted honestly).
- `READ_MODEL_CONSTANTS_PRICE` (`:29-44`, a separate variant) — confirm callers; only touch it if it is
  actually on a run path (it is not called by the pelagic/sediment paths).

## 3. Design

No signature change — `size(MODEL_CONSTANTS)` is the expected count (`n`).

```fortran
use iso_fortran_env, only: error_unit
integer :: n, ios, nread, i, n_bad
logical, allocatable :: seen(:)
logical :: lenient
character(len=32) :: env

n = size(MODEL_CONSTANTS)
allocate(seen(n)); seen = .false.
MODEL_CONSTANTS = 0.0_DBL        ! dropped slots -> deterministic 0 (matters only in lenient mode)
nread = 0; n_bad = 0             ! n_bad counts out-of-range / duplicate / malformed lines

do
    read(unit=INPUT_NO, fmt=*, iostat=ios) CONSTANT_NO, CONSTANT_NAME, CONSTANT_VALUE
    if (ios < 0) exit                                  ! EOF
    if (ios > 0) then                                  ! malformed line
        write(error_unit,*) 'READ_MODEL_CONSTANTS: malformed line after ', nread, ' records'
        n_bad = n_bad + 1; exit
    end if
    nread = nread + 1
    if (CONSTANT_NO < 1 .or. CONSTANT_NO > n) then     ! would corrupt memory
        write(error_unit,*) 'READ_MODEL_CONSTANTS: index out of range [1,', n, ']: ', CONSTANT_NO
        n_bad = n_bad + 1; cycle
    end if
    if (seen(CONSTANT_NO)) then
        write(error_unit,*) 'READ_MODEL_CONSTANTS: duplicate index: ', CONSTANT_NO
        n_bad = n_bad + 1
    end if
    MODEL_CONSTANTS(CONSTANT_NO) = CONSTANT_VALUE
    seen(CONSTANT_NO) = .true.
end do

! report every dropped (unfilled) index by number
do i = 1, n
    if (.not. seen(i)) then
        write(error_unit,*) 'READ_MODEL_CONSTANTS: constant #', i, ' MISSING from file (defaulted to 0)'
        n_bad = n_bad + 1
    end if
end do

call get_environment_variable('AQUABC_LENIENT_CONSTANTS', env)
lenient = (trim(env) == '1')
if (n_bad > 0 .and. .not. lenient) then
    error stop 'READ_MODEL_CONSTANTS: incomplete/invalid constants file (see indices above)'
end if
```

Key mechanics (lessons from the parked spec's review):
- **`error stop`** (not plain `stop`, which exits 0 in gfortran) → a nonzero exit a caller/PEST can
  detect.
- Diagnostics to **stderr (`error_unit` from `iso_fortran_env`)** so a test can grep the offending
  indices (portable across gfortran + ifx).
- `size()`-based `n` → no signature change, and it naturally uses the correct expected count for **both**
  the pelagic (`n=323`) and sediment (`n=171`) call sites.
- `seen`-mask catches the case a bare count check cannot: `dup(X)+missing(Y)` gives `nread=n` but
  `seen(Y)=.false.`.

## 4. Byte-identity / gating

For an **index-complete** file (all `1..n` present exactly once) `n_bad = 0` → no diagnostics, no stop,
and every slot is overwritten by its file value so the zero-init is irrelevant → **byte-identical**
(including exit code). Verified (2026-08-02) that **every shipped setup is index-complete**:
- pelagic `INPUTS/WCONST_04.txt` and `INPUTS_CL29/WCONST_04.txt`: declared `NUM_MODEL_CONSTANTS = 323`,
  323 lines, indices `1..323`, **0 gaps / 0 dups**;
- sediment `INPUTS/W_SED_CONST.txt` and `INPUTS_couptest/W_SED_CONST.txt`: 171 lines, `1..171`, 0 gaps.

So fail-loud-by-default is a safe contract tightening — nothing shipped breaks; the `AQUABC_LENIENT_
CONSTANTS=1` hatch covers any external caller that deliberately relied on defaults.

## 5. Files to change

1. `SOURCE_CODE/ESTAS/mod_UTILS_01.f90` — the `READ_MODEL_CONSTANTS` body (`:7-26`): the seen-mask +
   zero-init + bounds/dup/malformed checks + the named report + the env-var strict/lenient switch. No
   signature change; `UTILS_1` already `use GLOBAL` (provides `DBL`).
2. A test: `tests/fortran/test_read_constants.f90` (+ its `TEST_PROGS` Makefile wiring), or — for the
   abort path — a scripted integration test (below).

## 6. Testing

- **Byte-identity gate (primary):** old-code vs new-code across Standard (`MODEL_SEDIMENTS=0`), CL29
  (`=1`, `ESTAS_HOLD_VOLUME=1`), and the sediment test (`=2`, which exercises the *sediment* call site of
  the same reader) — outputs **and exit codes** identical (all files complete).
- **Behaviour test — the abort path needs a subprocess** (an in-process unit test cannot survive
  `error stop`): run the built `ESTAS_II` on a temp-dir copy of a setup whose `WCONST_04.txt` is doctored
  (a) missing one line, (b) an out-of-range index, (c) a duplicate index → assert **nonzero exit** and
  the **stderr names the offending index**; and with `AQUABC_LENIENT_CONSTANTS=1` → warns + completes.
  Never edit a tracked constants file — use temp copies.
- **In-process unit test** (`tests/fortran/`) can cover the non-aborting paths: a complete array is read
  correctly; lenient mode returns with the dropped slot = 0.

## 7. Risks

- **Contract tightening (fail-loud default):** verified no shipped setup breaks (all index-complete); the
  env-var hatch is the escape. Document `AQUABC_LENIENT_CONSTANTS` in `CHANGELOG` + the reference manual,
  and ensure CI runs the behaviour test **without** the lenient flag set.
- **The reader is shared** — a bug here affects both pelagic and sediment reads; the 3-config gate
  (including `MODEL_SEDIMENTS=2`) covers both call sites.
- **`error stop` in a library routine** aborts the whole process — correct for a fatal input error, and
  the reason (a genuinely unusable constants file) warrants it; lenient mode is the opt-out.
- **Misnamed-with-right-index stays undetected** (deferred, §2) — stated honestly so the reproducibility
  claim isn't overstated.

## 8. Decisions log

- **Target:** `READ_MODEL_CONSTANTS` (`mod_UTILS_01.f90`) — the shared ESTAS positional reader, verified
  (call-path traced) as the one CL29/standard/sediment actually use.
- **Default:** fail-loud (`error stop`) on dropped / out-of-range / duplicate / malformed / wrong-count;
  `AQUABC_LENIENT_CONSTANTS=1` escape hatch (consistent with the parked spec's chosen contract).
- **Init:** zero (deterministic) over a NaN sentinel.
- **No signature change** (`size()` = expected count); **both readers** fixed at once.
- **Deferred:** misnamed-with-right-index (needs a name table; numerically harmless).

## 9. References

- The parked, mis-targeted spec: `docs/superpowers/specs/2026-08-02-constants-fail-loud-design.md` §12
  (its 3-reviewer review identified the correct target + the `error stop`/stderr/subprocess-test
  mechanics reused here).
- Memory `aquabc-parallel-code-paths` (why the first attempt hit the wrong reader).
