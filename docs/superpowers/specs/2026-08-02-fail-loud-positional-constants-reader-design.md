# Fail-loud positional constants reader — design

**Motivation:** `BACKLOG.md` §2 "fail-loud on missing constants." This is the **correctly-targeted**
retarget of the parked `2026-08-02-constants-fail-loud-design.md` (which hardened the wrong, 0D-only
reader — see its §12 and memory `aquabc-parallel-code-paths`).
**Date:** 2026-08-02
**Status:** ✅ **Design CONFIRMED sound by a 3-reviewer in-loop review (2026-08-02) — refinements applied
(see §10).** Target correct (verified), per-box calling pattern fine, byte-identity sound (all shipped
files index-complete). Ready for implementation once the §10 test-wiring + doc refinements are folded in
(done below).
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
lenient = (trim(adjustl(env)) == '1')     ! only the literal '1' enables lenient (matches ESTAS_* idiom)
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
- The `use iso_fortran_env, only: error_unit` line goes at subroutine scope **before** `implicit none`.
- **"Wrong count" is covered transitively, not by an explicit check:** too few lines → unfilled indices
  (seen-mask); too many → an extra in-range index is a duplicate, an out-of-range one is bounds-caught.
  The authority becomes `size()`; a disagreement with the file's declared count surfaces as
  "missing"/"out of range", which is the more actionable message.
- **"Malformed" = a type-conversion read error only** (`ios>0`). List-directed input has blind spots the
  design does NOT claim to catch: a line **missing its value column** does not error — the read spans the
  record boundary and consumes the next line's first token, misaligning silently (it later surfaces as an
  out-of-range/duplicate/missing index, still loud, just mislabeled); blank lines are skipped; a full-line
  `!comment` trips `ios>0` → the loop exits and every remaining index is reported MISSING (loud + noisy).
  The trailing `! N description` on each real data line is correctly ignored (the read stops after 3
  items).

## 4. Byte-identity / gating

For an **index-complete** file (all `1..n` present exactly once) `n_bad = 0` → no diagnostics, no stop,
and every slot is overwritten by its file value so the zero-init is irrelevant → **byte-identical**
(including exit code). The in-loop review (§10) verified **every shipped setup that goes through this
reader is index-complete**:
- pelagic `WCONST_04.txt` in **all four** setups (`INPUTS/`, `INPUTS_CL29/`, `INPUTS_CL29_2023clim/`,
  `INPUTS_couptest/`): declared `NUM_MODEL_CONSTANTS = 323`, 323 lines, `1..323`, **0 gaps / 0 dups**
  (every root `INPUT_*.txt` driver points at one of these four folders);
- sediment `W_SED_CONST.txt` (`INPUTS/`, `INPUTS_couptest/`): 171 lines (`= NUM_SED_CONSTS`,
  `mod_GLOBAL.f90:40`), `1..171`, 0 gaps. The sediment read is **gated on `MODEL_SEDIMENTS > 1`**
  (`mod_AQUATIC_MODEL.f90:557`), so `INPUTS_CL29/` (`MODEL_SEDIMENTS=1`, no `W_SED_CONST.txt`) never calls
  it — only the `=2` setups (`sediment_test`, `couptest`) do, and theirs are complete.
- **CI:** `build-and-run` runs the standard `INPUT.txt` (`INPUTS/`, `MODEL_SEDIMENTS=0`, complete, no
  sediment read); the 0D example uses the *other* (para_aqua) reader → unaffected. Fail-loud-default
  keeps CI green.

So fail-loud-by-default is a safe contract tightening — nothing shipped breaks; the `AQUABC_LENIENT_
CONSTANTS=1` hatch covers any external caller that relied on defaults (see the §7 tightening tail).

## 5. Files to change

1. `SOURCE_CODE/ESTAS/mod_UTILS_01.f90` — the `READ_MODEL_CONSTANTS` body (`:7-26`): `use iso_fortran_env,
   only: error_unit` (before `implicit none`); the seen-mask + zero-init + bounds/dup/malformed checks +
   the named report (to `error_unit`) + the `AQUABC_LENIENT_CONSTANTS` strict/lenient switch. No signature
   change; `UTILS_1` already `use GLOBAL` (provides `DBL`).
2. **Test — a subprocess behaviour test in CI, NOT `tests/fortran`.** The `tests/fortran` harness
   (`Makefile:302-319`) treats a nonzero exit as *failure*, so an `error stop` driver cannot live there.
   Add a small **standalone abort-driver** `tests/fortran/drivers/read_constants_probe.f90` that just
   calls `READ_MODEL_CONSTANTS` on a file path from `argv` (the ~5-module chain: `precision_kinds →
   mod_GLOBAL → TIME_SERIES/INTERPOLATE → aquabc_pel_state_var_indexes → UTILS_1`), plus a runner (a
   `pytest`/shell wrapper) that copies a WCONST to a temp dir, doctors it, runs the probe, and **inverts
   the exit-code assertion**. Wire this runner into the **`build-and-run`** CI job (which already builds
   gfortran `ESTAS_II`), run **without** `AQUABC_LENIENT_CONSTANTS`.
3. `CHANGELOG.md` and `docs/ESTAS_Reference_Manual.md` — document `AQUABC_LENIENT_CONSTANTS` alongside the
   existing `ESTAS_HOLD_VOLUME` / `ESTAS_PELAGIC_SOLVER` env vars.
4. `.github/workflows/ci.yml` — the `build-and-run` step that runs the probe runner (item 2).

## 6. Testing

- **Byte-identity gate (regression net, NOT feature proof):** old-code vs new-code across Standard
  (`MODEL_SEDIMENTS=0`), CL29 (`=1`, `ESTAS_HOLD_VOLUME=1`), and the sediment test (`=2`, which exercises
  the *sediment* call site of the same reader) — outputs **and exit codes** identical (all files
  complete). This proves "no shipped setup changes" but is **vacuous for the new logic** (complete files
  never enter the error paths) — hence the positive test below is mandatory.
- **Positive behaviour test (the feature proof) — subprocess, in `build-and-run` CI:** run the
  abort-driver (§5.2) on temp-dir copies of `WCONST_04.txt` doctored for each case — (a) a line removed,
  (b) an out-of-range index, (c) a duplicate index — and assert **nonzero exit** + the **stderr contains a
  stable token** (grep the word `MISSING` / `range` / `duplicate` and the index number with `grep -w`,
  not a column-sensitive layout — list-directed `write` pads integers). A `AQUABC_LENIENT_CONSTANTS=1`
  case asserts exit 0 + the warning present. Never edit a tracked constants file — temp copies only.
- **Message contract (pin it, since the grep depends on it):** the report substrings are fixed —
  `... MISSING ...`, `... index out of range ...`, `... duplicate index ...` — each followed by the
  integer index. The behaviour test greps these.
- **Per-box repetition:** in **lenient** mode the pelagic reader runs once per box (the file is re-opened
  per box, §3), so each bad-line report appears **once per box** (25 Standard / 29 CL29). The lenient
  test must tolerate a box-count-multiple of report lines, not assert a single line; strict mode aborts on
  box 1 (single report).

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
- **Out-of-gate tightening tail (unverified files that ALSO use this reader / a sibling):** the fixed
  reader is `READ_MODEL_CONSTANTS`; `EXTRA_WCONST.txt` uses a *different* routine
  (`READ_EXTRA_PELAGIC_MODEL_CONSTS`, `sub_READ_PELAGIC_INPUTS.f90:301`) and stays unguarded (correct
  scope). Not in the 3-config gate and hence completeness-unverified: the variant
  `WCONST_02.txt`/`WCONST_03.txt` inputs (via `PELAGIC_INPUTS_WCONST_02.txt`) and the CLI-override
  constants file (`ESTAS_II.f90:38`). If any such file is index-incomplete, fail-loud-default would abort
  it — that is the intended behaviour, and `AQUABC_LENIENT_CONSTANTS=1` is the escape; flagged so the
  "nothing breaks" claim is scoped to the shipped/gated setups, not literally every file on disk.

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

---

## 10. In-loop review outcome (2026-08-02) — CONFIRMED sound, refinements applied

A 3-reviewer adversarial in-loop review (Fortran/byte-identity/per-box; completeness-of-all-files;
scope/testability). Unlike the parked v1, **no reviewer found a mis-targeting or a design flaw** — the
core is sound and the refinements below are folded into §3–§7.

**Confirmed (load-bearing):**
- **Target correct** — call chain traced: `mod_AQUATIC_MODEL.f90:158/573` → the pelagic
  (`sub_READ_PELAGIC_INPUTS.f90:284`) and sediment (`mod_BOTTOM_SEDIMENTS.f90:649`) callers are the *only*
  two; `READ_MODEL_CONSTANTS_PRICE` unused; the 0D `para_aqua` reader is the separate, previously
  mis-targeted path.
- **Per-box pattern is fine** — the file is `OPEN(status='OLD')`+`close`'d **each box iteration**
  (`sub_READ_PELAGIC_INPUTS.f90:282/287`), so every box re-reads from BOF (no drained-unit bug); the
  reader is stateless per call (fresh `seen`, per-slice zero-init). Strict aborts on box 1.
- **Byte-identity sound**; **all shipped/CI files index-complete** (§4); `error stop` + `get_environment_variable`
  already used in-tree (nonzero exit confirmed); no `GLOBAL` shadowing of the 8 new locals.

**Refinements applied:**
- **Test wiring corrected (the one HIGH finding):** the abort test can't live in `tests/fortran` (its
  `make test` treats nonzero exit as failure) → a standalone abort-driver + a subprocess runner in the
  `build-and-run` CI job; `ci.yml` + `CHANGELOG.md` + `docs/ESTAS_Reference_Manual.md` added to §5 (§6).
- **Per-box lenient repetition** documented; the lenient test tolerates a box-count multiple of report
  lines (§6).
- `trim(adjustl(env))` + literal-`1`-only (§3); `error_unit` placement + pinned stderr message contract
  (§3/§6); **malformed-line claim narrowed** to type-conversion errors only (§3); §4 completeness
  generalised to all four setups + the sediment `MODEL_SEDIMENTS>1` gate; the **out-of-gate tightening
  tail** (EXTRA_WCONST / variant WCONST_02-03 / CLI-override) stated (§7); "wrong count" transitivity
  noted (§3).
