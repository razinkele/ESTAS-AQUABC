# Fail-loud constants reader — design

**Motivation:** `BACKLOG.md` §2 (Calibration & reproducibility) — "fail-loud on missing constants."
Part of the calibration-rigor frontier (the phyto-side nutrient levers are exhausted;
see `cl29-epa-validation` structural conclusion).
**Date:** 2026-08-02
**Status:** Design — pending user review, then implementation plan.
**Scope tier:** One localized Fortran change to the pelagic constants reader + a test.

---

## 1. Problem

`READ_PELAGIC_MODEL_CONSTANTS` (`aquabc_II_pelagic_model_constants.f90:1656`) reads a name-keyed
`WCONST_*.txt` file. The mechanism (verified):
- `DEFAULT → INSERT → READ → INIT`: `INSERT` registers every constant NAME + its hardcoded default in
  the `para_aqua` table (`STRING_UTILS.f90`); `READ` overwrites table entries for the names **present
  in the file** via `para_put_value`; `INIT` reads each value back.
- **A constant missing from the file silently keeps its default** (never overwritten). The only guard is
  `if (j /= nconsts)` (`:1694`) which prints a bare **count** and whose `stop` is **commented out**
  (`:1698`) — it never says *which* constant is missing.
- **A misnamed constant** in the file hits `para_put_value`, which `stop`s with the cryptic
  `'error stop para_put_value: name does not exist'` (`STRING_UTILS.f90:102`) — no name, aborts at the
  first bad line only.

**Re-verification (2026-08-02, the BACKLOG said "re-verify counts before acting"):** the shipped files
are **complete** — CL29 `INPUTS_CL29/WCONST_04.txt` and standard `INPUTS/WCONST_04.txt` each have **323
unique constant names == the 323 registered in code** (zero missing, zero extra). So the BACKLOG's
"306 vs 318 silent default-fill" is **stale**; there is no current silent-default-fill in any shipped
setup. This item is therefore **defensive hardening for the calibration workflow**, not a live-bug fix:
calibration *edits* WCONST files, and a dropped or renamed constant currently fails silently (default)
or cryptically (unnamed stop). That is the reproducibility footgun to close.

## 2. Goal & scope

**Goal:** make the pelagic constants reader **fail loud and name-specific** — a run's parameters must be
fully specified by its input file, never silently defaulted or cryptically aborted.

**In scope**
- Enhance `READ_PELAGIC_MODEL_CONSTANTS` to detect and **name** (a) constants registered in code but
  **missing** from the file (silently defaulted), and (b) file names **not registered** in code
  (misnamed / stale).
- **Fail-loud by default:** abort (with the full list of offending names) if either set is non-empty.
- **Lenient escape hatch:** env var `AQUABC_LENIENT_CONSTANTS=1` restores the old warn-and-continue
  behaviour (now with the named report instead of a bare count).
- A test proving all three paths (complete / missing / misnamed).

**Out of scope**
- The other constant readers (sediment `W_SED_CONST.txt`, allelopathy `EXTRA_WCONST.txt`) — they have
  their own readers; note as a follow-on, same pattern.
- The `WRITE_PELAGIC_MODEL_CONSTANTS` path (already writes all registered constants — fine).
- Auto-completing / auto-generating WCONST files (unnecessary — shipped files are complete).
- The cost-function and PEST-workflow items (separate calibration-rigor sub-items).

## 3. Design

Rework the body of `READ_PELAGIC_MODEL_CONSTANTS` (`:1682–1699`). `nconsts = para_get_fill()` is the
registered count; `para_exists_name(name)`, `para_get_name(i,name)` are available primitives.

```
nconsts = para_get_fill()
allocate seen(nconsts) = .false.          ! which registered constants the file set
n_bad = 0                                  ! file names not registered in code
j = 0
do
  read(iu,*,iostat=ios) i, name, value
  if (ios < 0) exit
  j = j + 1
  if (para_exists_name(name)) then
    call para_put_value(name, value)       ! (never stops now — guarded by the check above)
    mark seen(index-of name) = .true.      ! index via a para_get_name compare loop, O(nconsts)
  else
    n_bad = n_bad + 1
    record name in bad_names(n_bad)        ! collect ALL, don't stop at the first
  end if
end do

! missing = registered names with seen(i) == .false.
build missing_names by scanning para_get_name(1..nconsts) where .not. seen(i)

report:
  if (size(missing_names) > 0) list them (name + current default value)  ! "silently defaulted"
  if (n_bad > 0)               list bad_names                            ! "unknown/misnamed in file"
  keep the existing "j constants read" line

strict = (AQUABC_LENIENT_CONSTANTS not set to '1')     ! read via get_environment_variable
if (strict .and. (size(missing_names) > 0 .or. n_bad > 0)) then
    stop 'error stop READ_PELAGIC_MODEL_CONSTANTS: incomplete/invalid constants file (see names above)'
end if
```

Notes:
- **Collect-then-report:** unlike today's stop-at-first-bad-name, this reports *every* offending name in
  one pass — far better for a calibration edit that broke several.
- The `seen`-index lookup is `O(nconsts²)≈323²` once at startup — negligible.
- `AQUABC_LENIENT_CONSTANTS` is read with `get_environment_variable` (the `ESTAS_HOLD_VOLUME` /
  `ESTAS_PELAGIC_SOLVER` pattern, `mod_SOLVER.f90:130`). Unset or ≠`1` ⟹ strict (fail-loud).

## 4. Byte-identity / gating

Every shipped setup's WCONST is **complete** (verified §1), so `missing_names` and `bad_names` are both
empty → **no report, no stop, no output change** — byte-identical (including exit code) for Standard,
CL29, and the sediment test. The behaviour changes *only* for an incomplete/misnamed file, which no
shipped setup produces. The strict default is thus a safe contract tightening; the env-var hatch covers
any external workflow that deliberately relied on defaults.

## 5. Files to change

1. `SOURCE_CODE/AQUABC/PELAGIC/aquabc_II_pelagic_model_constants.f90` — the `READ_PELAGIC_MODEL_CONSTANTS`
   body (`:1682–1699`): the seen-tracking, the missing/misnamed detection + named report, the env-var
   strict/lenient switch. (No new module, no signature change — `file` arg unchanged.)

## 6. Testing

- **Byte-identity gate (primary):** old-code vs new-code, the 3 standing configs (Standard
  `MODEL_SEDIMENTS=0`, CL29 `=1` `ESTAS_HOLD_VOLUME=1`, sediment test `=2`) — outputs **and exit codes**
  identical (all files complete).
- **Behaviour test** (Fortran unit test under `tests/fortran/`, or a scripted integration test that runs
  the built binary on doctored WCONST copies in a temp dir and checks exit code + stderr):
  1. **complete** file → no error, run proceeds, all 323 constants set.
  2. **missing** (drop one line, e.g. `KHS_DIN_CYN`) → default: aborts, message NAMES `KHS_DIN_CYN`;
     with `AQUABC_LENIENT_CONSTANTS=1`: warns naming it, continues.
  3. **misnamed** (rename one line, e.g. `KHS_DIN_CYNX`) → default: aborts, message NAMES `KHS_DIN_CYNX`
     as unknown (and `KHS_DIN_CYN` as missing); no cryptic `para_put_value` stop.
  The doctored files are temp copies — **never edit a tracked WCONST**.

## 7. Risks

- **Contract tightening (default flip to fail-loud):** verified no shipped setup breaks (all complete);
  the `AQUABC_LENIENT_CONSTANTS=1` hatch restores the old behaviour for any external caller that relied
  on silent defaults. Document the env var in `CHANGELOG` + the reference manual.
- **Other constant readers** (sediment / allelopathy) are unchanged — a partial guarantee; noted as a
  same-pattern follow-on so the reproducibility claim isn't overstated.
- **Startup cost** of the `seen` lookup: `O(nconsts²)` once ≈ 10⁵ string compares — negligible.

## 8. Decisions log

- **Scope:** the pelagic constants reader only; fail-loud + name the offenders; escape-hatch env var.
- **Default:** fail-loud (abort) on any missing/misnamed constant — the reproducibility-correct contract;
  safe because shipped files are complete.
- **Reframe:** re-verification showed the files are complete, so this is *defensive calibration-safety
  hardening*, not a live-bug fix — the honest framing.
