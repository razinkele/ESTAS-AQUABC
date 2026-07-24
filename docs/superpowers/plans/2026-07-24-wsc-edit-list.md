# WSC refactor — definitive per-occurrence enumerated edit list

Refactor: move 11 `GLOBAL` allocatable arrays into module instance `wsc` (`type wsc_state_t`,
new leaf module `WATER_SEDIMENT_COUPLING`). Bare GLOBAL ref `MEMBER` → `wsc%MEMBER`.
Spec: `docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md`.

The 11 members (all `real(DBL), allocatable, dimension(:,:)`):
`FLUXES_TO_WATER_COLUMN`, `FLUXES_OUTPUT_TO_WATER_COLUMN`, `DISSOLVED_FRACTIONS`,
`FRACTION_OF_DEPOSITION`, `SETTLING_RATES`, `NOT_DEPOSITED_FLUXES`, `FLUXES`,
`SETTLING_VELOCITIES_OUTPUT`, `EFFECTIVE_DISSLOVED_FRACTIONS`, `EFFECTIVE_DEPOSITION_FRACTIONS`,
`DEPOSITION_AREA_RATIOS`.

Rename surface = **5 files**. `mod_PELAGIC_BOX.f90` and `sub_READ_PELAGIC_INPUTS.f90` are OFF-surface
(component-only; verified below). Method = explicit enumerated `file:line old→new`, applied one edit at
a time (a blanket regex would over-prefix dummy-body uses invisibly to the byte-identical gate).

Classification legend: **RENAME** → prefix `wsc%`. **SKIP: dummy** (redeclared as a subroutine dummy
in that scope). **SKIP: component** (`… % DISSOLVED_FRACTIONS`, PELAGIC_BOX component; `%` may be on
prev continuation line). **SKIP: string** / **SKIP: comment**. **look-alike** = a token that merely
shares a substring (`PRESCRIBED_SEDIMENT_FLUXES`, `SEDIMENT_FLUXES`, `bsed%FLUXES_TO/FROM_SEDIMENTS`,
`NUM_FLUXES_*`, `*_FLUXES_*_FILENAME`) — **not an occurrence of any of the 11**, listed for audit only.

---

## Totals

| File | RENAME | SKIP (member-token) |
|---|---|---|
| mod_AQUATIC_MODEL.f90 | 13 | 1 (string) |
| mod_SOLVER.f90 | 17 | 49 (42 dummy + 3 component + 4 comment) |
| ESTAS_II.f90 | 11 | 0 |
| mod_SIMULATE.f90 | 9 | 0 |
| mod_BOTTOM_SEDIMENTS.f90 | 3 | 6 (dummy) |
| **TOTAL** | **53** | **56** |

Only one `call SOLVE` site in the whole tree (`mod_SIMULATE.f90:319`). Three `call CALC_DERIV`
(`mod_SOLVER.f90:151/342/398`, all inside `SOLVE`). `SEDIMENT_TRANSPORT` (`mod_BOTTOM_SEDIMENTS.f90:332`)
has no callers (dead code) but still host-associates the GLOBALs → its non-dummy refs rename.
Whole-tree scan found **no 6th consumer file** with a genuine GLOBAL ref (AQUABC library + `build/*__genmod.f90`
hits are library dummy args / a distinct local `FLUXES` / build artifacts — all off-surface).

---

## 1. mod_GLOBAL.f90 — deletion + breadcrumb

**Delete lines 142–156 inclusive** (both comment sub-blocks + the 11 declarations + interspersed blanks),
replace with a breadcrumb mirroring the `bsed` one at 135–140. Members declared at lines
143,144,147,148,149,150,151,153,154,155,156.

```
142    ! Variables for water column - bottom sediment interaction
143    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_TO_WATER_COLUMN
144    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES_OUTPUT_TO_WATER_COLUMN
145    ! End of variables for water column - bottom sediment interaction
146
147    real(kind = DBL), allocatable, dimension(:, :) :: DISSOLVED_FRACTIONS
148    real(kind = DBL), allocatable, dimension(:, :) :: FRACTION_OF_DEPOSITION
149    real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_RATES
150    real(kind = DBL), allocatable, dimension(:, :) :: NOT_DEPOSITED_FLUXES
151    real(kind = DBL), allocatable, dimension(:, :) :: FLUXES
152
153    real(kind = DBL), allocatable, dimension(:, :) :: SETTLING_VELOCITIES_OUTPUT
154    real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DISSLOVED_FRACTIONS
155    real(kind = DBL), allocatable, dimension(:, :) :: EFFECTIVE_DEPOSITION_FRACTIONS
156    real(kind = DBL), allocatable, dimension(:, :) :: DEPOSITION_AREA_RATIOS
```

Suggested breadcrumb (replaces 142–156):
```
    ! -----------------------------------------------------------------------------------
    ! Particle settling/deposition + water<->sediment flux coupling state (11 members)
    ! moved to the derived type `wsc` (type wsc_state_t) in module WATER_SEDIMENT_COUPLING
    ! — see docs/superpowers/specs/2026-07-23-water-sediment-coupling-derived-type-design.md.
    ! -----------------------------------------------------------------------------------
```
⚠️ Do NOT touch line 157 (blank) or 158–165 (`*_FILENAME` scalars) or the `bsed` breadcrumb 135–140.
GLOBAL allocatable count drops **23 → 12**.

---

## 2. mod_WATER_SEDIMENT_COUPLING.f90 — new leaf module

New file per spec §"New module" (`use precision_kinds, only: DBL`; `type wsc_state_t` with the 11
members in listed order; `type(wsc_state_t), public :: wsc`; no `contains`).

---

## 3. mod_AQUATIC_MODEL.f90  (module AQUATIC_MODEL)

**Insert `use WATER_SEDIMENT_COUPLING, only: wsc` after line 17** (`    use GLOBAL`).

Allocation block: members at **220,221** (water-coupling) and **230–238** (settling); non-member code at
222–229 (SURFACE_BOXES loop + MODEL_CONSTANTS). Zero-init at **660,661**.

| line | current code (trimmed) | action |
|---|---|---|
| 220 | `allocate(FLUXES_TO_WATER_COLUMN       (nkn,(nstate + …)))` | RENAME → `allocate(wsc%FLUXES_TO_WATER_COLUMN       (…))` |
| 221 | `allocate(FLUXES_OUTPUT_TO_WATER_COLUMN(nkn,(…)))` | RENAME → `allocate(wsc%FLUXES_OUTPUT_TO_WATER_COLUMN(…))` |
| 230 | `allocate(DISSOLVED_FRACTIONS           (nkn,(…)))` | RENAME → `allocate(wsc%DISSOLVED_FRACTIONS           (…))` |
| 231 | `allocate(FRACTION_OF_DEPOSITION        (nkn,(…)))` | RENAME → `allocate(wsc%FRACTION_OF_DEPOSITION        (…))` |
| 232 | `allocate(SETTLING_RATES                (nkn,(…)))` | RENAME → `allocate(wsc%SETTLING_RATES                (…))` |
| 233 | `allocate(NOT_DEPOSITED_FLUXES          (nkn,(…)))` | RENAME → `allocate(wsc%NOT_DEPOSITED_FLUXES          (…))` |
| 234 | `allocate(FLUXES                        (nkn, NUM_SED_VARS))` | RENAME → `allocate(wsc%FLUXES                        (…))` |
| 235 | `allocate(SETTLING_VELOCITIES_OUTPUT    (nkn,(…)))` | RENAME → `allocate(wsc%SETTLING_VELOCITIES_OUTPUT    (…))` |
| 236 | `allocate(EFFECTIVE_DISSLOVED_FRACTIONS (nkn,(…)))` | RENAME → `allocate(wsc%EFFECTIVE_DISSLOVED_FRACTIONS (…))` |
| 237 | `allocate(EFFECTIVE_DEPOSITION_FRACTIONS(nkn,(…)))` | RENAME → `allocate(wsc%EFFECTIVE_DEPOSITION_FRACTIONS(…))` |
| 238 | `allocate(DEPOSITION_AREA_RATIOS        (nkn,(…)))` | RENAME → `allocate(wsc%DEPOSITION_AREA_RATIOS        (…))` |
| 263 | `'  SEDIMENT FLUXES (g/m^3/days)'` | **SKIP: string** (output-file header) |
| 660 | `FLUXES_TO_WATER_COLUMN        = 0.0D0` | RENAME → `wsc%FLUXES_TO_WATER_COLUMN        = 0.0D0` |
| 661 | `FLUXES_OUTPUT_TO_WATER_COLUMN = 0.0D0` | RENAME → `wsc%FLUXES_OUTPUT_TO_WATER_COLUMN = 0.0D0` |

look-alikes (not members, untouched): 546 `READ_BOTTOM_SEDS_FLUXES_INPUTS`, 603 `BOTTOM_SEDIMENT_FLUXES_FILENAME`,
631 `COCOA_FLUXES_FROM_SEDIMENTS_FILENAME`, 641 `COCOA_FLUXES_TO_SEDIMENTS_FILENAME`.

**Count: 13 RENAME, 1 SKIP.**

---

## 4. mod_SOLVER.f90  (module PELAGIC_SOLVER)

**Insert `use WATER_SEDIMENT_COUPLING, only: wsc` after line 3** (`    use GLOBAL`).

Scope map: `SOLVE` = 50–456 (dummies: SETTLING_VELOCITIES_OUTPUT, EFFECTIVE_DISSLOVED_FRACTIONS,
EFFECTIVE_DEPOSITION_FRACTIONS, DEPOSITION_AREA_RATIOS). `CALC_DERIV` = 720–1658 (dummies:
EFFECTIVE_DISSLOVED_FRACTIONS, EFFECTIVE_DEPOSITION_FRACTIONS, DEPOSITION_AREA_RATIOS; + non-member
PRESCRIBED_SEDIMENT_FLUXES). Verified: **no local `::` declaration** of the 7 non-shadowed members →
their bare refs are host-associated GLOBAL. Coupling block 1536–1635 is inside `if (MODEL_BOTTOM_SEDIMENTS > 1)` (mode-2, gate at 1476).

### 4a. RENAME (17) — all inside CALC_DERIV

| line | current code (trimmed) | RENAME → new code |
|---|---|---|
| 1537 | `DISSOLVED_FRACTIONS    = EFFECTIVE_DISSLOVED_FRACTIONS` | **LHS only** → `wsc%DISSOLVED_FRACTIONS    = EFFECTIVE_DISSLOVED_FRACTIONS` (RHS = dummy, keep bare) |
| 1538 | `FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS` | **LHS only** → `wsc%FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS` (RHS = dummy) |
| 1544 | `SETTLING_VELOCITIES, DISSOLVED_FRACTIONS, &` | `DISSOLVED_FRACTIONS` → `wsc%DISSOLVED_FRACTIONS` (actual arg) |
| 1546 | `SETTLING_RATES     , bsed%FLUXES_TO_SEDIMENTS, &` | `SETTLING_RATES` → `wsc%SETTLING_RATES` (actual arg) |
| 1548 | `1 , FRACTION_OF_DEPOSITION              , &` | `FRACTION_OF_DEPOSITION` → `wsc%FRACTION_OF_DEPOSITION` |
| 1549 | `NOT_DEPOSITED_FLUXES, nstate            , &` | `NOT_DEPOSITED_FLUXES` → `wsc%NOT_DEPOSITED_FLUXES` |
| 1587 | `FLUXES_TO_WATER_COLUMN = 0.0D0` | → `wsc%FLUXES_TO_WATER_COLUMN = 0.0D0` |
| 1591 | `FLUXES_TO_WATER_COLUMN, nkn, nstate)` | → `wsc%FLUXES_TO_WATER_COLUMN, nkn, nstate)` (actual arg) |
| 1623 | `FLUXES_OUTPUT_TO_WATER_COLUMN(:,:) = FLUXES_TO_WATER_COLUMN(:,:)` | **BOTH** → `wsc%FLUXES_OUTPUT_TO_WATER_COLUMN(:,:) = wsc%FLUXES_TO_WATER_COLUMN(:,:)` (2 renames) |
| 1626 | `FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) = &` | → `wsc%FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) = &` |
| 1627 | `FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) / DRIVING_FUNCTIONS(:, 8)` | → `wsc%FLUXES_TO_WATER_COLUMN(:,STATE_VAR_NO) / …` |
| 1629 | `NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) = &` | → `wsc%NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) = &` |
| 1630 | `NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) / …` | → `wsc%NOT_DEPOSITED_FLUXES  (:,STATE_VAR_NO) / …` |
| 1632 | `SETTLING_RATES        (:,STATE_VAR_NO) = &` | → `wsc%SETTLING_RATES        (:,STATE_VAR_NO) = &` |
| 1633 | `SETTLING_RATES        (:,STATE_VAR_NO) / …` | → `wsc%SETTLING_RATES        (:,STATE_VAR_NO) / …` |
| 1654 | `(DERIVATIVES(i,:) + FLUXES_TO_WATER_COLUMN(i,:)) * &` | → `… + wsc%FLUXES_TO_WATER_COLUMN(i,:)) * &` |

(1537 + 1538 + 1544 + 1546 + 1548 + 1549 + 1587 + 1591 + 1623×2 + 1626 + 1627 + 1629 + 1630 + 1632 + 1633 + 1654 = **17**.)
⚠️ 132-col: 1544/1546/1548/1549 gain +4 — verify wrap; each stays well under 132.

### 4b. SKIP: dummy (42) — the 4 SOLVE / 3 CALC_DERIV shadowed members, every use

| lines | member(s) | scope | reason |
|---|---|---|---|
| 53,54,55,56 | SETTLING_VELOCITIES_OUTPUT, EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | SOLVE arg list | dummy |
| 70,73,76,79 | same 4 | SOLVE decls | dummy |
| 156,157,158 | EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | SOLVE (actual→CALC_DERIV @151) | dummy |
| 161 | SETTLING_VELOCITIES_OUTPUT (LHS) | SOLVE | dummy |
| 347,348,349 | EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | SOLVE (actual→CALC_DERIV @342) | dummy |
| 352 | SETTLING_VELOCITIES_OUTPUT (LHS) | SOLVE | dummy |
| 403,404,405 | EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | SOLVE (actual→CALC_DERIV @398) | dummy |
| 725,726,727 | EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | CALC_DERIV arg list | dummy |
| 755,759,763 | EFF_DISSLOVED, EFF_DEPOSITION, DEP_AREA_RATIOS | CALC_DERIV decls | dummy |
| 1235,1239,1252 | EFFECTIVE_DISSLOVED_FRACTIONS | CALC_DERIV body | dummy |
| 1260,1263 | EFF_DEPOSITION, DEP_AREA_RATIOS | CALC_DERIV body | dummy |
| 1266 | EFF_DEPOSITION + DEP_AREA_RATIOS (2 on line) | CALC_DERIV body | dummy |
| 1272 | EFFECTIVE_DEPOSITION_FRACTIONS | CALC_DERIV body | dummy |
| 1279,1284 | EFFECTIVE_DISSLOVED_FRACTIONS | CALC_DERIV body | dummy |
| 1289,1292 | EFF_DEPOSITION, DEP_AREA_RATIOS | CALC_DERIV body | dummy |
| 1295 | EFF_DEPOSITION + DEP_AREA_RATIOS (2 on line) | CALC_DERIV body | dummy |
| 1300 | EFFECTIVE_DEPOSITION_FRACTIONS | CALC_DERIV body | dummy |
| 1537 (RHS) | EFFECTIVE_DISSLOVED_FRACTIONS | CALC_DERIV | dummy (scope-mixed; LHS renamed above) |
| 1538 (RHS) | EFFECTIVE_DEPOSITION_FRACTIONS | CALC_DERIV | dummy (scope-mixed; LHS renamed above) |

### 4c. SKIP: component (3) — PELAGIC_BOXES(i) % DISSOLVED_FRACTIONS

| line | context | `%` position |
|---|---|---|
| 1229 | `…PELAGIC_BOXES(i) % &` / `DISSOLVED_FRACTIONS(j) < 0.0D0` | `%` on prev line 1228 |
| 1233 | `…PELAGIC_BOXES(i) % &` / `DISSOLVED_FRACTIONS(j) * (-1.0D0)` | `%` on prev line 1232 |
| 1241 | `PELAGIC_BOXES(i) % DISSOLVED_FRACTIONS(j)` | `%` same line |

### 4d. SKIP: comment (4)
1582 (`! … columns of FLUXES_TO_WATER_COLUMN.`), 1593 (`! NOT_DEPOSITED_FLUXES`),
1605 (`! SETTLING_RATES`), 1614 (`! FLUXES_TO_WATER_COLUMN`).

look-alikes (not members): 83/144/155/346/402/463/478/692/694/706/707/724/767/824/1315/1325 `PRESCRIBED_SEDIMENT_FLUXES`;
824/1393/1399/1408 `SEDIMENT_FLUXES`; 1546/1566/1571/1590 `bsed%FLUXES_TO/FROM_SEDIMENTS` + `NUM_FLUXES_*`; 35 comment.

**Count: 17 RENAME, 49 SKIP (42 dummy + 3 component + 4 comment).**

---

## 5. ESTAS_II.f90  (program ESTAS_II)

**Insert `use WATER_SEDIMENT_COUPLING, only: wsc` after line 3** (`    use GLOBAL`).

Dealloc block 83–97 (comment lines 85–88 between the two sub-blocks). All 11 → RENAME.

| line | current | RENAME → |
|---|---|---|
| 83 | `deallocate(FLUXES_TO_WATER_COLUMN       )` | `deallocate(wsc%FLUXES_TO_WATER_COLUMN       )` |
| 84 | `deallocate(FLUXES_OUTPUT_TO_WATER_COLUMN)` | `deallocate(wsc%FLUXES_OUTPUT_TO_WATER_COLUMN)` |
| 89 | `deallocate(DISSOLVED_FRACTIONS           )` | `deallocate(wsc%DISSOLVED_FRACTIONS           )` |
| 90 | `deallocate(FRACTION_OF_DEPOSITION        )` | `deallocate(wsc%FRACTION_OF_DEPOSITION        )` |
| 91 | `deallocate(SETTLING_RATES                )` | `deallocate(wsc%SETTLING_RATES                )` |
| 92 | `deallocate(NOT_DEPOSITED_FLUXES          )` | `deallocate(wsc%NOT_DEPOSITED_FLUXES          )` |
| 93 | `deallocate(FLUXES                        )` | `deallocate(wsc%FLUXES                        )` |
| 94 | `deallocate(SETTLING_VELOCITIES_OUTPUT    )` | `deallocate(wsc%SETTLING_VELOCITIES_OUTPUT    )` |
| 95 | `deallocate(EFFECTIVE_DISSLOVED_FRACTIONS )` | `deallocate(wsc%EFFECTIVE_DISSLOVED_FRACTIONS )` |
| 96 | `deallocate(EFFECTIVE_DEPOSITION_FRACTIONS)` | `deallocate(wsc%EFFECTIVE_DEPOSITION_FRACTIONS)` |
| 97 | `deallocate(DEPOSITION_AREA_RATIOS        )` | `deallocate(wsc%DEPOSITION_AREA_RATIOS        )` |

look-alikes (not members): 116 `bsed%FLUXES_TO_SEDIMENTS`, 123 `bsed%FLUXES_FROM_SEDIMENTS`.

**Count: 11 RENAME, 0 SKIP.**

---

## 6. mod_SIMULATE.f90  (module SIMULATE, subroutine RUN_SIMULATION)

**Insert `use WATER_SEDIMENT_COUPLING, only: wsc` after line 3** (`    use GLOBAL`).

No shadows in RUN_SIMULATION (its only dummy is AQUATIC_MODEL_DATA). SOLVE call site = 319–327 (the
only one; passes the 4 shadowed members' GLOBAL values in). Verified: `RUN_SIMULATION` spans 16–847
(the only subroutine), so all 9 sites below are in it; and `grep '::.*MEMBER'` returns **empty** → no
local `::` decl of any of the 6 members → every ref is host-associated GLOBAL (gets `wsc` via the
module-level `use`).

| line | current (trimmed) | RENAME → |
|---|---|---|
| 323 | `SETTLING_VELOCITIES_OUTPUT                 , &` | `wsc%SETTLING_VELOCITIES_OUTPUT                 , &` |
| 324 | `EFFECTIVE_DISSLOVED_FRACTIONS              , &` | `wsc%EFFECTIVE_DISSLOVED_FRACTIONS              , &` |
| 325 | `EFFECTIVE_DEPOSITION_FRACTIONS             , &` | `wsc%EFFECTIVE_DEPOSITION_FRACTIONS             , &` |
| 326 | `DEPOSITION_AREA_RATIOS                     , &` | `wsc%DEPOSITION_AREA_RATIOS                     , &` |
| 587 | `FLUXES_TO_WATER_COLUMN(i,j)), &` | `wsc%FLUXES_TO_WATER_COLUMN(i,j)), &` |
| 588 | `FLUXES_TO_WATER_COLUMN(i, j)` | `wsc%FLUXES_TO_WATER_COLUMN(i, j)` |
| 612 | `FLUXES_TO_WATER_COLUMN(i,j)), &` | `wsc%FLUXES_TO_WATER_COLUMN(i,j)), &` |
| 613 | `FLUXES_TO_WATER_COLUMN(i, j)` | `wsc%FLUXES_TO_WATER_COLUMN(i, j)` |
| 737 | `WTIME, i, FLUXES_OUTPUT_TO_WATER_COLUMN(i,:)` | `WTIME, i, wsc%FLUXES_OUTPUT_TO_WATER_COLUMN(i,:)` |

look-alikes (not members): 746/747 `bsed%FLUXES_FROM_SEDIMENTS`, 750 `bsed%FLUXES_TO_SEDIMENTS`.

**Count: 9 RENAME, 0 SKIP.**

---

## 7. mod_BOTTOM_SEDIMENTS.f90  (module BOTTOM_SEDIMENTS)

**Insert `use WATER_SEDIMENT_COUPLING, only: wsc` after line 15** (`    use GLOBAL`).

`SEDIMENT_TRANSPORT` = 332–358 (**dead code, no callers**). Dummies among the 11: DISSOLVED_FRACTIONS,
FRACTION_OF_DEPOSITION (arg list 333, decls 340/341). Non-dummies (SETTLING_VELOCITIES_OUTPUT,
EFFECTIVE_DISSLOVED_FRACTIONS, EFFECTIVE_DEPOSITION_FRACTIONS) are host-associated GLOBAL → RENAME.

| line | current (trimmed) | action |
|---|---|---|
| 333 | `(SETTLING_VELOCITIES, DISSOLVED_FRACTIONS, FRACTION_OF_DEPOSITION, &` | **SKIP: dummy** (both DISSOLVED_FRACTIONS + FRACTION_OF_DEPOSITION; SETTLING_VELOCITIES not a member) |
| 340 | `… :: DISSOLVED_FRACTIONS` | SKIP: dummy (decl) |
| 341 | `… :: FRACTION_OF_DEPOSITION` | SKIP: dummy (decl) |
| 351 | `SETTLING_VELOCITIES = SETTLING_VELOCITIES_OUTPUT` | **RENAME RHS** → `SETTLING_VELOCITIES = wsc%SETTLING_VELOCITIES_OUTPUT` (LHS not a member) |
| 354 | `DISSOLVED_FRACTIONS = EFFECTIVE_DISSLOVED_FRACTIONS` | **scope-mixed**: LHS SKIP: dummy; **RENAME RHS** → `DISSOLVED_FRACTIONS = wsc%EFFECTIVE_DISSLOVED_FRACTIONS` |
| 357 | `FRACTION_OF_DEPOSITION = EFFECTIVE_DEPOSITION_FRACTIONS` | **scope-mixed**: LHS SKIP: dummy; **RENAME RHS** → `FRACTION_OF_DEPOSITION = wsc%EFFECTIVE_DEPOSITION_FRACTIONS` |

look-alikes (not members): 36/42 `bsed` component decls `FLUXES_TO/FROM_SEDIMENTS`; 424/431 `bsed%FLUXES_*` alloc;
660/670/673 `*_FLUXES_*_FILENAME` reads; 237 comment (`prescribed fluxes/settling`).

**Count: 3 RENAME, 6 SKIP (dummy: 333×2, 340, 341, 354-LHS, 357-LHS).**

---

## Block-range confirmations (for the implementer)

- **GLOBAL deletion:** `mod_GLOBAL.f90:142–156` (inclusive) → breadcrumb. Members at 143,144,147–151,153–156.
- **Allocation block:** `mod_AQUATIC_MODEL.f90` members at 220,221,230–238 (non-member code 222–229 in between).
- **Zero-init:** `mod_AQUATIC_MODEL.f90:660–661`.
- **Dealloc block:** `ESTAS_II.f90` members at 83,84,89–97 (comments 85–88 between).
- **`use` inserts:** AQUATIC_MODEL after 17; SOLVER after 3; ESTAS_II after 3; SIMULATE after 3;
  BOTTOM_SEDIMENTS after 15 (each is the `use GLOBAL` line).

## Implementer note

All line numbers in the tables are **pre-edit** and key off the pristine files. Apply each edit by
**exact-string match** on the "current code" column, not by line number — inserting the `use` line
shifts every line below it. Apply one edit at a time (per §Method: the enumerated list + per-site
review is the primary correctness mechanism; the byte-identical gate and strip-proof are blind to a
dummy-body over-prefix).

## Off-surface (do NOT edit) — verified

- `mod_PELAGIC_BOX.f90:34/102/118` — `DISSOLVED_FRACTIONS` is a PELAGIC_BOX pointer component.
- `sub_READ_PELAGIC_INPUTS.f90:500` — `DISSOLVED_FRACTIONS(PELAGIC_STATE_VAR_NO)` is that component (`%` on prior continuation line 499).
- AQUABC library (`aquabc_II_pelagic_auxillary.f90` etc.) — member names appear only as that library's own
  dummy args / a distinct local `FLUXES(k)` / comments; the library `use GLOBAL, only:` dimension constants.
- `build/*__genmod.f90` — gfortran-generated interface artifacts, not source.
