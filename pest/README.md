# CL29 PEST++ calibration

Calibrates the 29-box CL29 model against the ingested Kuršių marios (KM) observations,
targeting the biases in [`../docs/CL29_KM_2022-2023_Validation.md`](../docs/CL29_KM_2022-2023_Validation.md)
(PO4/Si/Chl-a over-prediction, NO3 under-prediction).

Design: [`../docs/superpowers/specs/2026-07-21-cl29-pest-calibration-design.md`](../docs/superpowers/specs/2026-07-21-cl29-pest-calibration-design.md).

## Files

| File | Role |
|---|---|
| `build_pest.py` | Generates the PEST files from the parameter spec + `km_observations_tidy.csv` |
| `forward_run.py` | The PEST forward model: run CL29, score at the obs points, write `model_obs.out` |
| `km_observations_tidy.csv` | Calibration observations (from `tools/ingest_km_observations.py`) |
| `wconst_04.tpl` | Template → `INPUTS_CL29/WCONST_04.txt` (the calibrated parameters) |
| `model_obs.ins` | Instruction file → `model_obs.out` |
| `cl29.pst` | PEST control file (configured for `pestpp-ies`) |

## Regenerate

```sh
python tools/ingest_km_observations.py --out-dir /tmp/km && cp /tmp/km/km_observations_tidy.csv pest/
python pest/build_pest.py            # -> wconst_04.tpl, model_obs.ins, cl29.pst
```

Re-target the calibration by editing `PARAMS` (parameter set / bounds / transform) in
`build_pest.py`; obs weighting is `1 / mean(|value|)` per variable group (edit `obs_weights`).

## Run the calibration (your hardware)

A single CL29 forward run is **~9 minutes**, so this is a compute job — use the **iterative
ensemble smoother** `pestpp-ies` (no Jacobian; parallelizes over an ensemble):

```sh
# serial (slow — one ~9-min run per ensemble member per iteration):
pestpp-ies pest/cl29.pst

# parallel (recommended): a master + N workers, each in its own copy of the run dir
pestpp-ies pest/cl29.pst /h :4000                     # master (port 4000)
for i in $(seq 1 8); do
  ( cp -r . ../worker_$i && cd ../worker_$i && pestpp-ies pest/cl29.pst /h localhost:4000 ) &
done
```

Each worker directory needs `ESTAS_II`, `INPUT_CL29.txt`, `INPUTS_CL29/`, `pest/` and
`pest/km_observations_tidy.csv` (a full repo copy works). `++ies_num_reals(50)` and
`noptmax=3` are set in `cl29.pst` — raise `num_reals` for a better posterior, lower it to
cut cost. Results: `cl29.N.par.csv` / `cl29.N.obs.csv` (ensembles) and `cl29.N.phi.*`.

## Notes

- The calibration window is the model window (day ≤ `SIMULATION_END`, i.e. the KM **2022**
  obs); the 2023 obs need the climatological-forcing run and are excluded here.
- Parameters are `WCONST_04.txt` constants read by index, so the templated line stays
  parse-compatible with `READ_MODEL_CONSTANTS`.
- Scoring reuses `tools/validate_cl29_vs_epa.py`, so PEST's Φ and the validation report use
  the same modelled-vs-observed interpolation.
