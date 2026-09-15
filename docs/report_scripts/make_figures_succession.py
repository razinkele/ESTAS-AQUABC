#!/usr/bin/env python3
"""Observed phytoplankton succession from species-level field data, against the model.

The four-group model aggregates over an assemblage the monitoring programme
resolves to species. This figure shows what that aggregation discards, and where
the modelled succession departs from the observed one.

  succession.png   (a) observed monthly climatology by taxonomic class, the
                       field record at its native resolution;
                   (b) the same collapsed to the model's four groups, against
                       the modelled climatology for the same boxes and months.

Source is the AAA ``juros_mariu_monitoringas`` NDJSON (``Fitoplanktonas`` rows)
at the CL29-mapped LTK lagoon stations. Class -> group mapping and the null-date
recovery are IMPORTED from tools/ingest_km_plankton.py rather than restated --
a second mapping would be free to drift from the one that produced the scored
observations. Model output is read with the validator's own loader.

Wet biomass is converted to carbon with the same per-group ratios the ingester
uses, so panel (b)'s two series are in the same units. Panel (a) stays in wet
biomass: the ratios are per-group, so applying them below group level would
invent precision the 2015 campaign does not support.

Writes docs/report_scripts/cl29_succession_observed.csv alongside the figure so
every number in the panel traces to a committed file.

Env: CL29_FIG_OUT overrides the model output directory (default OUTPUTS_CL29);
CL29_JSONL overrides the NDJSON path.
"""
import collections
import csv
import os
import statistics as st
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
REPO = os.path.dirname(HERE) if os.path.basename(HERE) == "report_scripts" else HERE
REPO = os.path.dirname(REPO)          # .../aquabc
sys.path.insert(0, os.path.join(REPO, "tools"))

import ingest_km_plankton as km  # noqa: E402  (class_to_group, loaders, ratios)
import validate_cl29_vs_epa as val  # noqa: E402  (load_box_output, box_number)

FIG_DIR = os.path.join(HERE, "figures")
os.makedirs(FIG_DIR, exist_ok=True)    # figures/ is git-ignored: absent in a fresh checkout

JSONL = os.environ.get(
    "CL29_JSONL", os.path.join(os.path.dirname(REPO), "curonian", "DATA", "JTD", "monitoringasjsonl"))
OUT_DIR = os.environ.get("CL29_FIG_OUT", os.path.join(REPO, "OUTPUTS_CL29"))
STATION_MAP = os.path.join(REPO, "tools", "epa_station_to_box.csv")
BASE_YEAR = 2012
YEARS = range(2016, 2023)              # NDJSON starts 2016; hindcast ends 2022

GROUPS = ["DIA", "CYN", "FIX", "OPA"]
GROUP_LABEL = {"DIA": "Diatoms", "CYN": "Non-fixing cyanobacteria",
               "FIX": "Heterocystous (fixing) cyanobacteria", "OPA": "Other algae"}
# The model sums two pools for the fixer; the observations cannot separate them.
MODEL_COL = {"DIA": "DIA_C", "CYN": "CYN_C", "FIX": "FIX_TOT_C", "OPA": "OPA_C"}
GROUP_COLOR = {"DIA": "#4C72B0", "CYN": "#55A868", "FIX": "#C44E52", "OPA": "#8172B2"}
MONTHS = list(range(1, 13))
MONTH_LABEL = "JFMAMJJASOND"


def mapped_stations():
    """LTK station code -> CL29 box, from the mapping the validator scores against."""
    out = {}
    with open(STATION_MAP, newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(r for r in fh if not r.startswith("#")):
            st_code = (row.get("station") or "").strip()
            box = (row.get("box") or "").strip()
            if st_code and box.isdigit():
                out[st_code] = int(box)
                out["LTK" + st_code.lstrip("LTK")] = int(box)
    return out


def observed():
    """-> (by_class, by_group, n_rows, n_samples_by_month).

    by_class: {class: {month: mean wet biomass per sample}}
    by_group: {group: {month: mean mg C/L per sample}}
    Averaging is per SAMPLE first (a sample is one reg_nr+depth at one station),
    so a month with many species rows does not outweigh one with few.
    """
    fito, _zoo = km.load_jsonl_plankton(JSONL)   # LTK-filtered, dates already recovered
    rows = fito.to_dict("records")
    stations = mapped_stations()
    ratios = km.DEFAULT_RATIOS

    # sample -> month, and sample -> {class: biomass}, {group: carbon}
    smp_month, smp_class, smp_group = {}, collections.defaultdict(dict), collections.defaultdict(dict)
    kept = 0
    for r in rows:
        day, stn = r.get("date"), str(r.get("station") or "").strip()
        if day is None or day.year not in YEARS or stn not in stations:
            continue
        bio = r.get("biomass")
        try:
            bio = float(bio)
        except (TypeError, ValueError):
            continue
        if bio <= 0:
            continue
        key = (stn, r.get("sample"))
        smp_month[key] = day.month
        cls = str(r.get("cls") or "unclassified").strip() or "unclassified"
        smp_class[key][cls] = smp_class[key].get(cls, 0.0) + bio
        g = km.class_to_group(r.get("cls"), r.get("taxon"))
        smp_group[key][g] = smp_group[key].get(g, 0.0) + bio * ratios.get(g, 0.15)
        kept += 1

    def climatology(per_sample):
        acc = collections.defaultdict(lambda: collections.defaultdict(list))
        for key, d in per_sample.items():
            m = smp_month[key]
            for name, v in d.items():
                acc[name][m].append(v)
        return {name: {m: st.fmean(vs) for m, vs in months.items()} for name, months in acc.items()}

    # Months with no sample at all are NOT zero-biomass months -- the species programme
    # does not sample December or January here. Reporting them as 0 would manufacture an
    # observation, the very artefact class this paper is about, so they stay absent.
    n_samples = collections.Counter(smp_month.values())
    return climatology(smp_class), climatology(smp_group), kept, n_samples


def modelled():
    """{group: {month: mean mg C/L}} over the mapped boxes, same years."""
    boxes = set(mapped_stations().values())
    acc = collections.defaultdict(lambda: collections.defaultdict(list))
    import glob
    for path in sorted(glob.glob(os.path.join(OUT_DIR, "PELAGIC_BOX_*.out"))):
        if val.box_number(path) not in boxes:
            continue
        df = val.load_box_output(path, BASE_YEAR)
        for g in GROUPS:
            col = MODEL_COL[g]
            if col not in df.columns:
                continue
            sub = df[[c for c in ("date", col) if c in df.columns]].dropna()
            for day, v in zip(sub["date"], sub[col]):
                if day.year in YEARS:
                    acc[g][day.month].append(float(v))
    return {g: {m: st.fmean(vs) for m, vs in months.items()} for g, months in acc.items()}


def main():
    by_class, by_group, n, n_samples = observed()
    sampled = {m for m in MONTHS if n_samples.get(m, 0) > 0}
    unsampled = [m for m in MONTHS if m not in sampled]
    mod = modelled()

    # Panel (a): the classes that actually carry the assemblage.
    totals = {c: sum(m.values()) for c, m in by_class.items()}
    top = [c for c, _ in sorted(totals.items(), key=lambda kv: -kv[1])[:8]]

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(9.5, 8.2))

    months_s = [m for m in MONTHS if m in sampled]
    bottom = [0.0] * len(months_s)
    cmap = plt.get_cmap("tab10")
    for i, cls in enumerate(top):
        vals = [by_class[cls].get(m, 0.0) for m in months_s]
        ax1.bar(months_s, vals, bottom=bottom, label=cls, color=cmap(i % 10), width=0.82)
        bottom = [b + v for b, v in zip(bottom, vals)]
    ax1.set_title("(a) Observed succession by taxonomic class — species-level monitoring, "
                  f"{min(YEARS)}–{max(YEARS)}", fontsize=10, loc="left")
    ax1.set_ylabel("wet biomass (mg L$^{-1}$)")
    ax1.set_xticks(MONTHS)
    ax1.set_xticklabels(list(MONTH_LABEL))
    ax1.legend(fontsize=7, ncol=2, frameon=False)

    for g in GROUPS:
        o = [by_group.get(g, {}).get(m, 0.0) if m in sampled else float("nan") for m in MONTHS]
        mm = [mod.get(g, {}).get(m, float("nan")) for m in MONTHS]
        ax2.plot(MONTHS, o, "-o", ms=3.5, color=GROUP_COLOR[g], label=f"{GROUP_LABEL[g]} — observed")
        ax2.plot(MONTHS, mm, "--", color=GROUP_COLOR[g], alpha=0.85,
                 label=f"{GROUP_LABEL[g]} — modelled")
    ax2.set_title("(b) The same record collapsed to the model's four groups, against the "
                  "modelled climatology", fontsize=10, loc="left")
    ax2.set_ylabel("carbon (mg C L$^{-1}$)")
    ax2.set_xticks(MONTHS)
    ax2.set_xticklabels(list(MONTH_LABEL))
    ax2.legend(fontsize=7, ncol=2, frameon=False)

    for ax in (ax1, ax2):
        for m in unsampled:
            ax.axvspan(m - 0.5, m + 0.5, color="0.88", zorder=0)
        if unsampled:
            ax.text(unsampled[0] if unsampled[0] > 6 else unsampled[-1], ax.get_ylim()[1] * 0.92,
                    "not sampled", fontsize=7, color="0.35", ha="center", va="top", rotation=90)

    fig.tight_layout()
    out_png = os.path.join(FIG_DIR, "succession.png")
    fig.savefig(out_png, dpi=200)
    plt.close(fig)

    csv_path = os.path.join(HERE, "cl29_succession_observed.csv")
    with open(csv_path, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["level", "name", "month", "value", "units", "n_samples"])
        for cls in sorted(by_class):
            for m in MONTHS:
                if m in by_class[cls]:
                    w.writerow(["class", cls, m, f"{by_class[cls][m]:.6g}", "mg/L wet", n_samples.get(m, 0)])
        for g in GROUPS:
            for m in MONTHS:
                if m in by_group.get(g, {}):
                    w.writerow(["group_obs", g, m, f"{by_group[g][m]:.6g}", "mg C/L", n_samples.get(m, 0)])
                if m in mod.get(g, {}):
                    w.writerow(["group_model", g, m, f"{mod[g][m]:.6g}", "mg C/L", ""])

    print(f"{out_png}\n{csv_path}")
    print(f"species rows kept {n} (mapped LTK stations, {min(YEARS)}-{max(YEARS)})")
    print(f"classes {len(by_class)}; plotted top {len(top)}")
    print(f"months sampled {sorted(sampled)}; unsampled (shown as gaps) {unsampled}")


if __name__ == "__main__":
    main()
