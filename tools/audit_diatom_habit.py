#!/usr/bin/env python3
"""Size the benthic/tychoplanktonic share of the observed diatom biomass.

Streams the AAA monitoring NDJSON, keeps Fitoplanktonas rows at the CL29-mapped
LTK stations, filters to the two diatom orders, and splits genera into
planktonic (true phytoplankton) vs benthic/tychoplanktonic (periphyton, epipelon,
epiphyton -- material advected/resuspended into the water column) so the model's
DIA_C target can be compared against the pelagic share only.

Usage: diatom_habit.py <ndjson> [year_from] [year_to]
"""
import json
import sys
from collections import defaultdict

NDJSON = sys.argv[1]
Y0 = int(sys.argv[2]) if len(sys.argv) > 2 else 2016
Y1 = int(sys.argv[3]) if len(sys.argv) > 3 else 2022

STATIONS = {"LTK1", "LTK2", "LTK14", "LTK3B", "LTK12", "LTK5", "LTK7B", "LTK10"}
DIATOM_ORDERS = {"eupodiscales", "bacillariales"}

# Genus -> habit. PLANKTONIC = holoplanktonic freshwater/brackish diatoms.
# BENTHIC = periphytic/epipelic/epiphytic genera whose presence in a water
# sample is resuspension or littoral advection (the tychoplankton of s.35).
PLANKTONIC = {
    "aulacoseira", "melosira", "stephanodiscus", "cyclotella", "cyclostephanos",
    "thalassiosira", "actinocyclus", "skeletonema", "chaetoceros", "coscinodiscus",
    "asterionella", "fragilaria", "ulnaria", "acanthoceras", "attheya",
    "discostella", "puncticulata", "cyclotelloides", "centrales", "rhizosolenia",
}
BENTHIC = {
    "navicula", "nitzschia", "gomphonema", "cocconeis", "amphora", "cymbella",
    "surirella", "epithemia", "rhoicosphenia", "diatoma", "meridion", "eunotia",
    "achnanthes", "achnanthidium", "pinnularia", "stauroneis", "caloneis",
    "gyrosigma", "pleurosigma", "bacillaria", "hantzschia", "craticula",
    "encyonema", "reimeria", "planothidium", "staurosira", "staurosirella",
    "pseudostaurosira", "opephora", "tabularia", "ctenophora", "diploneis",
    "sellaphora", "luticola", "tryblionella", "denticula", "amphipleura",
    "cymatopleura", "campylodiscus", "entomoneis", "placoneis", "neidium",
    "rhopalodia", "martyana", "fallacia", "berkeleya", "licmophora",
    "synedra",  # Synedra ulna s.l. -- littoral/epiphytic in this system
}

rows = []
for ln in open(NDJSON):
    if "Fitoplanktonas" not in ln:
        continue
    d = json.loads(ln)
    if d.get("_type", "").split("/")[-1] != "Fitoplanktonas":
        continue
    if d.get("m_vietos_kodas") not in STATIONS:
        continue
    order = str(d.get("individu_klase") or "").strip().lower()
    if order not in DIATOM_ORDERS:
        continue
    date = d.get("data")
    if not date:
        continue
    year, month = int(date[:4]), int(date[5:7])
    if not (Y0 <= year <= Y1):
        continue
    try:
        bm = float(d.get("biomase") or 0.0)
    except (TypeError, ValueError):
        continue
    taxon = str(d.get("taksonas_rusis") or "").strip()
    rows.append((month, order, taxon, bm))

genus_bm = defaultdict(float)
for _, _, taxon, bm in rows:
    genus_bm[taxon.split()[0].lower() if taxon else "(blank)"] += bm

def habit(g):
    if g in PLANKTONIC:
        return "planktonic"
    if g in BENTHIC:
        return "benthic"
    return "unclassified"

print(f"CL29 LTK stations, {Y0}-{Y1}: {len(rows)} diatom rows, "
      f"{sum(b for *_ , b in rows):.1f} total biomass units\n")

print("Genera by biomass (top 25):")
print(f"  {'genus':<20} {'habit':<14} {'biomass':>9} {'share':>7}")
tot = sum(genus_bm.values())
for g, b in sorted(genus_bm.items(), key=lambda x: -x[1])[:25]:
    print(f"  {g:<20} {habit(g):<14} {b:>9.1f} {100*b/tot:>6.1f}%")

unc = sorted(((g, b) for g, b in genus_bm.items() if habit(g) == "unclassified"),
             key=lambda x: -x[1])
print(f"\nUnclassified: {len(unc)} genera, "
      f"{100*sum(b for _, b in unc)/tot:.1f}% of diatom biomass")
for g, b in unc[:12]:
    print(f"    {g:<24} {b:>8.1f} ({100*b/tot:.2f}%)")

print("\nMonthly benthic share of diatom biomass:")
print(f"  {'mon':>4} {'plankt':>9} {'benthic':>9} {'unclass':>9} {'benthic %':>10}")
by_mon = defaultdict(lambda: defaultdict(float))
for month, _, taxon, bm in rows:
    by_mon[month][habit(taxon.split()[0].lower() if taxon else "")] += bm
for m in range(1, 13):
    d = by_mon.get(m)
    if not d:
        continue
    p, b, u = d["planktonic"], d["benthic"], d["unclassified"]
    t = p + b + u
    print(f"  {m:>4} {p:>9.1f} {b:>9.1f} {u:>9.1f} {100*b/t:>9.1f}%")

print("\nAutumn (Oct-Nov) composition, planktonic genera only:")
aut = defaultdict(float)
for month, _, taxon, bm in rows:
    if month in (10, 11):
        g = taxon.split()[0].lower() if taxon else "(blank)"
        if habit(g) == "planktonic":
            aut[g] += bm
t = sum(aut.values())
for g, b in sorted(aut.items(), key=lambda x: -x[1])[:8]:
    print(f"  {g:<20} {b:>7.1f} {100*b/t:>6.1f}%")
print("\nAutumn species detail (top 10 by biomass):")
sp = defaultdict(float)
for month, _, taxon, bm in rows:
    if month in (10, 11) and habit(taxon.split()[0].lower() if taxon else "") == "planktonic":
        sp[taxon] += bm
for s, b in sorted(sp.items(), key=lambda x: -x[1])[:10]:
    print(f"  {s:<42} {b:>7.1f} {100*b/t:>6.1f}%")

print("\nMonthly profile of the main planktonic genera (biomass units):")
gen = ("stephanodiscus", "aulacoseira", "asterionella", "actinocyclus", "skeletonema", "fragilaria")
prof = defaultdict(lambda: defaultdict(float))
for month, _, taxon, bm in rows:
    g = taxon.split()[0].lower() if taxon else ""
    if g in gen:
        prof[g][month] += bm
print("  " + "genus".ljust(16) + "".join(f"{m:>7}" for m in range(2, 12)))
for g in gen:
    print("  " + g.ljust(16) + "".join(f"{prof[g].get(m,0):>7.1f}" for m in range(2, 12)))
print("\n  weighted mean month (thermal preference proxy):")
for g in gen:
    tot_b = sum(prof[g].values())
    if tot_b:
        wm = sum(m*b for m, b in prof[g].items())/tot_b
        print(f"    {g:<16} {wm:>5.1f}   (total {tot_b:.1f})")

print("\nSensitivity: Fragilaria is the swing taxon (many spp. are tychoplanktonic).")
for label, frag_benthic in (("Fragilaria = planktonic (headline)", False),
                            ("Fragilaria = benthic (upper bound)", True)):
    print(f"  {label}:")
    for m in (8, 10, 11):
        p = b = 0.0
        for month, _, taxon, bm in rows:
            if month != m:
                continue
            g = taxon.split()[0].lower() if taxon else ""
            h = habit(g)
            if g == "fragilaria" and frag_benthic:
                h = "benthic"
            if h == "benthic":
                b += bm
            else:
                p += bm
        print(f"     month {m:>2}: benthic {100*b/(p+b):>5.1f}%")
