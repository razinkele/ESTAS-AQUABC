#!/usr/bin/env python3
"""Build the CL29 ice-cover forcing from the CMEMS Baltic physics reanalysis.

The shipped INPUTS_CL29/ICE_COVER.txt is a two-row all-zero placeholder, so the
modelled lagoon never freezes (doc s.44). This fetches daily sea-ice area
fraction (`siconc`) over the Curonian Lagoon and writes the lagoon-mean series in
the ESTAS forcing format. All 29 boxes read the same series (TS 14, var 1).

Usage: fetch_ice_cover.py [out.txt] [start_year] [end_year]
"""
import sys
import datetime as dt

import numpy as np

OUT = sys.argv[1] if len(sys.argv) > 1 else "ICE_COVER.txt"
Y0 = int(sys.argv[2]) if len(sys.argv) > 2 else 2012
Y1 = int(sys.argv[3]) if len(sys.argv) > 3 else 2022

DATASET = "cmems_mod_bal_phy_my_P1D-m"
# Lagoon interior: avoids the Baltic side of the Curonian Spit so the mean is
# the lagoon's own ice, not the open sea's.
LON = (20.95, 21.30)
LAT = (55.00, 55.65)
BASE = dt.date(Y0, 1, 1)


def main():
    import copernicusmarine as cm

    ds = cm.open_dataset(
        dataset_id=DATASET,
        minimum_longitude=LON[0], maximum_longitude=LON[1],
        minimum_latitude=LAT[0], maximum_latitude=LAT[1],
        start_datetime=f"{Y0}-01-01", end_datetime=f"{Y1}-12-31",
        variables=["siconc"],
    )
    si = ds["siconc"]
    # lagoon mean over wet cells; NaN (land) excluded
    mean = si.mean(dim=("latitude", "longitude"), skipna=True).values
    times = si["time"].values
    days = np.array([(np.datetime64(t, "D").astype(dt.date) - BASE).days
                     for t in times], dtype=float)
    vals = np.clip(np.nan_to_num(mean, nan=0.0), 0.0, 1.0)

    order = np.argsort(days)
    days, vals = days[order], vals[order]

    with open(OUT, "w") as f:
        f.write("# ICE_COVER  (CMEMS %s, lagoon mean siconc, lon %.2f-%.2f lat %.2f-%.2f)\n"
                % (DATASET, LON[0], LON[1], LAT[0], LAT[1]))
        f.write("# DATA_SIZE\n%d\n" % len(days))
        f.write("# NUMBER_OF_VARIABLES\n1\n")
        f.write("# SCALE FACTORS\n#\n          1.00000000\n")
        f.write("# UNIT CONVERSION FACTORS\n#\n          1.00000000\n")
        f.write("# INTERPOLATE (1=yes)\n1\n")
        f.write("# TIME AND VALUES\n")
        for d, v in zip(days, vals):
            f.write("%f %f\n" % (d, v))

    iced = vals > 0.10
    print(f"wrote {OUT}: {len(days)} daily records, day {days[0]:.0f}..{days[-1]:.0f}")
    print(f"  ice fraction  max {vals.max():.3f}  mean {vals.mean():.4f}")
    print(f"  days with >10% cover: {int(iced.sum())} "
          f"({iced.sum()/max(1,(Y1-Y0+1)):.0f} per year)")
    for y in range(Y0, Y1 + 1):
        lo = (dt.date(y, 1, 1) - BASE).days
        hi = (dt.date(y, 12, 31) - BASE).days
        sel = (days >= lo) & (days <= hi)
        if sel.any():
            print(f"    {y}: {int((vals[sel] > 0.10).sum()):>3} ice days, "
                  f"max {vals[sel].max():.2f}")


if __name__ == "__main__":
    main()
