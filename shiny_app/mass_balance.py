"""
AQUABC Mass Balance Calculator Module

Calculates and tracks mass balance for key elements:
- Nitrogen (N)
- Carbon (C)
- Phosphorus (P)
- Silicon (Si)
"""

import logging
import os
from dataclasses import dataclass

import pandas as pd

logger = logging.getLogger("AQUABC.massbal")

# Default stoichiometric ratios (can be overridden from parameter file)
DEFAULT_STOICHIOMETRY = {
    "DIA_N_TO_C": 0.22,
    "DIA_P_TO_C": 0.024,
    "DIA_Si_TO_C": 0.25,
    "CYN_N_TO_C": 0.22,
    "CYN_P_TO_C": 0.024,
    "FIX_CYN_N_TO_C": 0.22,
    "FIX_CYN_P_TO_C": 0.024,
    "OPA_N_TO_C": 0.22,
    "OPA_P_TO_C": 0.024,
    "ZOO_N_TO_C": 0.22,
    "ZOO_P_TO_C": 0.024,
    "NOST_N_TO_C": 0.22,
    "NOST_P_TO_C": 0.024,
}

# Column mappings for OUTPUT.csv
# Maps element pools to their column names
NITROGEN_POOLS = {
    "inorganic": ["NH4N", "NO3N"],
    "organic_dissolved": ["DON"],
    "particulate": ["DETN"],
    "zooplankton": ["ZOON"],
    # Phytoplankton N calculated from C using ratios
}

CARBON_POOLS = {
    "inorganic": ["DIC"],
    "organic_dissolved": ["DOC"],
    "particulate": ["DETC"],
    "zooplankton": ["ZOOC"],
    "phytoplankton": ["DIAC", "NOFIX_CYNC", "FIX_CYNC", "OPA"],
}

PHOSPHORUS_POOLS = {
    "inorganic": ["PO4P"],
    "organic_dissolved": ["DOP"],
    "particulate": ["DETP"],
    "zooplankton": ["ZOOP"],
    # Phytoplankton P calculated from C using ratios
}

SILICON_POOLS = {
    "dissolved": ["DISSOLVED_SILICA"],
    "particulate": ["PARTICULATE_SILICA"],
    # Diatom Si calculated from DIAC using ratio
}


@dataclass
class MassBalanceResult:
    """Results of mass balance calculation"""
    element: str
    initial_total: float
    final_total: float
    min_total: float
    max_total: float
    mean_total: float
    absolute_change: float
    percent_change: float
    time_series: pd.Series
    pool_breakdown: dict[str, pd.Series]

    def is_conserved(self, tolerance: float = 1.0) -> bool:
        """Check if mass is conserved within tolerance (percent)"""
        return abs(self.percent_change) <= tolerance

    def to_dict(self) -> dict:
        """Convert to dictionary for display"""
        return {
            "element": self.element,
            "initial_total": self.initial_total,
            "final_total": self.final_total,
            "min_total": self.min_total,
            "max_total": self.max_total,
            "mean_total": self.mean_total,
            "absolute_change": self.absolute_change,
            "percent_change": self.percent_change,
            "conserved": self.is_conserved(),
        }


class MassBalanceCalculator:
    """Calculate mass balance for AQUABC model output"""

    def __init__(self, output_csv: str, stoichiometry: dict[str, float] | None = None):
        self.output_csv = output_csv
        self.stoichiometry = stoichiometry or DEFAULT_STOICHIOMETRY.copy()
        self.df: pd.DataFrame | None = None
        self._loaded = False

    def load_data(self, max_rows: int | None = None) -> bool:
        """Load OUTPUT.csv data"""
        if not os.path.exists(self.output_csv):
            logger.error(f"Output file not found: {self.output_csv}")
            return False

        try:
            self.df = pd.read_csv(
                self.output_csv,
                comment='#',
                skip_blank_lines=True,
                nrows=max_rows
            )
            # Clean column names
            self.df.columns = [c.strip() for c in self.df.columns]
            self._loaded = True
            logger.info(f"Loaded {len(self.df)} rows from {os.path.basename(self.output_csv)}")
            return True
        except Exception as e:
            logger.error(f"Error loading output file: {e}")
            return False

    def _get_column(self, name: str) -> pd.Series:
        """Get column by name, return zeros if not found"""
        if self.df is None:
            return pd.Series([0])
        if name in self.df.columns:
            return self.df[name].fillna(0)
        logger.warning(f"Column '{name}' not found in output, using zeros")
        return pd.Series([0] * len(self.df))

    def calculate_nitrogen(self) -> MassBalanceResult:
        """Calculate total nitrogen mass balance"""
        if not self._loaded:
            self.load_data()

        pools = {}

        # Inorganic N
        nh4n = self._get_column("NH4N")
        no3n = self._get_column("NO3N")
        pools["NH4-N"] = nh4n
        pools["NO3-N"] = no3n

        # Dissolved organic N
        don = self._get_column("DON")
        pools["DON"] = don

        # Particulate N (detritus)
        detn = self._get_column("DETN")
        pools["Detritus-N"] = detn

        # Zooplankton N
        zoon = self._get_column("ZOON")
        pools["Zoo-N"] = zoon

        # Phytoplankton N (calculated from C * N:C ratio)
        diac = self._get_column("DIAC")
        dia_n = diac * self.stoichiometry.get("DIA_N_TO_C", 0.22)
        pools["Diatom-N"] = dia_n

        cync = self._get_column("NOFIX_CYNC")
        cyn_n = cync * self.stoichiometry.get("CYN_N_TO_C", 0.22)
        pools["Cyano-N"] = cyn_n

        fix_cync = self._get_column("FIX_CYNC")
        fix_cyn_n = fix_cync * self.stoichiometry.get("FIX_CYN_N_TO_C", 0.22)
        pools["FixCyano-N"] = fix_cyn_n

        opa = self._get_column("OPA")
        opa_n = opa * self.stoichiometry.get("OPA_N_TO_C", 0.22)
        pools["OtherPhyto-N"] = opa_n

        # Total N
        total_n = nh4n + no3n + don + detn + zoon + dia_n + cyn_n + fix_cyn_n + opa_n

        return self._create_result("Nitrogen", total_n, pools)

    def calculate_carbon(self) -> MassBalanceResult:
        """Calculate total carbon mass balance"""
        if not self._loaded:
            self.load_data()

        pools = {}

        # Inorganic C
        dic = self._get_column("DIC")
        pools["DIC"] = dic

        # Dissolved organic C
        doc = self._get_column("DOC")
        pools["DOC"] = doc

        # Particulate C (detritus)
        detc = self._get_column("DETC")
        pools["Detritus-C"] = detc

        # Zooplankton C
        zooc = self._get_column("ZOOC")
        pools["Zoo-C"] = zooc

        # Phytoplankton C
        diac = self._get_column("DIAC")
        pools["Diatom-C"] = diac

        cync = self._get_column("NOFIX_CYNC")
        pools["Cyano-C"] = cync

        fix_cync = self._get_column("FIX_CYNC")
        pools["FixCyano-C"] = fix_cync

        opa = self._get_column("OPA")
        pools["OtherPhyto-C"] = opa

        # Total C
        total_c = dic + doc + detc + zooc + diac + cync + fix_cync + opa

        return self._create_result("Carbon", total_c, pools)

    def calculate_phosphorus(self) -> MassBalanceResult:
        """Calculate total phosphorus mass balance"""
        if not self._loaded:
            self.load_data()

        pools = {}

        # Inorganic P
        po4p = self._get_column("PO4P")
        pools["PO4-P"] = po4p

        # Dissolved organic P
        dop = self._get_column("DOP")
        pools["DOP"] = dop

        # Particulate P (detritus)
        detp = self._get_column("DETP")
        pools["Detritus-P"] = detp

        # Zooplankton P
        zoop = self._get_column("ZOOP")
        pools["Zoo-P"] = zoop

        # Phytoplankton P (calculated from C * P:C ratio)
        diac = self._get_column("DIAC")
        dia_p = diac * self.stoichiometry.get("DIA_P_TO_C", 0.024)
        pools["Diatom-P"] = dia_p

        cync = self._get_column("NOFIX_CYNC")
        cyn_p = cync * self.stoichiometry.get("CYN_P_TO_C", 0.024)
        pools["Cyano-P"] = cyn_p

        fix_cync = self._get_column("FIX_CYNC")
        fix_cyn_p = fix_cync * self.stoichiometry.get("FIX_CYN_P_TO_C", 0.024)
        pools["FixCyano-P"] = fix_cyn_p

        opa = self._get_column("OPA")
        opa_p = opa * self.stoichiometry.get("OPA_P_TO_C", 0.024)
        pools["OtherPhyto-P"] = opa_p

        # Total P
        total_p = po4p + dop + detp + zoop + dia_p + cyn_p + fix_cyn_p + opa_p

        return self._create_result("Phosphorus", total_p, pools)

    def calculate_silicon(self) -> MassBalanceResult:
        """Calculate total silicon mass balance"""
        if not self._loaded:
            self.load_data()

        pools = {}

        # Dissolved Si
        diss_si = self._get_column("DISSOLVED_SILICA")
        pools["Dissolved-Si"] = diss_si

        # Particulate Si
        part_si = self._get_column("PARTICULATE_SILICA")
        pools["Particulate-Si"] = part_si

        # Diatom Si (calculated from DIAC * Si:C ratio)
        diac = self._get_column("DIAC")
        dia_si = diac * self.stoichiometry.get("DIA_Si_TO_C", 0.25)
        pools["Diatom-Si"] = dia_si

        # Total Si
        total_si = diss_si + part_si + dia_si

        return self._create_result("Silicon", total_si, pools)

    def _create_result(self, element: str, total_series: pd.Series,
                       pools: dict[str, pd.Series]) -> MassBalanceResult:
        """Create MassBalanceResult from calculated series"""
        initial = total_series.iloc[0] if len(total_series) > 0 else 0
        final = total_series.iloc[-1] if len(total_series) > 0 else 0

        absolute_change = final - initial
        percent_change = (absolute_change / initial * 100) if initial != 0 else 0

        return MassBalanceResult(
            element=element,
            initial_total=initial,
            final_total=final,
            min_total=total_series.min(),
            max_total=total_series.max(),
            mean_total=total_series.mean(),
            absolute_change=absolute_change,
            percent_change=percent_change,
            time_series=total_series,
            pool_breakdown=pools
        )

    def calculate_all(self) -> dict[str, MassBalanceResult]:
        """Calculate mass balance for all elements"""
        if not self._loaded:
            self.load_data()

        return {
            "Nitrogen": self.calculate_nitrogen(),
            "Carbon": self.calculate_carbon(),
            "Phosphorus": self.calculate_phosphorus(),
            "Silicon": self.calculate_silicon(),
        }

    def get_time_column(self) -> pd.Series:
        """Get TIME column from data"""
        if self.df is None:
            return pd.Series([0])
        return self._get_column("TIME")

    def get_summary_table(self) -> pd.DataFrame:
        """Get summary table of all mass balances"""
        results = self.calculate_all()

        data = []
        for element, result in results.items():
            data.append({
                "Element": element,
                "Initial": f"{result.initial_total:.4f}",
                "Final": f"{result.final_total:.4f}",
                "Min": f"{result.min_total:.4f}",
                "Max": f"{result.max_total:.4f}",
                "Change (%)": f"{result.percent_change:.2f}%",
                "Status": "✓ Conserved" if result.is_conserved() else "⚠ Imbalance"
            })

        return pd.DataFrame(data)


def load_stoichiometry_from_params(param_file: str) -> dict[str, float]:
    """Load stoichiometric ratios from parameter file"""
    stoich = DEFAULT_STOICHIOMETRY.copy()

    if not os.path.exists(param_file):
        logger.warning(f"Parameter file not found: {param_file}, using defaults")
        return stoich

    try:
        with open(param_file) as f:
            for line in f:
                # Look for ratio parameters
                for key in stoich.keys():
                    if key in line:
                        parts = line.split()
                        if len(parts) >= 3:
                            try:
                                stoich[key] = float(parts[2])
                            except ValueError:
                                pass
        logger.info(f"Loaded stoichiometry from {os.path.basename(param_file)}")
    except Exception as e:
        logger.error(f"Error loading stoichiometry: {e}")

    return stoich


# Test function
def test_mass_balance():
    """Test mass balance calculations"""

    script_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.dirname(script_dir)
    output_csv = os.path.join(root_dir, "OUTPUT.csv")
    param_file = os.path.join(root_dir, "INPUTS", "WCONST_04.txt")

    print(f"Testing mass balance with: {output_csv}")
    print("=" * 60)

    if not os.path.exists(output_csv):
        print(f"Output file not found: {output_csv}")
        return

    # Load stoichiometry from parameters
    stoich = load_stoichiometry_from_params(param_file)
    print("Stoichiometric ratios:")
    for key, val in list(stoich.items())[:5]:
        print(f"  {key}: {val}")
    print()

    # Calculate mass balances
    calc = MassBalanceCalculator(output_csv, stoich)
    calc.load_data()

    print(f"Data loaded: {len(calc.df)} rows")
    print()

    # Get summary
    summary = calc.get_summary_table()
    print("Mass Balance Summary:")
    print(summary.to_string(index=False))
    print()

    # Detailed results
    results = calc.calculate_all()
    for element, result in results.items():
        print(f"\n{element} Pools:")
        for pool_name, pool_series in result.pool_breakdown.items():
            initial = pool_series.iloc[0] if len(pool_series) > 0 else 0
            final = pool_series.iloc[-1] if len(pool_series) > 0 else 0
            print(f"  {pool_name:15s}: {initial:10.4f} -> {final:10.4f}")

    print("\nTest complete!")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    test_mass_balance()
