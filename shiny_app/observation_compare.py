"""
AQUABC Observation Comparison Module

Compares model output with observed/measured data.
Calculates statistical metrics for model validation.
"""

import logging
import os
from dataclasses import dataclass

import numpy as np
import pandas as pd

logger = logging.getLogger("AQUABC.obs")


@dataclass
class ComparisonMetrics:
    """Statistical metrics for model-observation comparison"""
    variable: str
    n_points: int
    obs_mean: float
    model_mean: float
    obs_std: float
    model_std: float
    bias: float  # Mean error (model - obs)
    mae: float   # Mean Absolute Error
    rmse: float  # Root Mean Square Error
    nrmse: float  # Normalized RMSE (% of obs range)
    r_squared: float  # Coefficient of determination
    correlation: float  # Pearson correlation
    skill_score: float  # Willmott skill score (0-1)

    def get_rating(self) -> str:
        """Get qualitative rating based on metrics"""
        if self.r_squared >= 0.8 and self.nrmse <= 20:
            return "Excellent"
        elif self.r_squared >= 0.6 and self.nrmse <= 30:
            return "Good"
        elif self.r_squared >= 0.4 and self.nrmse <= 50:
            return "Fair"
        else:
            return "Poor"

    def to_dict(self) -> dict:
        """Convert to dictionary"""
        return {
            "variable": self.variable,
            "n_points": self.n_points,
            "obs_mean": self.obs_mean,
            "model_mean": self.model_mean,
            "bias": self.bias,
            "mae": self.mae,
            "rmse": self.rmse,
            "nrmse": self.nrmse,
            "r_squared": self.r_squared,
            "correlation": self.correlation,
            "skill_score": self.skill_score,
            "rating": self.get_rating(),
        }


class ObservationData:
    """Container for observation data"""

    def __init__(self, filepath: str | None = None):
        self.filepath = filepath
        self.df: pd.DataFrame | None = None
        self.time_column: str = "TIME"
        self.variables: list[str] = []
        self._loaded = False

    def load_csv(self, filepath: str) -> tuple[bool, str]:
        """
        Load observation data from CSV file.
        Expected format: TIME column + variable columns
        """
        if not os.path.exists(filepath):
            return False, f"File not found: {filepath}"

        try:
            self.df = pd.read_csv(filepath, comment='#', skip_blank_lines=True)
            self.df.columns = [c.strip() for c in self.df.columns]

            # Find time column (case-insensitive)
            time_cols = [c for c in self.df.columns if c.upper() in ['TIME', 'DATE', 'DATETIME', 'T']]
            if time_cols:
                self.time_column = time_cols[0]
            else:
                # Assume first column is time
                self.time_column = self.df.columns[0]
                logger.warning(f"No TIME column found, using '{self.time_column}'")

            # Get variable columns (all except time)
            self.variables = [c for c in self.df.columns if c != self.time_column]

            self.filepath = filepath
            self._loaded = True
            logger.info(f"Loaded {len(self.df)} observations with {len(self.variables)} variables")
            return True, f"Loaded {len(self.df)} observations"

        except Exception as e:
            logger.error(f"Error loading observation file: {e}")
            return False, f"Error: {e}"

    def load_from_dataframe(self, df: pd.DataFrame, time_column: str = "TIME") -> tuple[bool, str]:
        """Load observation data from pandas DataFrame"""
        try:
            self.df = df.copy()
            self.df.columns = [c.strip() for c in self.df.columns]
            self.time_column = time_column
            self.variables = [c for c in self.df.columns if c != self.time_column]
            self._loaded = True
            return True, f"Loaded {len(self.df)} observations"
        except Exception as e:
            return False, f"Error: {e}"

    def get_time_range(self) -> tuple[float, float]:
        """Get time range of observations"""
        if not self._loaded or self.df is None:
            return (0, 0)
        time_col = self.df[self.time_column]
        return (time_col.min(), time_col.max())

    def get_variable_data(self, variable: str) -> pd.DataFrame | None:
        """Get time and variable data as DataFrame"""
        if not self._loaded or self.df is None:
            return None
        if variable not in self.variables:
            return None
        return self.df[[self.time_column, variable]].dropna()


class ModelObservationComparison:
    """Compare model output with observations"""

    def __init__(self, model_csv: str, obs_data: ObservationData):
        self.model_csv = model_csv
        self.obs_data = obs_data
        self.model_df: pd.DataFrame | None = None
        self._loaded = False

    def load_model_data(self) -> bool:
        """Load model output data"""
        if not os.path.exists(self.model_csv):
            logger.error(f"Model output not found: {self.model_csv}")
            return False

        try:
            self.model_df = pd.read_csv(
                self.model_csv,
                comment='#',
                skip_blank_lines=True
            )
            self.model_df.columns = [c.strip() for c in self.model_df.columns]
            self._loaded = True
            logger.info(f"Loaded model output: {len(self.model_df)} rows")
            return True
        except Exception as e:
            logger.error(f"Error loading model output: {e}")
            return False

    def get_matching_variables(self) -> list[str]:
        """Get variables that exist in both model and observations"""
        if not self._loaded or self.model_df is None:
            return []

        model_vars = set(self.model_df.columns)
        obs_vars = set(self.obs_data.variables)

        # Try case-insensitive matching
        matching = []
        for obs_var in obs_vars:
            if obs_var in model_vars:
                matching.append(obs_var)
            else:
                # Try uppercase
                obs_upper = obs_var.upper()
                for model_var in model_vars:
                    if model_var.upper() == obs_upper:
                        matching.append((obs_var, model_var))
                        break

        return matching

    def interpolate_model_to_obs_times(self, variable: str, obs_times: pd.Series) -> pd.Series:
        """Interpolate model output to observation times"""
        if self.model_df is None:
            return pd.Series([np.nan] * len(obs_times))

        # Get model time column
        model_time_col = self.model_df.columns[0]  # Assume first column is TIME
        model_times = self.model_df[model_time_col].values
        model_values = self.model_df[variable].values

        # Interpolate
        interpolated = np.interp(
            obs_times.values,
            model_times,
            model_values,
            left=np.nan,
            right=np.nan
        )

        return pd.Series(interpolated, index=obs_times.index)

    def calculate_metrics(self, variable: str) -> ComparisonMetrics | None:
        """Calculate comparison metrics for a variable"""
        if not self._loaded:
            self.load_model_data()

        if self.model_df is None or not self.obs_data._loaded:
            return None

        # Get observation data
        obs_df = self.obs_data.get_variable_data(variable)
        if obs_df is None or len(obs_df) == 0:
            logger.warning(f"No observation data for {variable}")
            return None

        obs_times = obs_df[self.obs_data.time_column]
        obs_values = obs_df[variable].values

        # Find matching model variable (case-insensitive)
        model_var = None
        for col in self.model_df.columns:
            if col.upper() == variable.upper():
                model_var = col
                break

        if model_var is None:
            logger.warning(f"Variable {variable} not found in model output")
            return None

        # Interpolate model to observation times
        model_values = self.interpolate_model_to_obs_times(model_var, obs_times).values

        # Remove NaN pairs
        mask = ~(np.isnan(obs_values) | np.isnan(model_values))
        obs_clean = obs_values[mask]
        model_clean = model_values[mask]

        if len(obs_clean) < 2:
            logger.warning(f"Not enough valid data points for {variable}")
            return None

        # Calculate statistics
        n = len(obs_clean)
        obs_mean = np.mean(obs_clean)
        model_mean = np.mean(model_clean)
        obs_std = np.std(obs_clean)
        model_std = np.std(model_clean)

        # Bias (mean error)
        errors = model_clean - obs_clean
        bias = np.mean(errors)

        # MAE
        mae = np.mean(np.abs(errors))

        # RMSE
        rmse = np.sqrt(np.mean(errors ** 2))

        # Normalized RMSE (% of observation range)
        obs_range = np.max(obs_clean) - np.min(obs_clean)
        nrmse = (rmse / obs_range * 100) if obs_range > 0 else np.inf

        # Correlation
        if obs_std > 0 and model_std > 0:
            correlation = np.corrcoef(obs_clean, model_clean)[0, 1]
        else:
            correlation = 0.0

        # R-squared
        ss_res = np.sum(errors ** 2)
        ss_tot = np.sum((obs_clean - obs_mean) ** 2)
        r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0.0
        r_squared = max(0, r_squared)  # Clamp to non-negative

        # Willmott skill score
        # d = 1 - sum((P-O)^2) / sum((|P-O_mean| + |O-O_mean|)^2)
        numerator = np.sum(errors ** 2)
        denominator = np.sum((np.abs(model_clean - obs_mean) + np.abs(obs_clean - obs_mean)) ** 2)
        skill_score = 1 - (numerator / denominator) if denominator > 0 else 0.0
        skill_score = max(0, min(1, skill_score))  # Clamp to [0, 1]

        return ComparisonMetrics(
            variable=variable,
            n_points=n,
            obs_mean=obs_mean,
            model_mean=model_mean,
            obs_std=obs_std,
            model_std=model_std,
            bias=bias,
            mae=mae,
            rmse=rmse,
            nrmse=nrmse,
            r_squared=r_squared,
            correlation=correlation,
            skill_score=skill_score
        )

    def calculate_all_metrics(self) -> dict[str, ComparisonMetrics]:
        """Calculate metrics for all matching variables"""
        results = {}
        matching = self.get_matching_variables()

        for var in matching:
            if isinstance(var, tuple):
                obs_var, model_var = var
            else:
                obs_var = var

            metrics = self.calculate_metrics(obs_var)
            if metrics:
                results[obs_var] = metrics

        return results

    def get_comparison_data(self, variable: str) -> pd.DataFrame | None:
        """Get paired observation-model data for plotting"""
        if not self._loaded:
            self.load_model_data()

        if self.model_df is None or not self.obs_data._loaded:
            return None

        # Get observation data
        obs_df = self.obs_data.get_variable_data(variable)
        if obs_df is None:
            return None

        obs_times = obs_df[self.obs_data.time_column]
        obs_values = obs_df[variable]

        # Find matching model variable
        model_var = None
        for col in self.model_df.columns:
            if col.upper() == variable.upper():
                model_var = col
                break

        if model_var is None:
            return None

        # Interpolate model to observation times
        model_values = self.interpolate_model_to_obs_times(model_var, obs_times)

        result = pd.DataFrame({
            'Time': obs_times.values,
            'Observed': obs_values.values,
            'Modeled': model_values.values
        })

        return result.dropna()

    def get_summary_table(self) -> pd.DataFrame:
        """Get summary table of all comparisons"""
        metrics = self.calculate_all_metrics()

        if not metrics:
            return pd.DataFrame({"Message": ["No matching variables found"]})

        data = []
        for var, m in metrics.items():
            data.append({
                "Variable": var,
                "N": m.n_points,
                "Obs Mean": f"{m.obs_mean:.4f}",
                "Model Mean": f"{m.model_mean:.4f}",
                "Bias": f"{m.bias:+.4f}",
                "RMSE": f"{m.rmse:.4f}",
                "R²": f"{m.r_squared:.3f}",
                "Rating": m.get_rating()
            })

        return pd.DataFrame(data)


def create_sample_observations(output_csv: str, noise_level: float = 0.1,
                               sample_fraction: float = 0.1) -> pd.DataFrame:
    """
    Create sample observation data from model output (for testing).
    Adds random noise to simulate measurement error.
    """
    if not os.path.exists(output_csv):
        return pd.DataFrame()

    model_df = pd.read_csv(output_csv, comment='#', skip_blank_lines=True)
    model_df.columns = [c.strip() for c in model_df.columns]

    # Sample rows
    n_samples = max(10, int(len(model_df) * sample_fraction))
    indices = np.sort(np.random.choice(len(model_df), n_samples, replace=False))
    sample_df = model_df.iloc[indices].copy()

    # Add noise to numeric columns (except TIME)
    for col in sample_df.columns[1:]:
        if sample_df[col].dtype in [np.float64, np.int64]:
            noise = np.random.normal(0, noise_level * sample_df[col].std(), len(sample_df))
            sample_df[col] = sample_df[col] + noise

    return sample_df


# Test function
def test_observation_compare():
    """Test observation comparison"""
    script_dir = os.path.dirname(os.path.realpath(__file__))
    root_dir = os.path.dirname(script_dir)
    output_csv = os.path.join(root_dir, "OUTPUT.csv")

    print(f"Testing observation comparison with: {output_csv}")
    print("=" * 60)

    if not os.path.exists(output_csv):
        print(f"Output file not found: {output_csv}")
        return

    # Create sample observations
    print("Creating sample observations (10% of data with 10% noise)...")
    sample_obs = create_sample_observations(output_csv, noise_level=0.1, sample_fraction=0.1)
    print(f"Sample observations: {len(sample_obs)} rows")
    print()

    # Load observations
    obs_data = ObservationData()
    success, msg = obs_data.load_from_dataframe(sample_obs)
    print(f"Load status: {msg}")
    print(f"Variables: {obs_data.variables[:5]}...")
    print()

    # Compare
    comparison = ModelObservationComparison(output_csv, obs_data)
    comparison.load_model_data()

    print("Matching variables:")
    matching = comparison.get_matching_variables()
    print(f"  {matching[:5]}...")
    print()

    # Calculate metrics for first variable
    if matching:
        var = matching[0] if not isinstance(matching[0], tuple) else matching[0][0]
        metrics = comparison.calculate_metrics(var)
        if metrics:
            print(f"Metrics for {var}:")
            for key, val in metrics.to_dict().items():
                print(f"  {key}: {val}")
            print()

    # Summary table
    print("Summary Table:")
    summary = comparison.get_summary_table()
    print(summary.head(10).to_string(index=False))

    print("\nTest complete!")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    test_observation_compare()
