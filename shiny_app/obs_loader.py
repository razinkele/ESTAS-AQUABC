"""
AQUABC Observation File Loader Module

Parses observation data files from the OBSERVATIONS directory.
Supports:
  - .dates files: Whitespace-separated with variable index header
  - .xlsx files: Excel files with various observation formats
  - .csv files: Standard CSV observations

The module provides:
  - File discovery in OBSERVATIONS directory
  - Automatic format detection and parsing
  - Preview of available measurements per file
  - Mapping to AQUABC model state variables
"""

import os
import re
import logging
from datetime import datetime, timedelta
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Any
from pathlib import Path

import pandas as pd
import numpy as np

logger = logging.getLogger("AQUABC.obs_loader")

# State variable index to name mapping (from AQUABC model)
# Index corresponds to column number in .dates files
STATE_VARIABLE_INDEX = {
    1: {"name": "NH4_N", "description": "Ammonium Nitrogen", "units": "mg N/L"},
    2: {"name": "NO3_N", "description": "Nitrate Nitrogen", "units": "mg N/L"},
    3: {"name": "PO4_P", "description": "Orthophosphate Phosphorus", "units": "mg P/L"},
    4: {"name": "DISS_OXYGEN", "description": "Dissolved Oxygen", "units": "mg O₂/L"},
    5: {"name": "DIA_C", "description": "Diatoms Carbon", "units": "mg C/L"},
    6: {"name": "ZOO_C", "description": "Zooplankton Carbon", "units": "mg C/L"},
    7: {"name": "ZOO_N", "description": "Zooplankton Nitrogen", "units": "mg N/L"},
    8: {"name": "ZOO_P", "description": "Zooplankton Phosphorus", "units": "mg P/L"},
    9: {"name": "DET_PART_ORG_C", "description": "Detritus POC", "units": "mg C/L"},
    10: {"name": "DET_PART_ORG_N", "description": "Detritus PON", "units": "mg N/L"},
    11: {"name": "DET_PART_ORG_P", "description": "Detritus POP", "units": "mg P/L"},
    12: {"name": "DISS_ORG_C", "description": "Dissolved Organic Carbon", "units": "mg C/L"},
    13: {"name": "DISS_ORG_N", "description": "Dissolved Organic Nitrogen", "units": "mg N/L"},
    14: {"name": "DISS_ORG_P", "description": "Dissolved Organic Phosphorus", "units": "mg P/L"},
    15: {"name": "CYN_C", "description": "Non-fixing Cyanobacteria", "units": "mg C/L"},
    16: {"name": "OPA_C", "description": "Other Phytoplankton", "units": "mg C/L"},
    17: {"name": "DISS_Si", "description": "Dissolved Silica", "units": "mg Si/L"},
    18: {"name": "PART_Si", "description": "Particulate Silica", "units": "mg Si/L"},
    19: {"name": "FIX_CYN_C", "description": "N-fixing Cyanobacteria", "units": "mg C/L"},
    20: {"name": "INORG_C", "description": "Dissolved Inorganic Carbon", "units": "mol C/L"},
    21: {"name": "TOT_ALK", "description": "Total Alkalinity", "units": "eq/L"},
    22: {"name": "FE_II", "description": "Ferrous Iron", "units": "mg Fe/L"},
    23: {"name": "FE_III", "description": "Ferric Iron", "units": "mg Fe/L"},
    24: {"name": "MN_II", "description": "Manganous Manganese", "units": "mg Mn/L"},
    25: {"name": "MN_IV", "description": "Manganic Manganese", "units": "mg Mn/L"},
    26: {"name": "CA", "description": "Calcium", "units": "mg Ca/L"},
    27: {"name": "MG", "description": "Magnesium", "units": "mg Mg/L"},
    28: {"name": "S_PLUS_6", "description": "Sulphate", "units": "mg S/L"},
    29: {"name": "S_MINUS_2", "description": "Sulphide", "units": "mg S/L"},
    30: {"name": "CH4_C", "description": "Methane Carbon", "units": "mg C/L"},
    31: {"name": "NOST_VEG_HET_C", "description": "Nostocales Carbon", "units": "mg C/L"},
    32: {"name": "NOST_AKI_C", "description": "Akinetes Carbon", "units": "mg C/L"},
    33: {"name": "SEC_METAB_DIA", "description": "Diatom Secondary Metabolites", "units": "relative"},
    34: {"name": "SEC_METAB_NOFIX_CYN", "description": "Non-fix Cyanobacteria Metabolites", "units": "relative"},
    35: {"name": "SEC_METAB_FIX_CYN", "description": "Fix Cyanobacteria Metabolites", "units": "relative"},
    36: {"name": "SEC_METAB_NOST", "description": "Nostocales Metabolites", "units": "relative"},
    # Additional commonly observed variables (indices > 36)
    37: {"name": "pH", "description": "pH", "units": "-"},
    38: {"name": "Temp", "description": "Temperature", "units": "°C"},
    39: {"name": "Tot_P", "description": "Total Phosphorus", "units": "mg P/L"},
    40: {"name": "Tot_N", "description": "Total Nitrogen", "units": "mg N/L"},
    # Chlorophyll and biomass
    57: {"name": "N2_fix", "description": "N₂ Fixation Rate", "units": "µg N/L/d"},
    58: {"name": "GPP", "description": "Gross Primary Production", "units": "mg C/L/d"},
    59: {"name": "Chl_a", "description": "Chlorophyll a", "units": "µg/L"},
}


def get_variable_name(index: int) -> str:
    """Get variable name from index."""
    if index in STATE_VARIABLE_INDEX:
        return STATE_VARIABLE_INDEX[index]["name"]
    return f"Var_{index}"


def get_variable_description(index: int) -> str:
    """Get variable description from index."""
    if index in STATE_VARIABLE_INDEX:
        info = STATE_VARIABLE_INDEX[index]
        return f"{info['name']} - {info['description']} ({info['units']})"
    return f"Variable {index}"


@dataclass
class ObservationFile:
    """Information about an observation file."""
    filepath: str
    filename: str
    file_type: str  # 'dates', 'xlsx', 'csv'
    station: str = ""
    description: str = ""
    n_records: int = 0
    date_range: Tuple[Optional[str], Optional[str]] = (None, None)
    variables: List[int] = field(default_factory=list)  # Variable indices
    variable_names: List[str] = field(default_factory=list)
    has_data: Dict[int, int] = field(default_factory=dict)  # var_index -> count of valid values


@dataclass
class LoadedObservations:
    """Container for loaded observation data."""
    file_info: ObservationFile
    df: pd.DataFrame
    time_column: str = "datetime"
    julian_day_column: str = "julian_day"
    
    def get_available_variables(self) -> List[Tuple[int, str, int]]:
        """Return list of (index, name, count) for variables with data."""
        result = []
        for var_idx, count in self.file_info.has_data.items():
            if count > 0:
                name = get_variable_description(var_idx)
                result.append((var_idx, name, count))
        return sorted(result, key=lambda x: x[0])
    
    def get_variable_data(self, var_index: int) -> Optional[pd.DataFrame]:
        """Get time series for a specific variable."""
        col_name = str(var_index)
        if col_name not in self.df.columns:
            return None
        
        subset = self.df[[self.julian_day_column, col_name]].copy()
        subset = subset[subset[col_name] != -1]  # -1 = missing value
        subset = subset.dropna()
        subset.columns = ["TIME", "VALUE"]
        return subset


def parse_dates_datetime(date_str: str, time_str: str = "000000") -> Optional[datetime]:
    """Parse date and time strings from .dates file format.
    
    Args:
        date_str: Date in YYYYMMDD format
        time_str: Time in HHMMSS format (default midnight)
    
    Returns:
        datetime object or None if parsing fails
    """
    try:
        date_str = str(date_str).strip()
        time_str = str(time_str).strip() if time_str else "000000"
        
        if len(date_str) == 8:
            year = int(date_str[0:4])
            month = int(date_str[4:6])
            day = int(date_str[6:8])
            
            hour = int(time_str[0:2]) if len(time_str) >= 2 else 0
            minute = int(time_str[2:4]) if len(time_str) >= 4 else 0
            second = int(time_str[4:6]) if len(time_str) >= 6 else 0
            
            return datetime(year, month, day, hour, minute, second)
    except (ValueError, IndexError) as e:
        logger.debug(f"Failed to parse date '{date_str}' time '{time_str}': {e}")
    return None


def datetime_to_julian_day(dt: datetime, reference_year: int = 2015) -> float:
    """Convert datetime to Julian day (days since start of reference year)."""
    reference_date = datetime(reference_year, 1, 1)
    delta = dt - reference_date
    return delta.total_seconds() / 86400.0


def load_dates_file(filepath: str) -> Optional[LoadedObservations]:
    """
    Load a .dates observation file.
    
    Format:
      - First line: Header with #date time followed by variable indices
      - Data lines: YYYYMMDD HHMMSS followed by whitespace-separated values
      - Missing values are represented as -1
    
    Args:
        filepath: Path to .dates file
        
    Returns:
        LoadedObservations object or None on failure
    """
    if not os.path.exists(filepath):
        logger.error(f"File not found: {filepath}")
        return None
    
    try:
        filename = os.path.basename(filepath)
        
        # Extract station name from filename (e.g., sta1ND.dates -> sta1ND)
        station = os.path.splitext(filename)[0]
        
        # Read file content
        with open(filepath, 'r') as f:
            lines = f.readlines()
        
        if not lines:
            logger.error(f"Empty file: {filepath}")
            return None
        
        # Parse header line
        header_line = lines[0].strip()
        if header_line.startswith('#'):
            header_line = header_line[1:].strip()
        
        # Split header - first two are date/time, rest are variable indices
        header_parts = header_line.split()
        
        # Find where variable indices start (after 'date' and 'time')
        var_indices = []
        for i, part in enumerate(header_parts):
            if i < 2:  # Skip date, time
                continue
            try:
                idx = int(part)
                var_indices.append(idx)
            except ValueError:
                continue
        
        logger.info(f"Found {len(var_indices)} variable columns in {filename}")
        
        # Parse data lines
        data_rows = []
        for line in lines[1:]:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            
            parts = line.split()
            if len(parts) < 2:
                continue
            
            date_str = parts[0]
            time_str = parts[1] if len(parts) > 1 else "000000"
            
            dt = parse_dates_datetime(date_str, time_str)
            if dt is None:
                continue
            
            # Parse values
            values = []
            for i, idx in enumerate(var_indices):
                val_idx = i + 2  # Skip date, time
                if val_idx < len(parts):
                    try:
                        val = float(parts[val_idx])
                        values.append(val)
                    except ValueError:
                        values.append(-1.0)  # Missing
                else:
                    values.append(-1.0)
            
            julian_day = datetime_to_julian_day(dt)
            data_rows.append({
                "datetime": dt,
                "julian_day": julian_day,
                **{str(var_indices[i]): values[i] for i in range(len(var_indices))}
            })
        
        if not data_rows:
            logger.warning(f"No valid data rows in {filepath}")
            return None
        
        # Create DataFrame
        df = pd.DataFrame(data_rows)
        
        # Count valid values per variable
        has_data = {}
        for var_idx in var_indices:
            col = str(var_idx)
            if col in df.columns:
                valid_count = (df[col] != -1).sum()
                if valid_count > 0:
                    has_data[var_idx] = int(valid_count)
        
        # Date range
        date_min = df["datetime"].min().strftime("%Y-%m-%d") if len(df) > 0 else None
        date_max = df["datetime"].max().strftime("%Y-%m-%d") if len(df) > 0 else None
        
        # Create file info
        file_info = ObservationFile(
            filepath=filepath,
            filename=filename,
            file_type="dates",
            station=station,
            description=f"Station {station} observations",
            n_records=len(df),
            date_range=(date_min, date_max),
            variables=var_indices,
            variable_names=[get_variable_name(i) for i in var_indices],
            has_data=has_data,
        )
        
        return LoadedObservations(
            file_info=file_info,
            df=df,
        )
        
    except Exception as e:
        logger.error(f"Error loading {filepath}: {e}")
        import traceback
        traceback.print_exc()
        return None


def load_xlsx_file(filepath: str, sheet_name: Optional[str] = None) -> Optional[LoadedObservations]:
    """
    Load an Excel observation file.
    
    Attempts to auto-detect the format:
      - Looks for date/time column
      - Identifies variable columns
    
    Args:
        filepath: Path to .xlsx file
        sheet_name: Optional specific sheet to read
        
    Returns:
        LoadedObservations object or None on failure
    """
    if not os.path.exists(filepath):
        logger.error(f"File not found: {filepath}")
        return None
    
    try:
        filename = os.path.basename(filepath)
        
        # Read Excel file
        if sheet_name:
            df = pd.read_excel(filepath, sheet_name=sheet_name)
        else:
            # Try to read first sheet
            df = pd.read_excel(filepath)
        
        if df.empty:
            logger.warning(f"Empty Excel file: {filepath}")
            return None
        
        # Clean column names
        df.columns = [str(c).strip() for c in df.columns]
        
        # Find date/time column
        time_col = None
        date_patterns = ['date', 'time', 'datetime', 'timestamp', 'day', 'julian']
        for col in df.columns:
            if any(p in col.lower() for p in date_patterns):
                time_col = col
                break
        
        if time_col is None:
            # Use first column
            time_col = df.columns[0]
            logger.warning(f"No date column found in {filename}, using '{time_col}'")
        
        # Convert time column to datetime if needed
        if not pd.api.types.is_datetime64_any_dtype(df[time_col]):
            try:
                df[time_col] = pd.to_datetime(df[time_col])
            except Exception:
                logger.warning(f"Could not convert {time_col} to datetime")
        
        # Create julian day column
        if pd.api.types.is_datetime64_any_dtype(df[time_col]):
            reference_date = pd.Timestamp(year=2015, month=1, day=1)
            df["julian_day"] = (df[time_col] - reference_date).dt.total_seconds() / 86400.0
            df["datetime"] = df[time_col]
        else:
            # Assume column is already julian day
            df["julian_day"] = pd.to_numeric(df[time_col], errors='coerce')
            df["datetime"] = pd.to_datetime('2015-01-01') + pd.to_timedelta(df["julian_day"], unit='D')
        
        # Identify numeric variable columns
        numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
        var_cols = [c for c in numeric_cols if c not in ['julian_day', time_col]]
        
        # Map column names to variable indices (best effort)
        var_indices = []
        has_data = {}
        for col in var_cols:
            # Try to match column name to known variables
            matched_idx = None
            col_lower = col.lower().replace(' ', '_').replace('-', '_')
            
            for idx, info in STATE_VARIABLE_INDEX.items():
                if info['name'].lower() == col_lower:
                    matched_idx = idx
                    break
                if col_lower in info['description'].lower():
                    matched_idx = idx
                    break
            
            if matched_idx:
                var_indices.append(matched_idx)
                valid_count = df[col].notna().sum()
                if valid_count > 0:
                    has_data[matched_idx] = int(valid_count)
                # Rename column to index for consistency
                df.rename(columns={col: str(matched_idx)}, inplace=True)
            else:
                # Use arbitrary index
                new_idx = 100 + len(var_indices)
                var_indices.append(new_idx)
                STATE_VARIABLE_INDEX[new_idx] = {
                    "name": col,
                    "description": col,
                    "units": "unknown"
                }
                valid_count = df[col].notna().sum()
                if valid_count > 0:
                    has_data[new_idx] = int(valid_count)
                df.rename(columns={col: str(new_idx)}, inplace=True)
        
        # Date range
        date_min = df["datetime"].min().strftime("%Y-%m-%d") if len(df) > 0 and pd.notna(df["datetime"].min()) else None
        date_max = df["datetime"].max().strftime("%Y-%m-%d") if len(df) > 0 and pd.notna(df["datetime"].max()) else None
        
        # Create file info
        file_info = ObservationFile(
            filepath=filepath,
            filename=filename,
            file_type="xlsx",
            station=os.path.splitext(filename)[0],
            description=f"Excel observations from {filename}",
            n_records=len(df),
            date_range=(date_min, date_max),
            variables=var_indices,
            variable_names=[get_variable_name(i) for i in var_indices],
            has_data=has_data,
        )
        
        return LoadedObservations(
            file_info=file_info,
            df=df,
        )
        
    except Exception as e:
        logger.error(f"Error loading Excel file {filepath}: {e}")
        import traceback
        traceback.print_exc()
        return None


def scan_observations_directory(obs_dir: str) -> List[ObservationFile]:
    """
    Scan OBSERVATIONS directory for observation files.
    
    Args:
        obs_dir: Path to OBSERVATIONS directory
        
    Returns:
        List of ObservationFile objects with metadata
    """
    files = []
    
    if not os.path.isdir(obs_dir):
        logger.warning(f"Observations directory not found: {obs_dir}")
        return files
    
    for filename in sorted(os.listdir(obs_dir)):
        filepath = os.path.join(obs_dir, filename)
        
        if not os.path.isfile(filepath):
            continue
        
        ext = os.path.splitext(filename)[1].lower()
        
        if ext == '.dates':
            # Load and extract metadata
            loaded = load_dates_file(filepath)
            if loaded:
                files.append(loaded.file_info)
            else:
                # Add placeholder with basic info
                files.append(ObservationFile(
                    filepath=filepath,
                    filename=filename,
                    file_type="dates",
                    station=os.path.splitext(filename)[0],
                    description="Could not parse file",
                ))
        
        elif ext == '.xlsx':
            # Try to get sheet names for metadata
            try:
                xl = pd.ExcelFile(filepath)
                sheet_names = xl.sheet_names
                
                files.append(ObservationFile(
                    filepath=filepath,
                    filename=filename,
                    file_type="xlsx",
                    station=os.path.splitext(filename)[0],
                    description=f"Excel file with {len(sheet_names)} sheet(s)",
                ))
            except Exception as e:
                logger.debug(f"Could not read Excel metadata from {filename}: {e}")
                files.append(ObservationFile(
                    filepath=filepath,
                    filename=filename,
                    file_type="xlsx",
                    station=os.path.splitext(filename)[0],
                    description="Excel file",
                ))
        
        elif ext == '.csv':
            files.append(ObservationFile(
                filepath=filepath,
                filename=filename,
                file_type="csv",
                station=os.path.splitext(filename)[0],
                description="CSV observation file",
            ))
        
        elif ext == '.txt' and 'readme' not in filename.lower():
            files.append(ObservationFile(
                filepath=filepath,
                filename=filename,
                file_type="txt",
                station=os.path.splitext(filename)[0],
                description="Text observation file",
            ))
    
    return files


def get_file_preview(filepath: str, max_rows: int = 10) -> Dict[str, Any]:
    """
    Get a preview of observation file contents.
    
    Args:
        filepath: Path to observation file
        max_rows: Maximum rows to include in preview
        
    Returns:
        Dict with 'columns', 'data', 'info' keys
    """
    ext = os.path.splitext(filepath)[1].lower()
    
    try:
        if ext == '.dates':
            loaded = load_dates_file(filepath)
            if loaded:
                preview_df = loaded.df.head(max_rows).copy()
                
                # Format columns for display
                cols = ["datetime", "julian_day"]
                for var_idx in sorted(loaded.file_info.has_data.keys())[:20]:  # Limit columns
                    cols.append(str(var_idx))
                
                cols = [c for c in cols if c in preview_df.columns]
                preview_df = preview_df[cols]
                
                # Format datetime
                if "datetime" in preview_df.columns:
                    preview_df["datetime"] = preview_df["datetime"].dt.strftime("%Y-%m-%d %H:%M")
                
                # Round numeric columns
                for col in preview_df.select_dtypes(include=[np.number]).columns:
                    preview_df[col] = preview_df[col].round(4)
                
                # Replace -1 with NaN for display
                preview_df = preview_df.replace(-1, np.nan)
                
                return {
                    "columns": cols,
                    "data": preview_df.to_dict('records'),
                    "info": {
                        "n_records": loaded.file_info.n_records,
                        "n_variables": len(loaded.file_info.has_data),
                        "date_range": loaded.file_info.date_range,
                        "variables_with_data": [
                            (idx, get_variable_description(idx), count)
                            for idx, count in sorted(loaded.file_info.has_data.items())
                        ]
                    }
                }
        
        elif ext == '.xlsx':
            loaded = load_xlsx_file(filepath)
            if loaded:
                preview_df = loaded.df.head(max_rows).copy()
                
                # Format datetime
                if "datetime" in preview_df.columns:
                    preview_df["datetime"] = preview_df["datetime"].dt.strftime("%Y-%m-%d")
                
                cols = list(preview_df.columns)[:15]  # Limit columns
                preview_df = preview_df[cols]
                
                # Round numeric columns
                for col in preview_df.select_dtypes(include=[np.number]).columns:
                    preview_df[col] = preview_df[col].round(4)
                
                return {
                    "columns": cols,
                    "data": preview_df.to_dict('records'),
                    "info": {
                        "n_records": loaded.file_info.n_records,
                        "n_variables": len(loaded.file_info.has_data),
                        "date_range": loaded.file_info.date_range,
                        "variables_with_data": [
                            (idx, get_variable_description(idx), count)
                            for idx, count in sorted(loaded.file_info.has_data.items())
                        ]
                    }
                }
        
        elif ext == '.csv':
            df = pd.read_csv(filepath, nrows=max_rows)
            return {
                "columns": list(df.columns),
                "data": df.to_dict('records'),
                "info": {
                    "n_records": "~",
                    "n_variables": len(df.columns) - 1,
                }
            }
    
    except Exception as e:
        logger.error(f"Error previewing {filepath}: {e}")
    
    return {
        "columns": [],
        "data": [],
        "info": {"error": "Could not read file"}
    }


def load_observation_file(filepath: str) -> Optional[LoadedObservations]:
    """
    Load observation file based on extension.
    
    Args:
        filepath: Path to observation file
        
    Returns:
        LoadedObservations object or None
    """
    ext = os.path.splitext(filepath)[1].lower()
    
    if ext == '.dates':
        return load_dates_file(filepath)
    elif ext == '.xlsx':
        return load_xlsx_file(filepath)
    elif ext == '.csv':
        # Delegate to existing ObservationData class for CSV
        return None  # Will use existing CSV loader
    else:
        logger.warning(f"Unsupported file type: {ext}")
        return None


def create_variable_mapping_table() -> pd.DataFrame:
    """Create a reference table of variable indices and descriptions."""
    rows = []
    for idx, info in sorted(STATE_VARIABLE_INDEX.items()):
        rows.append({
            "Index": idx,
            "Variable": info["name"],
            "Description": info["description"],
            "Units": info["units"]
        })
    return pd.DataFrame(rows)
