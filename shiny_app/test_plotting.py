#!/usr/bin/env python3
"""
Test suite for AQUABC plotting functionality.

This module tests the data loading and plotting functions used by the Shiny app.
Run with: python -m pytest test_plotting.py -v
Or standalone: python test_plotting.py
"""

import os
import sys
import tempfile
from datetime import datetime, timedelta

import numpy as np
import pandas as pd

# Add shiny_app to path
_script_dir = os.path.dirname(os.path.realpath(__file__))
_parent_dir = os.path.dirname(_script_dir)
if _script_dir not in sys.path:
    sys.path.insert(0, _script_dir)
if _parent_dir not in sys.path:
    sys.path.insert(0, _parent_dir)

# Constants from app.py
PELAGIC_BOX_COLUMNS = [
    "TIME_DAYS", "NH4_N", "NO3_N", "PO4_P", "DISS_OXYGEN", "DIA_C",
    "ZOO_C", "ZOO_N", "ZOO_P", "DET_PART_ORG_C", "DET_PART_ORG_N",
    "DET_PART_ORG_P", "DISS_ORG_C", "DISS_ORG_N", "DISS_ORG_P",
    "CYN_C", "OPA_C", "DISS_Si", "PART_Si", "FIX_CYN_C",
    "INORG_C", "TOT_ALK", "FE_II", "FE_III", "MN_II", "MN_IV",
    "CA", "MG", "S_PLUS_6", "S_MINUS_2", "CH4_C",
    "NOST_VEG_HET_C", "AKI_C", "SEC_METAB_DIA",
    "SEC_METAB_NOFIX_CYN", "SEC_METAB_FIX_CYN", "SEC_METAB_NOST"
]


def read_pelagic_binary(bin_file, max_rows=None):
    """Read Fortran binary PELAGIC_BOX output file.

    Binary format (from Fortran stream I/O):
    - Each row: TIME (float64) + 36 state variables (float64)
    - Total 37 columns, all double precision (8 bytes)
    - No record markers (Fortran ACCESS='STREAM')
    """
    ncols = len(PELAGIC_BOX_COLUMNS)  # 37

    with open(bin_file, 'rb') as f:
        data = np.fromfile(f, dtype=np.float64)

    nrows = len(data) // ncols
    remainder = len(data) % ncols

    if remainder != 0:
        print(f"Warning: Binary file has {remainder} extra bytes, truncating")
        data = data[:nrows * ncols]

    data = data.reshape(nrows, ncols)

    if max_rows is not None and nrows > max_rows:
        data = data[:max_rows]

    df = pd.DataFrame(data, columns=PELAGIC_BOX_COLUMNS)
    return df


def read_pelagic_text(text_file, max_rows=None):
    """Read PELAGIC_BOX text output file (whitespace-separated)."""
    df = pd.read_csv(text_file, sep=r'\s+', nrows=max_rows)
    df.columns = [c.strip() for c in df.columns]
    return df


class TestDataGeneration:
    """Test helpers for generating mock data"""

    @staticmethod
    def create_mock_csv(filepath, nrows=100):
        """Create a mock OUTPUT.csv file for testing"""
        data = {
            'TIME_DAYS': np.linspace(0, 365, nrows),
            'NH4_N': np.random.rand(nrows) * 0.5,
            'NO3_N': np.random.rand(nrows) * 2.0,
            'PO4_P': np.random.rand(nrows) * 0.1,
            'DISS_OXYGEN': 8 + np.random.rand(nrows) * 4,
            'DIA_C': np.random.rand(nrows) * 100,
        }
        df = pd.DataFrame(data)
        df.to_csv(filepath, index=False)
        return df

    @staticmethod
    def create_mock_binary(filepath, nrows=100):
        """Create a mock binary PELAGIC_BOX file for testing"""
        ncols = len(PELAGIC_BOX_COLUMNS)
        data = np.zeros((nrows, ncols), dtype=np.float64)
        data[:, 0] = np.linspace(0, 365, nrows)  # TIME_DAYS
        for i in range(1, ncols):
            data[:, i] = np.random.rand(nrows) * 10

        with open(filepath, 'wb') as f:
            data.tofile(f)
        return data

    @staticmethod
    def create_mock_text(filepath, nrows=100):
        """Create a mock text PELAGIC_BOX file for testing"""
        ncols = len(PELAGIC_BOX_COLUMNS)
        data = np.zeros((nrows, ncols))
        data[:, 0] = np.linspace(0, 365, nrows)
        for i in range(1, ncols):
            data[:, i] = np.random.rand(nrows) * 10

        df = pd.DataFrame(data, columns=PELAGIC_BOX_COLUMNS)
        # Write with whitespace separator (like Fortran output)
        with open(filepath, 'w') as f:
            f.write(' '.join(PELAGIC_BOX_COLUMNS) + '\n')
            for _, row in df.iterrows():
                f.write(' '.join(f'{v:.6E}' for v in row.values) + '\n')
        return df


class TestBinaryFileReading:
    """Tests for binary file reading"""

    def test_read_binary_basic(self):
        """Test reading a basic binary file"""
        with tempfile.NamedTemporaryFile(suffix='.bin', delete=False) as f:
            tmp_path = f.name

        try:
            # Create mock data
            original_data = TestDataGeneration.create_mock_binary(tmp_path, nrows=50)

            # Read it back
            df = read_pelagic_binary(tmp_path)

            assert df is not None, "DataFrame should not be None"
            assert len(df) == 50, f"Expected 50 rows, got {len(df)}"
            assert len(df.columns) == 37, f"Expected 37 columns, got {len(df.columns)}"
            assert list(df.columns) == PELAGIC_BOX_COLUMNS, "Column names don't match"

            # Check values match
            np.testing.assert_array_almost_equal(
                df['TIME_DAYS'].values,
                original_data[:, 0],
                decimal=5
            )
            print("✓ Binary file reading: PASSED")
        finally:
            os.unlink(tmp_path)

    def test_read_binary_with_max_rows(self):
        """Test reading binary file with row limit"""
        with tempfile.NamedTemporaryFile(suffix='.bin', delete=False) as f:
            tmp_path = f.name

        try:
            TestDataGeneration.create_mock_binary(tmp_path, nrows=100)

            df = read_pelagic_binary(tmp_path, max_rows=25)

            assert len(df) == 25, f"Expected 25 rows, got {len(df)}"
            print("✓ Binary file max_rows limit: PASSED")
        finally:
            os.unlink(tmp_path)

    def test_read_binary_empty_file(self):
        """Test handling of empty binary file"""
        with tempfile.NamedTemporaryFile(suffix='.bin', delete=False) as f:
            tmp_path = f.name

        try:
            # Empty file
            df = read_pelagic_binary(tmp_path)
            assert len(df) == 0, "Empty file should produce 0 rows"
            print("✓ Empty binary file handling: PASSED")
        finally:
            os.unlink(tmp_path)


class TestTextFileReading:
    """Tests for text file reading"""

    def test_read_text_basic(self):
        """Test reading a basic text file"""
        with tempfile.NamedTemporaryFile(suffix='.out', delete=False, mode='w') as f:
            tmp_path = f.name

        try:
            TestDataGeneration.create_mock_text(tmp_path, nrows=50)

            df = read_pelagic_text(tmp_path)

            assert df is not None, "DataFrame should not be None"
            assert len(df) == 50, f"Expected 50 rows, got {len(df)}"
            assert len(df.columns) == 37, f"Expected 37 columns, got {len(df.columns)}"
            print("✓ Text file reading: PASSED")
        finally:
            os.unlink(tmp_path)

    def test_read_text_with_max_rows(self):
        """Test reading text file with row limit"""
        with tempfile.NamedTemporaryFile(suffix='.out', delete=False, mode='w') as f:
            tmp_path = f.name

        try:
            TestDataGeneration.create_mock_text(tmp_path, nrows=100)

            df = read_pelagic_text(tmp_path, max_rows=30)

            assert len(df) == 30, f"Expected 30 rows, got {len(df)}"
            print("✓ Text file max_rows limit: PASSED")
        finally:
            os.unlink(tmp_path)


class TestCSVReading:
    """Tests for CSV file reading"""

    def test_read_csv_basic(self):
        """Test reading a basic CSV file"""
        with tempfile.NamedTemporaryFile(suffix='.csv', delete=False, mode='w') as f:
            tmp_path = f.name

        try:
            TestDataGeneration.create_mock_csv(tmp_path, nrows=50)

            df = pd.read_csv(tmp_path, comment='#', skip_blank_lines=True)
            df.columns = [c.strip() for c in df.columns]

            assert df is not None, "DataFrame should not be None"
            assert len(df) == 50, f"Expected 50 rows, got {len(df)}"
            assert 'TIME_DAYS' in df.columns, "TIME_DAYS column missing"
            assert 'NH4_N' in df.columns, "NH4_N column missing"
            print("✓ CSV file reading: PASSED")
        finally:
            os.unlink(tmp_path)


class TestDateConversion:
    """Tests for Julian day to date conversion"""

    def test_julian_to_date(self):
        """Test converting Julian days to actual dates"""
        reference_date = datetime(1997, 1, 1)

        # Test day 0
        date_0 = reference_date + timedelta(days=0)
        assert date_0 == datetime(1997, 1, 1), "Day 0 should be Jan 1, 1997"

        # Test day 365
        date_365 = reference_date + timedelta(days=365)
        assert date_365 == datetime(1998, 1, 1), "Day 365 should be Jan 1, 1998"

        # Test day 6209 (Dec 31, 2014 with base 1998)
        base_1998 = datetime(1998, 1, 1)
        date_6209 = base_1998 + timedelta(days=6209 - 1)
        assert date_6209.year == 2014, f"Expected year 2014, got {date_6209.year}"
        assert date_6209.month == 12, f"Expected month 12, got {date_6209.month}"
        assert date_6209.day == 31, f"Expected day 31, got {date_6209.day}"

        print("✓ Julian day to date conversion: PASSED")


class TestPlotlyFigureCreation:
    """Tests for Plotly figure creation"""

    def test_create_basic_figure(self):
        """Test creating a basic Plotly figure"""
        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping figure creation tests")
            return

        # Create mock data
        x = list(range(100))
        y = [np.sin(i * 0.1) for i in x]

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=x, y=y, mode='lines', name='Test'))

        assert fig is not None, "Figure should not be None"
        assert len(fig.data) == 1, "Figure should have 1 trace"
        assert fig.data[0].name == 'Test', "Trace name should be 'Test'"

        print("✓ Basic Plotly figure creation: PASSED")

    def test_create_dual_axis_figure(self):
        """Test creating a figure with dual y-axes"""
        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping dual axis tests")
            return

        x = list(range(100))
        y1 = [np.sin(i * 0.1) for i in x]
        y2 = [np.cos(i * 0.1) * 100 for i in x]

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=x, y=y1, mode='lines', name='Left', yaxis='y'))
        fig.add_trace(go.Scatter(x=x, y=y2, mode='lines', name='Right', yaxis='y2'))

        fig.update_layout(
            yaxis=dict(title='Left axis'),
            yaxis2=dict(title='Right axis', overlaying='y', side='right')
        )

        assert len(fig.data) == 2, "Figure should have 2 traces"
        print("✓ Dual axis Plotly figure creation: PASSED")

    def test_create_date_axis_figure(self):
        """Test creating a figure with date x-axis"""
        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping date axis tests")
            return

        reference_date = datetime(1997, 1, 1)
        julian_days = np.linspace(0, 365, 100)
        x_dates = [reference_date + timedelta(days=float(jd)) for jd in julian_days]
        y = [np.sin(jd * 0.02) for jd in julian_days]

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=x_dates, y=y, mode='lines', name='Time series'))

        fig.update_layout(
            xaxis=dict(
                title='Date',
                type='date',
                tickformat='%Y-%m-%d'
            )
        )

        assert len(fig.data) == 1, "Figure should have 1 trace"
        print("✓ Date axis Plotly figure creation: PASSED")

    def test_figure_to_html(self):
        """Test converting figure to HTML"""
        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping HTML conversion tests")
            return

        fig = go.Figure()
        fig.add_trace(go.Scatter(x=[1, 2, 3], y=[1, 4, 9], mode='lines'))

        html = fig.to_html(include_plotlyjs='cdn')

        assert html is not None, "HTML should not be None"
        assert '<div' in html, "HTML should contain div elements"
        assert 'plotly' in html.lower(), "HTML should reference plotly"

        print("✓ Figure to HTML conversion: PASSED")


class TestDataSmoothing:
    """Tests for data smoothing (rolling average)"""

    def test_rolling_average(self):
        """Test rolling average calculation"""
        y = pd.Series([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

        smoothed = y.rolling(window=3, min_periods=1).mean()

        assert len(smoothed) == len(y), "Smoothed series should have same length"
        assert smoothed.iloc[0] == 1.0, "First value should be 1.0 (single value average)"
        assert smoothed.iloc[2] == 2.0, "Third value should be 2.0 (average of 1,2,3)"

        print("✓ Rolling average: PASSED")


class TestRealOutputFiles:
    """Tests that use real output files if available"""

    def __init__(self):
        self.root = _parent_dir
        self.outputs_dir = os.path.join(self.root, 'OUTPUTS')

    def test_real_binary_file(self):
        """Test reading a real binary output file if exists"""
        import glob
        bin_files = glob.glob(os.path.join(self.outputs_dir, '*PELAGIC_BOX*.bin'))

        if not bin_files:
            print("⚠ No binary output files found in OUTPUTS/, skipping real file test")
            return

        bin_file = bin_files[0]
        print(f"Testing with real binary file: {os.path.basename(bin_file)}")

        df = read_pelagic_binary(bin_file, max_rows=100)

        assert df is not None, "DataFrame should not be None"
        assert len(df) > 0, "DataFrame should have rows"
        assert 'TIME_DAYS' in df.columns, "TIME_DAYS column should exist"

        print(f"✓ Real binary file reading: {len(df)} rows, {len(df.columns)} cols - PASSED")

    def test_real_text_file(self):
        """Test reading a real text output file if exists"""
        import glob
        out_files = glob.glob(os.path.join(self.outputs_dir, 'PELAGIC_BOX*.out'))

        if not out_files:
            print("⚠ No text output files found in OUTPUTS/, skipping real file test")
            return

        out_file = out_files[0]
        print(f"Testing with real text file: {os.path.basename(out_file)}")

        df = read_pelagic_text(out_file, max_rows=100)

        assert df is not None, "DataFrame should not be None"
        assert len(df) > 0, "DataFrame should have rows"

        print(f"✓ Real text file reading: {len(df)} rows, {len(df.columns)} cols - PASSED")

    def test_real_csv_file(self):
        """Test reading real OUTPUT.csv if exists"""
        csv_file = os.path.join(self.root, 'OUTPUT.csv')

        if not os.path.exists(csv_file):
            print("⚠ OUTPUT.csv not found, skipping real CSV test")
            return

        print("Testing with OUTPUT.csv")

        df = pd.read_csv(csv_file, comment='#', skip_blank_lines=True, nrows=100)
        df.columns = [c.strip() for c in df.columns]

        assert df is not None, "DataFrame should not be None"
        assert len(df) > 0, "DataFrame should have rows"

        print(f"✓ Real CSV file reading: {len(df)} rows, {len(df.columns)} cols - PASSED")


class TestFullPlotPipeline:
    """Tests the full plotting pipeline as used in the Shiny app"""

    def __init__(self):
        self.root = _parent_dir
        self.outputs_dir = os.path.join(self.root, 'OUTPUTS')

    def test_full_plot_from_text_file(self):
        """Test creating a complete plot from a real text file"""
        import glob

        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping full pipeline test")
            return

        out_files = glob.glob(os.path.join(self.outputs_dir, 'PELAGIC_BOX*.out'))

        if not out_files:
            print("⚠ No text output files found in OUTPUTS/, skipping full pipeline test")
            return

        out_file = out_files[0]
        print(f"Testing full plot pipeline with: {os.path.basename(out_file)}")

        # Step 1: Read data
        df = read_pelagic_text(out_file, max_rows=1000)
        assert df is not None, "Failed to read data"
        assert len(df) > 0, "No data rows"

        # Step 2: Get x-axis (TIME_DAYS)
        xcol = df.columns[0]
        assert 'TIME' in xcol.upper(), f"First column should be TIME, got {xcol}"

        # Step 3: Convert to dates
        reference_date = datetime(1997, 1, 1)
        try:
            x_dates = [reference_date + timedelta(days=float(jd)) for jd in df[xcol]]
            x_data = x_dates
            x_is_date = True
        except (ValueError, TypeError) as e:
            print(f"Warning: Could not convert to dates: {e}")
            x_data = df[xcol].tolist()
            x_is_date = False

        # Step 4: Select variables to plot
        left_vars = ['NH4_N', 'NO3_N'] if 'NH4_N' in df.columns else [df.columns[1]]
        right_vars = ['DISS_OXYGEN'] if 'DISS_OXYGEN' in df.columns else [df.columns[2]]

        # Step 5: Create figure
        fig = go.Figure()

        for var in left_vars:
            if var in df.columns:
                y = df[var].tolist()
                fig.add_trace(go.Scatter(x=x_data, y=y, mode='lines', name=var, yaxis='y'))

        for var in right_vars:
            if var in df.columns:
                y = df[var].tolist()
                fig.add_trace(go.Scatter(x=x_data, y=y, mode='lines', name=var, yaxis='y2'))

        # Step 6: Configure layout
        if x_is_date:
            xaxis_config = dict(title='Date', type='date', tickformat='%Y-%m-%d')
        else:
            xaxis_config = dict(title=xcol)

        layout = dict(
            title='Test Plot',
            yaxis=dict(title='Left axis'),
            yaxis2=dict(title='Right axis', overlaying='y', side='right'),
            xaxis=xaxis_config,
            legend=dict(orientation='h', yanchor='top', y=-0.15, xanchor='center', x=0.5),
            margin=dict(b=80)
        )

        fig.update_layout(**layout)

        # Step 7: Convert to FigureWidget (what Shiny does)
        widget = go.FigureWidget(fig)

        assert widget is not None, "FigureWidget should not be None"
        assert len(widget.data) > 0, "Widget should have traces"

        # Step 8: Verify HTML export
        html = fig.to_html(include_plotlyjs='cdn')
        assert html is not None, "HTML export failed"
        assert len(html) > 1000, "HTML seems too short"

        print(f"✓ Full plot pipeline: {len(widget.data)} traces, {len(df)} data points - PASSED")

    def test_smoothing_pipeline(self):
        """Test that smoothing works in the pipeline"""
        import glob

        try:
            import plotly.graph_objects as go
        except ImportError:
            print("⚠ Plotly not installed, skipping smoothing test")
            return

        out_files = glob.glob(os.path.join(self.outputs_dir, 'PELAGIC_BOX*.out'))

        if not out_files:
            print("⚠ No text output files found, skipping smoothing test")
            return

        df = read_pelagic_text(out_files[0], max_rows=500)

        if 'NH4_N' not in df.columns:
            print("⚠ NH4_N column not found, skipping smoothing test")
            return

        # Apply smoothing
        y_raw = df['NH4_N']
        y_smooth = y_raw.rolling(window=10, min_periods=1).mean()

        assert len(y_smooth) == len(y_raw), "Smoothed series should have same length"
        assert y_smooth.isna().sum() == 0, "Smoothed series should have no NaN values"

        # Create plot with smoothed data
        x_data = df[df.columns[0]].tolist()
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=x_data, y=y_smooth.tolist(), mode='lines', name='NH4_N (smoothed)'))

        assert len(fig.data) == 1, "Figure should have 1 trace"

        print("✓ Smoothing pipeline: PASSED")


def run_all_tests():
    """Run all tests"""
    print("=" * 60)
    print("AQUABC Plotting Test Suite")
    print("=" * 60)
    print()

    test_classes = [
        TestBinaryFileReading(),
        TestTextFileReading(),
        TestCSVReading(),
        TestDateConversion(),
        TestPlotlyFigureCreation(),
        TestDataSmoothing(),
        TestRealOutputFiles(),
        TestFullPlotPipeline(),
    ]

    passed = 0
    failed = 0

    for test_class in test_classes:
        class_name = test_class.__class__.__name__
        print(f"\n--- {class_name} ---")

        # Get all test methods
        test_methods = [m for m in dir(test_class) if m.startswith('test_')]

        for method_name in test_methods:
            try:
                method = getattr(test_class, method_name)
                method()
                passed += 1
            except AssertionError as e:
                print(f"✗ {method_name}: FAILED - {e}")
                failed += 1
            except Exception as e:
                print(f"✗ {method_name}: ERROR - {e}")
                failed += 1

    print()
    print("=" * 60)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 60)

    return failed == 0


if __name__ == '__main__':
    import sys
    success = run_all_tests()
    sys.exit(0 if success else 1)
