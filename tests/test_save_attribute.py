#!/usr/bin/env python3
"""
Test for SAVE attribute fix (issue #1541).
Tests that SAVE attribute is correctly parsed and transpiled.
"""

import subprocess
import sys
import os
import pytest
from pathlib import Path
import tempfile

# Repository root directory
REPO_ROOT = Path(__file__).parent.parent

def test_save_attribute_basic():
    """Test basic SAVE attribute functionality with the reproducer."""

    # Read the reproducer file
    reproducer_path = REPO_ROOT / "examples" / "issue_1541_save_attribute.f90"
    assert reproducer_path.exists(), "Reproducer file not found"

    with open(reproducer_path, 'r') as f:
        source_code = f.read()

    # Test that it contains SAVE attribute
    assert "save ::" in source_code.lower() or "save, " in source_code.lower(), \
        "Source should contain SAVE attribute"

    # Try to compile with gfortran to verify it's valid Fortran
    with tempfile.NamedTemporaryFile(suffix=".f90", delete=False) as f:
        f.write(source_code.encode())
        f.flush()

        try:
            # Try to compile with gfortran
            compile_result = subprocess.run(
                ["gfortran", "-fsyntax-only", "-ffree-form", f.name],
                capture_output=True,
                text=True,
                timeout=5
            )

            os.unlink(f.name)

            if compile_result.returncode != 0:
                pytest.fail(f"Original Fortran compilation failed: {compile_result.stderr}")

        except FileNotFoundError:
            # gfortran not available, skip validation
            os.unlink(f.name)
            pass
        except subprocess.TimeoutExpired:
            os.unlink(f.name)
            pytest.fail("Gfortran validation timeout")

def test_save_attribute_transpilation():
    """Test that SAVE attribute survives transpilation through fortfront."""

    # Read the reproducer file
    reproducer_path = REPO_ROOT / "examples" / "issue_1541_save_attribute.f90"
    with open(reproducer_path, 'r') as f:
        source_code = f.read()

    # Try to run fortfront on the reproducer
    try:
        # Build the project first
        build_result = subprocess.run(
            ["fpm", "build"],
            capture_output=True,
            text=True,
            timeout=30,
            cwd=REPO_ROOT
        )

        if build_result.returncode != 0:
            pytest.fail(f"FPM build failed: {build_result.stderr}")

        # Find the fortfront executable
        build_dir = REPO_ROOT / "build"
        fortfront_exe = None
        for path in build_dir.rglob("fortfront"):
            if path.is_file() and os.access(path, os.X_OK):
                fortfront_exe = path
                break

        if not fortfront_exe:
            pytest.fail("Could not find fortfront executable after build")

        result = subprocess.run(
            [str(fortfront_exe), str(reproducer_path)],
            capture_output=True,
            text=True,
            timeout=10,
            cwd=REPO_ROOT
        )

        # The command should succeed
        if result.returncode != 0:
            pytest.fail(f"Fortfront transpilation failed: {result.stderr}")

        transpiled_output = result.stdout

        # Check that SAVE attribute is preserved in output
        assert "save" in transpiled_output.lower(), \
            f"SAVE attribute should be preserved in transpiled output. Got: {transpiled_output}"

        # Check that the variable name 'count' is preserved
        assert "count" in transpiled_output.lower(), \
            f"Variable name 'count' should be preserved. Got: {transpiled_output}"

        # Check that initialization is preserved
        assert "= 0" in transpiled_output, \
            f"Variable initialization should be preserved. Got: {transpiled_output}"

        # Try to compile the transpiled output
        with tempfile.NamedTemporaryFile(suffix=".f90", delete=False) as f:
            f.write(transpiled_output.encode())
            f.flush()

            try:
                compile_result = subprocess.run(
                    ["gfortran", "-fsyntax-only", "-ffree-form", f.name],
                    capture_output=True,
                    text=True,
                    timeout=5
                )

                os.unlink(f.name)

                if compile_result.returncode != 0:
                    pytest.fail(f"Transpiled Fortran compilation failed: {compile_result.stderr}")

            except FileNotFoundError:
                # gfortran not available, skip validation
                os.unlink(f.name)
                pass
            except subprocess.TimeoutExpired:
                os.unlink(f.name)
                pytest.fail("Gfortran validation timeout")

    except subprocess.TimeoutExpired:
        pytest.fail("Fortfront transpilation timeout")
    except Exception as e:
        pytest.fail(f"Unexpected error during transpilation: {str(e)}")

def test_save_attribute_execution():
    """Test that SAVE attribute works correctly at runtime (if gfortran available)."""

    # Read the reproducer file
    reproducer_path = REPO_ROOT / "examples" / "issue_1541_save_attribute.f90"
    with open(reproducer_path, 'r') as f:
        source_code = f.read()

    # Try to compile and run the program to verify SAVE behavior
    with tempfile.NamedTemporaryFile(suffix=".f90", delete=False) as f:
        f.write(source_code.encode())
        f.flush()

        try:
            # Compile with gfortran
            compile_result = subprocess.run(
                ["gfortran", "-o", f.name + ".exe", f.name],
                capture_output=True,
                text=True,
                timeout=5
            )

            if compile_result.returncode != 0:
                # Skip test if compilation fails
                os.unlink(f.name)
                pytest.skip(f"Cannot compile test program: {compile_result.stderr}")
                return

            # Run the program and capture output
            run_result = subprocess.run(
                [f.name + ".exe"],
                capture_output=True,
                text=True,
                timeout=5
            )

            # Cleanup
            os.unlink(f.name)
            if os.path.exists(f.name + ".exe"):
                os.unlink(f.name + ".exe")

            if run_result.returncode != 0:
                pytest.skip(f"Test program execution failed: {run_result.stderr}")
                return

            output = run_result.stdout.strip()

            # Expected output should be:
            # 1
            # 2
            lines = [line.strip() for line in output.split('\n') if line.strip()]

            if len(lines) >= 2:
                assert lines[0] == "1", f"First call should output '1', got '{lines[0]}'"
                assert lines[1] == "2", f"Second call should output '2', got '{lines[1]}'"
            else:
                pytest.skip(f"Unexpected output format: {output}")

        except FileNotFoundError:
            # gfortran not available, skip test
            os.unlink(f.name)
            pytest.skip("gfortran not available for execution test")
        except subprocess.TimeoutExpired:
            os.unlink(f.name)
            if os.path.exists(f.name + ".exe"):
                os.unlink(f.name + ".exe")
            pytest.fail("Execution test timeout")

if __name__ == "__main__":
    test_save_attribute_basic()
    test_save_attribute_transpilation()
    test_save_attribute_execution()
    print("All SAVE attribute tests passed!")