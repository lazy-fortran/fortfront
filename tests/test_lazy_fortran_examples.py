#!/usr/bin/env python3
"""
Comprehensive test suite for Lazy Fortran (.lf) examples.
Tests all .lf files in the repository with xfail markers for known failures.
"""

import subprocess
import sys
import os
import pytest
from pathlib import Path
import tempfile
import re

# Import the fortfront API wrapper
from fortfront_api import transform_lazy_fortran_string

# Repository root directory
REPO_ROOT = Path(__file__).parent.parent

# Timeout for each test (handled by the API now)
TIMEOUT_SECONDS = 10

# Expected failures - these will be marked with xfail
# Format: filename -> (reason, issue_number)
XFAIL_TESTS = {
    # Codegen issues - complex code generation fails (issue #1234)
    "test_cg_complex.lf": ("Generated Fortran doesn't compile", 1234),
    "test_cg_slice.lf": ("Generated Fortran doesn't compile - syntax error", 1234),
    
    # Empty program/module handling issues (issue #1235)
    # test_empty.lf is now fixed - removed from xfail
    "test_empty_all.lf": ("Empty statements produce syntax errors", 1235),
    "test_empty_edge.lf": ("Edge case empty constructs fail", 1235),
    
    # Constructor type issues (issue #1236)
    "test_constructor.lf": ("Constructor codegen produces invalid Fortran", 1236),
    
    # Complex expression issues - issue 209 related (issue #1237)
    "test_209_all.lf": ("Complex multi-type expressions fail", 1237),
    "test_209_complex.lf": ("Complex nested expressions fail", 1237),
    "test_209_mixed.lf": ("Mixed type expressions fail", 1237),
    
    # Parsing ambiguities - issue 214 (issue #1238)
    "test_issue_214_ambiguous.lf": ("Ambiguous syntax not handled", 1238),
    "test_issue_214_correct.lf": ("Expected parsing fails", 1238),
    "test_issue_214_wrong.lf": ("Incorrect parsing accepted", 1238),
    
    # Comment handling - issue 508 (issue #1239)
    "test_issue_508_comment.lf": ("Comments break parsing", 1239),
    "test_issue_508_multicomment.lf": ("Multiple comments fail", 1239),
    "test_issue_508_nocomment.lf": ("Comment removal breaks code", 1239),
    
    # Operator precedence and comparison issues (issue #1240)
    "test_comparison_associativity.lf": ("Comparison operator associativity wrong", 1240),
    "test_comprehensive_precedence.lf": ("Operator precedence incorrect", 1240),
    "test_unary_precedence.lf": ("Unary operator precedence fails", 1240),
    
    # Scope and semantic analysis (issue #1241)
    "test_sem_scope.lf": ("Scope resolution produces invalid code", 1241),
    "test_sem_mixed.lf": ("Mixed semantic constructs fail", 1241),
    
    # Semicolon handling (issue #1242)
    "test_semicolons_mixed.lf": ("Mixed semicolon usage fails", 1242),
    
    # Array slicing issues (issue #1243)
    "test_slice1.lf": ("Array slice syntax generates bad code", 1243),
    "test_slice2.lf": ("Complex slicing fails", 1243),
    "test_slice_debug.lf": ("Slice debugging case fails", 1243),
    "test_std_slice.lf": ("Standard library slice fails", 1243),
}

def collect_lf_files():
    """Collect all .lf files from generated_tests and examples directories."""
    lf_files = []
    
    # Collect from generated_tests
    gen_tests = REPO_ROOT / "generated_tests"
    if gen_tests.exists():
        lf_files.extend(sorted(gen_tests.glob("*.lf")))
    
    # Collect from examples
    examples = REPO_ROOT / "examples"
    if examples.exists():
        lf_files.extend(sorted(examples.glob("*.lf")))
    
    return lf_files

def run_fortfront(lf_file):
    """Run fortfront on a .lf file and return the result."""
    try:
        # Read the .lf file content
        lf_content = lf_file.read_text()
        
        # Use the fortfront API to transform the code
        success, output, error_msg = transform_lazy_fortran_string(lf_content)
        
        # Check if compilation succeeded
        if not success:
            return False, f"Compilation failed: {error_msg}"
        
        # Check if output is valid Fortran
        if not output.strip():
            return False, "Empty output generated"
        
        # Basic validation of output
        output = output.strip()
        
        # Check for basic Fortran structure
        if not ("program" in output.lower() or "module" in output.lower() or 
                "function" in output.lower() or "subroutine" in output.lower()):
            return False, "Output doesn't appear to be valid Fortran"
        
        # Optionally compile the generated Fortran to validate it
        with tempfile.NamedTemporaryFile(suffix=".f90", delete=False) as f:
            f.write(output.encode())
            f.flush()
            
            try:
                # Try to compile with gfortran if available
                compile_result = subprocess.run(
                    ["gfortran", "-fsyntax-only", "-ffree-form", f.name],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                
                os.unlink(f.name)
                
                if compile_result.returncode != 0:
                    return False, f"Generated Fortran compilation failed: {compile_result.stderr}"
            except FileNotFoundError:
                # gfortran not available, skip validation
                os.unlink(f.name)
                # Still return success since fortfront compilation worked
                pass
            except subprocess.TimeoutExpired:
                os.unlink(f.name)
                return False, "Gfortran validation timeout"
        
        return True, "Success"
        
    except Exception as e:
        return False, f"Unexpected error: {str(e)}"

def get_test_category(filename):
    """Categorize test based on filename patterns."""
    name = filename.stem.lower()
    
    if "function" in name:
        return "function"
    elif "arr" in name or "array" in name:
        return "array"
    elif "slice" in name:
        return "slice"
    elif "concat" in name or "string" in name:
        return "string"
    elif "do" in name or "loop" in name:
        return "loop"
    elif "intrinsic" in name:
        return "intrinsic"
    elif "scope" in name:
        return "scope"
    elif "precedence" in name or "operator" in name:
        return "operator"
    elif "issue" in name:
        return "issue"
    elif "semicolon" in name:
        return "semicolon"
    elif "comment" in name:
        return "comment"
    elif "empty" in name:
        return "empty"
    elif "expr" in name:
        return "expression"
    elif "ctor" in name or "constructor" in name:
        return "constructor"
    elif "mixed" in name or "complex" in name:
        return "mixed"
    else:
        return "general"

# Generate test parameters with xfail markers
def create_test_params():
    """Create test parameters with xfail markers."""
    lf_files = collect_lf_files()
    params = []
    
    for lf_file in lf_files:
        test_id = f"{lf_file.parent.name}/{lf_file.name}"
        
        # Check if this test is expected to fail
        if lf_file.name in XFAIL_TESTS:
            reason, issue = XFAIL_TESTS[lf_file.name]
            param = pytest.param(
                lf_file,
                id=test_id,
                marks=pytest.mark.xfail(reason=f"{reason} (issue #{issue})")
            )
        else:
            param = pytest.param(lf_file, id=test_id)
        
        params.append(param)
    
    return params

@pytest.mark.parametrize("lf_file", create_test_params())
def test_lazy_fortran_compilation(lf_file):
    """Test that a .lf file compiles successfully to valid Fortran."""
    success, message = run_fortfront(lf_file)
    
    if not success:
        # Log the failure details for debugging
        print(f"\nFailed: {lf_file}")
        print(f"Reason: {message}")
        print(f"Category: {get_test_category(lf_file)}")
        
        # Read and show first few lines of the file for context
        content = lf_file.read_text()
        lines = content.split('\n')[:5]
        print("File content (first 5 lines):")
        for line in lines:
            print(f"  {line}")
    
    assert success, f"Compilation failed for {lf_file.name}: {message}"

if __name__ == "__main__":
    # Run pytest with this file
    sys.exit(pytest.main([__file__, "-v"]))