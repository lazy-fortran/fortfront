#!/usr/bin/env python3
"""
Python wrapper for fortfront Fortran API using ctypes.
Provides direct access to the transform_lazy_fortran_string function.
"""

import ctypes
import os
from pathlib import Path
from typing import Tuple, Optional

class FortfrontAPI:
    """Wrapper for fortfront library API."""
    
    def __init__(self, library_path: Optional[Path] = None):
        """
        Initialize the fortfront API wrapper.
        
        Args:
            library_path: Path to the fortfront shared library.
                         If None, will search in standard locations.
        """
        self.lib = self._load_library(library_path)
        self._setup_functions()
    
    def _load_library(self, library_path: Optional[Path] = None) -> None:
        """Load the fortfront library (placeholder for future direct API use)."""
        # For now, we'll use fpm run which is more reliable
        # In the future, this could load a shared library directly
        self.use_fpm = True
        return None
    
    def _setup_functions(self):
        """Setup ctypes function signatures for Fortran API."""
        # The Fortran interface likely uses C bindings
        # We need to check the actual C interface signature
        pass
    
    def transform_lazy_fortran(self, source: str) -> Tuple[bool, str, str]:
        """
        Transform lazy Fortran source to standard Fortran.
        
        Args:
            source: Lazy Fortran source code
            
        Returns:
            Tuple of (success, output, error_msg)
        """
        import subprocess
        
        repo_root = Path(__file__).parent.parent
        
        # First, try to use the built binary directly (much faster)
        fortfront_binary = None
        build_dir = repo_root / "build"
        if build_dir.exists():
            for subdir in build_dir.iterdir():
                if subdir.is_dir():
                    app_binary = subdir / "app" / "fortfront"
                    if app_binary.exists() and os.access(app_binary, os.X_OK):
                        fortfront_binary = app_binary
                        break
        
        # Use the binary if found, otherwise fall back to fpm run
        if fortfront_binary:
            try:
                result = subprocess.run(
                    [str(fortfront_binary)],
                    input=source,
                    capture_output=True,
                    text=True,
                    timeout=10,
                    cwd=repo_root
                )
                
                if result.returncode == 0:
                    return True, result.stdout, ""
                else:
                    return False, result.stdout, result.stderr
                    
            except subprocess.TimeoutExpired:
                return False, "", "Timeout during compilation"
            except Exception as e:
                # Fall back to fpm if binary execution fails
                pass
        
        # Fallback: Use fpm run (slower but more reliable)
        try:
            # Check if fpm is available
            fpm_check = subprocess.run(["which", "fpm"], capture_output=True)
            if fpm_check.returncode != 0:
                # Try to find fpm in common locations
                fpm_paths = ["/home/ert/.local/bin/fpm", "/usr/local/bin/fpm", "fpm"]
                fpm_cmd = None
                for path in fpm_paths:
                    if os.path.exists(path) and os.access(path, os.X_OK):
                        fpm_cmd = path
                        break
                if not fpm_cmd:
                    return False, "", "fpm not found. Please install fpm or build fortfront."
            else:
                fpm_cmd = "fpm"
            
            result = subprocess.run(
                [fpm_cmd, "run", "fortfront", "--"],
                input=source,
                capture_output=True,
                text=True,
                timeout=10,
                cwd=repo_root
            )
            
            if result.returncode == 0:
                return True, result.stdout, ""
            else:
                return False, result.stdout, result.stderr
                
        except subprocess.TimeoutExpired:
            return False, "", "Timeout during compilation"
        except Exception as e:
            return False, "", str(e)


# Singleton instance
_api_instance = None

def get_fortfront_api() -> FortfrontAPI:
    """Get or create the singleton fortfront API instance."""
    global _api_instance
    if _api_instance is None:
        _api_instance = FortfrontAPI()
    return _api_instance

def transform_lazy_fortran_string(source: str) -> Tuple[bool, str, str]:
    """
    Transform lazy Fortran source to standard Fortran.
    
    This is a convenience function that uses the singleton API instance.
    
    Args:
        source: Lazy Fortran source code
        
    Returns:
        Tuple of (success, output, error_msg)
    """
    api = get_fortfront_api()
    return api.transform_lazy_fortran(source)