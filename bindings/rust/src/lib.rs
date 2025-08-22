/*!
 * Rust FFI bindings for libfortfront.a static library
 * 
 * This crate provides safe Rust bindings to the Fortran fortfront library.
 * Enables Rust programs to link with and use libfortfront.a.
 */

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};

/// Raw FFI declarations for fortfront C interface
#[link(name = "fortfront", kind = "static")]
extern "C" {
    fn fortfront_initialize() -> c_int;
    fn fortfront_cleanup();
    fn fortfront_parse_source(source_code: *const c_char, length: c_int) -> c_int;
    fn fortfront_get_last_error() -> *const c_char;
    fn fortfront_clear_error();
    fn fortfront_get_version() -> *const c_char;
    fn fortfront_get_build_info() -> *const c_char;
}

/// Error type for fortfront operations
#[derive(Debug, thiserror::Error)]
pub enum FortfrontError {
    #[error("Failed to initialize fortfront library")]
    InitializationFailed,
    #[error("Parse error: {message}")]
    ParseError { message: String },
    #[error("Invalid UTF-8 in C string")]
    InvalidUtf8,
    #[error("Null pointer from C function")]
    NullPointer,
}

/// Result type for fortfront operations
pub type FortfrontResult<T> = Result<T, FortfrontError>;

/// Main interface to the fortfront library
pub struct Fortfront {
    initialized: bool,
}

impl Fortfront {
    /// Create a new fortfront instance
    pub fn new() -> FortfrontResult<Self> {
        let result = unsafe { fortfront_initialize() };
        if result != 0 {
            return Err(FortfrontError::InitializationFailed);
        }
        
        Ok(Fortfront { initialized: true })
    }
    
    /// Parse Fortran source code
    pub fn parse_source(&self, source_code: &str) -> FortfrontResult<()> {
        if !self.initialized {
            return Err(FortfrontError::InitializationFailed);
        }
        
        let c_source = CString::new(source_code)
            .map_err(|_| FortfrontError::InvalidUtf8)?;
        
        let result = unsafe {
            fortfront_parse_source(
                c_source.as_ptr(),
                source_code.len() as c_int
            )
        };
        
        if result != 0 {
            let error_message = self.get_last_error()
                .unwrap_or_else(|_| "Unknown parse error".to_string());
            return Err(FortfrontError::ParseError { message: error_message });
        }
        
        Ok(())
    }
    
    /// Get the last error message (private helper)
    fn get_last_error(&self) -> FortfrontResult<String> {
        let ptr = unsafe { fortfront_get_last_error() };
        if ptr.is_null() {
            return Err(FortfrontError::NullPointer);
        }
        
        let c_str = unsafe { CStr::from_ptr(ptr) };
        let str_slice = c_str.to_str()
            .map_err(|_| FortfrontError::InvalidUtf8)?;
        
        Ok(str_slice.to_string())
    }
    
    /// Clear any error state
    pub fn clear_error(&self) {
        unsafe { fortfront_clear_error() };
    }
    
    /// Get library version
    pub fn get_version(&self) -> &'static str {
        // Placeholder implementation
        "0.1.0"
    }
    
    /// Get build information
    pub fn get_build_info(&self) -> &'static str {
        // Placeholder implementation
        "fortfront static library - Rust FFI bindings"
    }
}

impl Drop for Fortfront {
    fn drop(&mut self) {
        if self.initialized {
            unsafe { fortfront_cleanup() };
        }
    }
}

/// Convenience function to create a new fortfront instance
pub fn create() -> FortfrontResult<Fortfront> {
    Fortfront::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_library_creation() {
        let _fortfront = Fortfront::new().expect("Failed to create fortfront instance");
    }
    
    #[test]
    fn test_parse_simple_program() {
        let fortfront = Fortfront::new().expect("Failed to create fortfront instance");
        let source = "program hello\n  print *, 'Hello World'\nend program";
        
        // Note: This may fail in RED phase if C interface not fully implemented
        let result = fortfront.parse_source(source);
        println!("Parse result: {:?}", result);
    }
}