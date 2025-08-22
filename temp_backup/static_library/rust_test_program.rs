/*
 * Rust Test Program for libfortfront.a Static Linking
 * 
 * **Given-When-Then**: Simple Rust program to verify linking with libfortfront.a
 * **Given**: libfortfront.a exists and provides C-compatible FFI interface
 * **When**: Compiling this program with cargo build
 * **Then**: Should compile, link, and run without external dependencies
 */

use std::ffi::{CString, c_char, c_int};
use std::ptr;

/* 
 * NOTE: These extern declarations will fail in RED phase since C FFI interface not implemented yet
 * The actual FFI bindings will need to be created in the GREEN phase
 */

extern "C" {
    fn fortfront_initialize() -> c_int;
    fn fortfront_parse_source(source_code: *const c_char, length: c_int) -> c_int;
    fn fortfront_cleanup();
}

struct FortfrontLibrary {
    initialized: bool,
}

impl FortfrontLibrary {
    fn new() -> Result<Self, String> {
        unsafe {
            if fortfront_initialize() == 0 {
                Ok(FortfrontLibrary { initialized: true })
            } else {
                Err("Failed to initialize fortfront library".to_string())
            }
        }
    }
    
    fn parse_source(&self, source_code: &str) -> Result<(), String> {
        if !self.initialized {
            return Err("Library not initialized".to_string());
        }
        
        let c_source = CString::new(source_code)
            .map_err(|_| "Invalid source code string")?;
        
        unsafe {
            if fortfront_parse_source(c_source.as_ptr(), source_code.len() as c_int) == 0 {
                Ok(())
            } else {
                Err("Failed to parse source code".to_string())
            }
        }
    }
}

impl Drop for FortfrontLibrary {
    fn drop(&mut self) {
        if self.initialized {
            unsafe {
                fortfront_cleanup();
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing libfortfront.a from Rust...");
    
    // This will fail in RED phase - FFI interface not implemented
    let fortfront = FortfrontLibrary::new()?;
    
    // Test basic parsing functionality
    let test_code = "program hello\n  print *, 'Hello World'\nend program";
    
    fortfront.parse_source(test_code)?;
    
    println!("PASS: Rust program successfully used libfortfront.a");
    Ok(())
}