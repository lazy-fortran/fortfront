/*
 * C++ Interface for libfortfront.a Static Library
 * 
 * This header provides C++ wrapper interface to the Fortran fortfront library.
 * Enables external C++ programs to use libfortfront.a with modern C++ features.
 */

#ifndef FORTFRONT_CPP_HPP
#define FORTFRONT_CPP_HPP

#include "fortfront_c.h"
#include <string>
#include <stdexcept>
#include <memory>

namespace fortfront {

    /*
     * Exception class for fortfront errors
     */
    class FortfrontError : public std::runtime_error {
    public:
        explicit FortfrontError(const std::string& message) 
            : std::runtime_error("Fortfront error: " + message) {}
    };

    /*
     * RAII wrapper for fortfront library
     */
    class Library {
    private:
        bool initialized_;
        
    public:
        Library() : initialized_(false) {
            if (fortfront_initialize() != 0) {
                throw FortfrontError("Failed to initialize fortfront library");
            }
            initialized_ = true;
        }
        
        ~Library() {
            if (initialized_) {
                fortfront_cleanup();
            }
        }
        
        // Delete copy constructor and assignment operator
        Library(const Library&) = delete;
        Library& operator=(const Library&) = delete;
        
        // Allow move construction and assignment
        Library(Library&& other) noexcept : initialized_(other.initialized_) {
            other.initialized_ = false;
        }
        
        Library& operator=(Library&& other) noexcept {
            if (this != &other) {
                if (initialized_) {
                    fortfront_cleanup();
                }
                initialized_ = other.initialized_;
                other.initialized_ = false;
            }
            return *this;
        }
        
        /*
         * Parse Fortran source code
         */
        bool parse_source(const std::string& source_code) {
            if (!initialized_) {
                throw FortfrontError("Library not initialized");
            }
            
            int result = fortfront_parse_source(source_code.c_str(), 
                                              static_cast<int>(source_code.length()));
            return result == 0;
        }
        
        /*
         * Get library version
         */
        std::string get_version() const {
            if (!initialized_) {
                throw FortfrontError("Library not initialized");
            }
            const char* version = fortfront_get_version();
            return version ? std::string(version) : "";
        }
        
        /*
         * Get build information
         */
        std::string get_build_info() const {
            if (!initialized_) {
                throw FortfrontError("Library not initialized");
            }
            const char* build_info = fortfront_get_build_info();
            return build_info ? std::string(build_info) : "";
        }
        
        /*
         * Check if library is initialized
         */
        bool is_initialized() const {
            return initialized_;
        }
    };

    /*
     * Convenience function to create library instance
     */
    inline std::unique_ptr<Library> create_library() {
        return std::make_unique<Library>();
    }

} // namespace fortfront

#endif /* FORTFRONT_CPP_HPP */