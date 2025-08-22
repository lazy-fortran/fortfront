/*
 * C++ Test Program for libfortfront.a Static Linking
 * 
 * **Given-When-Then**: Simple C++ program to verify linking with libfortfront.a
 * **Given**: libfortfront.a exists and provides C++-compatible interface
 * **When**: Compiling this program with g++ -static
 * **Then**: Should compile, link, and run without external dependencies
 */

#include <iostream>
#include <string>
#include <stdexcept>

/* 
 * NOTE: These declarations will fail in RED phase since C++ interface not implemented yet
 * The actual C++ interface will need to be created in the GREEN phase
 */

extern "C" {
    /* C-compatible interface to Fortran functions */
    int fortfront_initialize(void);
    int fortfront_parse_source(const char* source_code, int length);
    void fortfront_cleanup(void);
}

class FortfrontWrapper {
private:
    bool initialized;
    
public:
    FortfrontWrapper() : initialized(false) {
        if (fortfront_initialize() == 0) {
            initialized = true;
        } else {
            throw std::runtime_error("Failed to initialize fortfront library");
        }
    }
    
    ~FortfrontWrapper() {
        if (initialized) {
            fortfront_cleanup();
        }
    }
    
    bool parse_source(const std::string& source_code) {
        if (!initialized) {
            return false;
        }
        
        return fortfront_parse_source(source_code.c_str(), 
                                    static_cast<int>(source_code.length())) == 0;
    }
};

int main() {
    std::cout << "Testing libfortfront.a from C++..." << std::endl;
    
    try {
        /* This will fail in RED phase - C++ interface not implemented */
        FortfrontWrapper fortfront;
        
        /* Test basic parsing functionality */
        std::string test_code = "program hello\n  print *, 'Hello World'\nend program";
        
        if (!fortfront.parse_source(test_code)) {
            std::cerr << "FAIL: Could not parse test Fortran code" << std::endl;
            return 1;
        }
        
        std::cout << "PASS: C++ program successfully used libfortfront.a" << std::endl;
        return 0;
        
    } catch (const std::exception& e) {
        std::cerr << "FAIL: Exception occurred: " << e.what() << std::endl;
        return 1;
    }
}