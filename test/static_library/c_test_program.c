/*
 * C Test Program for libfortfront.a Static Linking
 * 
 * **Given-When-Then**: Simple C program to verify linking with libfortfront.a
 * **Given**: libfortfront.a exists and provides C-compatible interface
 * **When**: Compiling this program with gcc -static
 * **Then**: Should compile, link, and run without external dependencies
 */

#include <stdio.h>
#include <stdlib.h>

/* 
 * NOTE: These declarations will fail in RED phase since C interface not implemented yet
 * The actual C interface will need to be created in the GREEN phase
 */

/* Forward declarations for Fortran functions (C-compatible interface) */
extern int fortfront_initialize(void);
extern int fortfront_parse_source(const char* source_code, int length);
extern void fortfront_cleanup(void);

int main(void) {
    printf("Testing libfortfront.a from C...\n");
    
    /* This will fail in RED phase - C interface not implemented */
    int result = fortfront_initialize();
    if (result != 0) {
        fprintf(stderr, "FAIL: Could not initialize fortfront library\n");
        return 1;
    }
    
    /* Test basic parsing functionality */
    const char* test_code = "program hello\n  print *, 'Hello World'\nend program";
    result = fortfront_parse_source(test_code, strlen(test_code));
    if (result != 0) {
        fprintf(stderr, "FAIL: Could not parse test Fortran code\n");
        fortfront_cleanup();
        return 1;
    }
    
    fortfront_cleanup();
    printf("PASS: C program successfully used libfortfront.a\n");
    return 0;
}