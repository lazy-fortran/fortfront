/*
 * C Interface for libfortfront.a Static Library
 * 
 * This header provides C-compatible interface to the Fortran fortfront library.
 * Enables external C and C++ programs to link with and use libfortfront.a.
 */

#ifndef FORTFRONT_C_H
#define FORTFRONT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Library initialization and cleanup
 */
int fortfront_initialize(void);
void fortfront_cleanup(void);

/*
 * Basic parsing interface
 */
int fortfront_parse_source(const char* source_code, int length);

/*
 * Retrieve the transformed (standardized) output of the most recent
 * successful fortfront_parse_source call. Returns a null-terminated string,
 * or NULL when no output is available. The returned pointer is owned by the
 * library and remains valid until the next parse or cleanup call.
 */
const char* fortfront_get_output(void);

/*
 * Error handling
 */
const char* fortfront_get_last_error(void);
void fortfront_clear_error(void);

/*
 * Library information
 */
const char* fortfront_get_version(void);
const char* fortfront_get_build_info(void);

#ifdef __cplusplus
}
#endif

#endif /* FORTFRONT_C_H */