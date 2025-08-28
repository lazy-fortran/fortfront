#!/usr/bin/env cmake -P

# Test discovery validation script for Issue #768
# Tests the difference between GLOB and GLOB_RECURSE for test/*.f90

message(STATUS "=== Test Discovery Pattern Validation ===")

# Test old shallow pattern
file(GLOB SHALLOW_TESTS "test/*.f90")
list(LENGTH SHALLOW_TESTS SHALLOW_COUNT)
message(STATUS "Shallow pattern 'test/*.f90' found: ${SHALLOW_COUNT} files")

# Test new recursive pattern  
file(GLOB_RECURSE RECURSIVE_TESTS "test/*.f90")
list(LENGTH RECURSIVE_TESTS RECURSIVE_COUNT)
message(STATUS "Recursive pattern 'test/*.f90' found: ${RECURSIVE_COUNT} files")

message(STATUS "=== Improvement Analysis ===")
math(EXPR IMPROVEMENT "${RECURSIVE_COUNT} - ${SHALLOW_COUNT}")
message(STATUS "Additional tests discovered: ${IMPROVEMENT}")
message(STATUS "Total test coverage: ${RECURSIVE_COUNT}/243 expected")

if(RECURSIVE_COUNT EQUAL 243)
    message(STATUS "✅ SUCCESS: All 243 tests discovered!")
else()
    message(WARNING "❌ ISSUE: Expected 243, found ${RECURSIVE_COUNT}")
endif()

message(STATUS "=== Issue #768 Fix Validation Complete ===")