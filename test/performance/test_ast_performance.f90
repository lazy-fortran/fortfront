program test_ast_performance
    ! Legacy AST performance test - redirects to arena benchmarks
    ! Issue #400 - Arena performance benchmark implementation
    
    use iso_fortran_env, only: error_unit, output_unit
    implicit none

    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A)') "AST Performance Test Suite"
    write(output_unit, '(A)') "======================================="
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "This test has been replaced by comprehensive"
    write(output_unit, '(A)') "arena benchmarks in test_arena_benchmarks."
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Run: fpm test test_arena_benchmarks"
    write(output_unit, '(A)') "for complete performance analysis."
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "AST performance test PASSED (redirected)"
    write(output_unit, '(A)') "======================================="

end program test_ast_performance