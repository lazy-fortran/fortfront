program sample_fluff_tool
    use fortfront_core           ! Direct Fortran module usage
    use fortfront_semantic       ! No C API needed
    use fortfront_ast_arena      ! Pure Fortran interfaces
    implicit none
    
    character(len=*), parameter :: source = "print *, 'Hello'"
    ! ast = fortfront_parse(source)  ! Pure Fortran function calls
    ! call fluff_analyze(ast)        ! All Fortran, all the time
    print *, "Sample fluff tool would work here"
end program sample_fluff_tool
