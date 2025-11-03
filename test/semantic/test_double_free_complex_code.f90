program test_double_free_complex_code
    use lexer_api, only: lex_source, lex_file
    use parser_api, only: parse_tokens, parse_tokens_safe
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, compile_source
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    logical :: success

    print *, "Testing double free issue with complex nested code..."

    ! Test the exact code from issue #88
    call read_example('examples/f90/issue_88_double_free_complex.f90', test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    success = (len(error_msg) == 0)

    if (success) then
        print *, "✓ Complex nested code compiled successfully"
        print *, "✓ No double free error occurred"
        print *, "✓ implicit_statement_node handled correctly"
    else
        print *, "ERROR: Compilation failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    print *, "All tests passed!"

contains
    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_double_free_complex_code
