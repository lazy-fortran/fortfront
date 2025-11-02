program test_issue_2016_nested_call_types
    use frontend_tooling_api
    use ast_arena_modern, only: ast_arena_t
    implicit none

    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg, output
    character(len=:), allocatable :: source
    type(tooling_parse_options_t) :: opts

    call read_example('examples/lf/issue_2016_nested_call_type_mismatch.lf', source)

    opts%run_semantics = .true.
    call tooling_load_ast_from_string(source, arena, root_index, error_msg, opts)

    if (len_trim(error_msg) > 0) then
        print *, 'ERROR:', error_msg
        stop 1
    end if

    call generate_fortran_output(arena, root_index, output)

    ! Check that square function has integer parameter, not real
    if (index(output, 'integer function square') == 0) then
        print *, 'FAIL: square function should return integer'
        print *, 'Output:', output
        stop 1
    end if

    if (index(output, 'integer, intent(in) :: x') == 0 .and. &
        index(output, 'integer :: x') == 0) then
        print *, 'FAIL: square parameter x should be integer'
        print *, 'Output:', output
        stop 1
    end if

    if (index(output, 'real') > 0) then
        print *, 'FAIL: should not have any real types'
        print *, 'Output:', output
        stop 1
    end if

    print *, 'PASS: nested call types inferred correctly'

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, iostat, file_size
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', action='read', &
              form='unformatted', access='stream', iostat=iostat)
        if (iostat /= 0) then
            print *, 'Cannot open file:', filepath
            stop 1
        end if

        inquire (unit=unit, size=file_size)
        allocate (character(len=file_size) :: content)
        allocate (buffer(file_size))
        read (unit, iostat=iostat) buffer
        close (unit)

        if (iostat /= 0) then
            print *, 'Cannot read file:', filepath
            stop 1
        end if

        content = transfer(buffer, content)
    end subroutine read_example

    subroutine generate_fortran_output(arena, prog_index, fortran_code)
        use codegen_core, only: codegen_core_generate_arena, initialize_codegen
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: fortran_code

        call initialize_codegen()
        fortran_code = codegen_core_generate_arena(arena, prog_index)
    end subroutine generate_fortran_output

end program test_issue_2016_nested_call_types
