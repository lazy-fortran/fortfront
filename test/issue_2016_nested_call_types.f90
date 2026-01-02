program test_issue_2016_nested_call_types
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_tooling_api
    use ast_arena_modern, only: ast_arena_t
    implicit none

    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: output
    character(len=:), allocatable :: source
    type(tooling_parse_options_t) :: opts

    call read_example('examples/lf/issue_2016_nested_call_type_mismatch.lf', &
                      source)

    opts%run_semantics = .true.
    call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                      opts)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'ERROR: ' // trim(error_msg)
        stop 1
    end if

    call generate_fortran_output(arena, root_index, output)

    if (index(output, 'integer function square') == 0) then
        write (error_unit, '(A)') 'FAIL: square function should return integer'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    if (index(output, 'integer, intent(in) :: x') == 0 .and. &
        index(output, 'integer :: x') == 0) then
        write (error_unit, '(A)') 'FAIL: square parameter x should be integer'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    if (index(output, ' real ') > 0) then
        write (error_unit, '(A)') 'FAIL: output should not contain real types'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    print *, 'PASS: nested call types inferred correctly'

contains

    include 'common/read_example.inc'


    subroutine generate_fortran_output(arena, prog_index, fortran_code)
        use codegen_core, only: codegen_core_generate_arena, initialize_codegen
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable, intent(out) :: fortran_code

        call initialize_codegen()
        fortran_code = codegen_core_generate_arena(arena, prog_index)
    end subroutine generate_fortran_output

end program test_issue_2016_nested_call_types
