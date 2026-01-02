program test_issue_1583_computed_goto
    use fortfront, only: transform_lazy_fortran_string, tooling_parse_options_t, &
                         tooling_load_ast_from_string, ast_arena_t, ast_to_json, &
                         token_t
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1583: Computed GOTO preservation ==='

    if (.not. test_computed_goto_basic()) all_passed = .false.
    if (.not. test_computed_goto_full()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1583 fixed!'
    else
        print *, 'Issue #1583 test failed!'
        stop 1
    end if

contains

    include '../../common/read_example.inc'


    logical function test_computed_goto_basic()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options

        test_computed_goto_basic = .true.
        print *, 'Testing basic computed goto preservation...'

        call read_example('examples/f90/issue_1583_computed_goto_basic.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_computed_goto_basic = .false.
                return
            end if
        end if

        ! Check that INVALID_LABEL is not present
        if (index(output, 'INVALID_LABEL') > 0) then
            print *, '  FAIL: INVALID_LABEL found in output'
            test_computed_goto_basic = .false.
        else
            print *, '  PASS: No INVALID_LABEL in output'
        end if

        ! Check if computed goto statement is preserved
        if (index(output, 'goto (100, 200, 300)') == 0 .and. &
            index(output, 'go to (100, 200, 300)') == 0) then
            print *, '  FAIL: computed goto statement missing'
            test_computed_goto_basic = .false.
        else
            print *, '  PASS: computed goto statement present'
        end if

        ! Check labels are preserved
        if (index(output, '100 print') > 0 .and. index(output, '200 print') > 0 .and. &
            index(output, '300 print') > 0 .and. index(output, '999 continue') > 0) then
            print *, '  PASS: all labels preserved'
        else
            print *, '  FAIL: labels missing'
            test_computed_goto_basic = .false.
        end if
    end function test_computed_goto_basic

    logical function test_computed_goto_full()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options
        type(ast_arena_t) :: arena
        integer :: root_index
        type(token_t), allocatable :: tokens(:)

        test_computed_goto_full = .true.
        print *, 'Testing computed goto AST generation...'

        call read_example('examples/f90/issue_1583_computed_goto_minimal.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          options, tokens)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: AST load error:', trim(error_msg)
                test_computed_goto_full = .false.
                return
            end if
        end if

        if (root_index > 0 .and. arena%size > 0) then
            print *, '  PASS: AST created successfully'
        else
            print *, '  FAIL: AST empty or invalid'
            test_computed_goto_full = .false.
        end if

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Transform error:', trim(error_msg)
                test_computed_goto_full = .false.
                return
            end if
        end if

        if (index(output, 'INVALID_LABEL') == 0) then
            print *, '  PASS: Output valid'
        else
            print *, '  FAIL: INVALID_LABEL in output'
            test_computed_goto_full = .false.
        end if
    end function test_computed_goto_full

end program test_issue_1583_computed_goto
