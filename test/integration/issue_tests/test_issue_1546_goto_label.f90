program test_issue_1546_goto_label
    use fortfront, only: transform_lazy_fortran_string, tooling_parse_options_t, &
                         tooling_load_ast_from_string, ast_arena_t, ast_to_json, &
                         token_t
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1546: GOTO label preservation ==='

    if (.not. test_goto_label_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1546 fixed!'
    else
        print *, 'Issue #1546 test failed!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function test_goto_label_preservation()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: ast_json
        type(token_t), allocatable :: tokens(:)
        integer :: root_index, k, print_count

        test_goto_label_preservation = .true.
        print *, 'Testing goto label preservation...'

        call read_example('examples/f90/issue_1546_goto_label.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          options, tokens)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: AST load error:', trim(error_msg)
                test_goto_label_preservation = .false.
                return
            end if
        end if

        call ast_to_json(arena, root_index, ast_json)

        ! Check if AST was created successfully
        if (root_index > 0 .and. arena%size > 0) then
            print *, '  PASS: AST created successfully'
        else
            print *, '  FAIL: AST empty or invalid'
            test_goto_label_preservation = .false.
        end if

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_goto_label_preservation = .false.
                return
            end if
        end if

        ! Check if goto statement is present in output
        if (index(output, 'go to 10') == 0 .and. index(output, 'goto 10') == 0) then
            print *, '  FAIL: goto statement missing in output'
            test_goto_label_preservation = .false.
        else
            print *, '  PASS: goto statement present'
        end if

        ! Check for numeric label
        if (index(output, '10  i = i + 1') == 0 .and. &
            index(output, '10 i = i + 1') == 0) then
            print *, '  FAIL: numeric label missing in output'
            test_goto_label_preservation = .false.
        else
            print *, '  PASS: numeric label preserved'
        end if
    end function test_goto_label_preservation

end program test_issue_1546_goto_label
