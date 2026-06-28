program test_issue_1696_stop_statements
    use fortfront, only: transform_lazy_fortran_string, tooling_parse_options_t, &
        tooling_load_ast_from_string, ast_arena_t, token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1696: STOP statement preservation ==='

    if (.not. test_stop_statement_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1696 verified!'
    else
        print *, 'Issue #1696 test failed!'
        stop 1
    end if

contains

    include '../../common/read_example.inc'

    logical function test_stop_statement_preservation()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        test_stop_statement_preservation = .true.
        print *, 'Testing STOP statement preservation...'

        call read_example('examples/f90/issue_1696_stop_statements.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options, tokens)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: AST load error:', trim(error_msg)
                test_stop_statement_preservation = .false.
                return
            end if
        end if

        if (root_index > 0 .and. arena%size > 0) then
            print *, '  PASS: AST created successfully'
        else
            print *, '  FAIL: AST empty or invalid'
            test_stop_statement_preservation = .false.
            return
        end if

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_stop_statement_preservation = .false.
                return
            end if
        end if

        if (index(output, "stop 'Error: x is too large'") == 0) then
            print *, '  FAIL: STOP statement missing from output'
            test_stop_statement_preservation = .false.
        else
            print *, '  PASS: STOP statement preserved'
        end if
    end function test_stop_statement_preservation
end program test_issue_1696_stop_statements
