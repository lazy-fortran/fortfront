program test_issue_1546_goto_label
    use fortfront, only: transform_lazy_fortran_string, tooling_parse_options_t, &
                         tooling_load_ast_from_string, ast_arena_t, ast_to_json, &
                         token_t
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

    logical function test_goto_label_preservation()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: ast_json
        type(token_t), allocatable :: tokens(:)
        integer :: root_index, k, print_count

        test_goto_label_preservation = .true.
        print *, 'Testing goto label preservation...'

        source = 'program demo' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: i' // new_line('a') // &
                 '    i = 0' // new_line('a') // &
                 '10  i = i + 1' // new_line('a') // &
                 '    if (i < 3) goto 10' // new_line('a') // &
                 '    stop' // new_line('a') // &
                 'end program demo'

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

        if (index(ast_json, '"type":"goto"') == 0) then
            print *, '  FAIL: goto node missing in AST'
            test_goto_label_preservation = .false.
        else
            print *, '  PASS: goto node present in AST'
        end if

        print *, '  Tokens around goto:'
        print_count = min(size(tokens), 30)
        do k = 1, print_count
            print *, '    ', k, tokens(k)%kind, trim(tokens(k)%text)
        end do

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_goto_label_preservation = .false.
                return
            end if
        end if

        if (index(output, 'go to 10') == 0) then
            print *, '  FAIL: goto target missing in output'
            test_goto_label_preservation = .false.
        else
            print *, '  PASS: goto target preserved'
        end if

        if (index(output, '10  i = i + 1') == 0 .and. &
            index(output, '10 i = i + 1') == 0) then
            print *, '  FAIL: numeric label missing in output'
            test_goto_label_preservation = .false.
        else
            print *, '  PASS: numeric label preserved'
        end if
    end function test_goto_label_preservation

end program test_issue_1546_goto_label
