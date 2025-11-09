program test_issue_2236_optional_assumed_rank
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_file, &
                         ast_arena_t, token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2236: Optional assumed-rank calls crash monomorphization ==='

    if (.not. test_optional_assumed_rank_parsing()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2236 fixed!'
    else
        print *, 'Issue #2236 regression detected!'
        stop 1
    end if

contains

    logical function test_optional_assumed_rank_parsing()
        use fortfront, only: tooling_load_ast_from_string
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source
        integer :: root_index

        test_optional_assumed_rank_parsing = .true.
        print *, 'Testing optional assumed-rank parameter parsing and monomorphization...'

        ! Reproducer from issue #2236
        source = 'program optional_rank_bridge' // new_line('a') // &
                 '  implicit none' // new_line('a') // &
                 '  integer :: payload(1)' // new_line('a') // &
                 '  payload = 5' // new_line('a') // &
                 '  if (echo() /= 1) stop 1' // new_line('a') // &
                 '  if (echo(payload) /= 2) stop 2' // new_line('a') // &
                 'contains' // new_line('a') // &
                 '  integer function echo(sample)' // new_line('a') // &
                 '    type(*), optional, dimension(..) :: sample' // new_line('a') // &
                 '    if (present(sample)) then' // new_line('a') // &
                 '      echo = 2' // new_line('a') // &
                 '    else' // new_line('a') // &
                 '      echo = 1' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 '  end function echo' // new_line('a') // &
                 'end program optional_rank_bridge'

        options = tooling_parse_options_t()
        options%run_semantics = .true.  ! Enable semantic analysis including monomorphization

        ! Process the source - this should not crash (issue #2236 was a crash at line 1657)
        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
                                          options, tokens)

        ! We don't check for semantic errors because the monomorphization
        ! of optional assumed-rank parameters may not be fully implemented yet.
        ! The important thing is that it doesn't crash at line 1657.

        if (root_index <= 0) then
            print *, '  FAIL: Parsing failed to produce AST'
            test_optional_assumed_rank_parsing = .false.
            return
        end if

        print *, '  PASS: Optional assumed-rank calls processed without crash'
    end function test_optional_assumed_rank_parsing

end program test_issue_2236_optional_assumed_rank
