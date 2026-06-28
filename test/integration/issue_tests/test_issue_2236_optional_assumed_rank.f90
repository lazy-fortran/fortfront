program test_issue_2236_optional_assumed_rank
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: tooling_parse_options_t, ast_arena_t, token_t
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

    include '../../common/read_example.inc'

    logical function test_optional_assumed_rank_parsing()
        use fortfront, only: tooling_load_ast_from_string
        type(ast_arena_t) :: arena
        type(tooling_parse_options_t) :: options
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: source
        integer :: root_index

        test_optional_assumed_rank_parsing = .true.
        print *, &
            'Testing optional assumed-rank parameter parsing and monomorphization...'

        call read_example('examples/f90/issue_2236_optional_assumed_rank_bridge.f90', &
            source)

        options = tooling_parse_options_t()
        options%run_semantics = .true.
        ! Enable semantic analysis including monomorphization.

        ! Process the source. This should not crash.
        ! Issue #2236 crashed during semantic analysis at line 1657.
        call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
            options, tokens)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A,1X,A)') '  FAIL: semantic error:', &
                    trim(error_msg)
                test_optional_assumed_rank_parsing = .false.
                return
            end if
        end if

        if (root_index <= 0) then
            print *, '  FAIL: Parsing failed to produce AST'
            test_optional_assumed_rank_parsing = .false.
            return
        end if

        print *, '  PASS: Optional assumed-rank calls processed without crash'
    end function test_optional_assumed_rank_parsing


end program test_issue_2236_optional_assumed_rank
