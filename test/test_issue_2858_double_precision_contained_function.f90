program test_issue_2858_double_precision_contained_function
    use fortfront, only: parse_tokens, ast_arena_t, create_ast_arena, &
        lex_source, token_t, ast_node, function_def_node
    implicit none

    logical :: all_passed
    character(len=:), allocatable :: ref_name, ref_return_type

    all_passed = .true.

    ! A program-contained double precision function must parse to a named
    ! function_def with return_type "double precision", matching the module case,
    ! not the unnamed_function placeholder node reported in issue #2858.
    if (.not. probe_function( &
        'program p' // new_line('A') // &
        '  print *, d()' // new_line('A') // &
        'contains' // new_line('A') // &
        '  double precision function d()' // new_line('A') // &
        '    d = 1.5d0' // new_line('A') // &
        '  end function' // new_line('A') // &
        'end program', ref_name, ref_return_type)) then
        print *, '  FAIL: contained function did not parse'
        all_passed = .false.
    else
        if (ref_name /= 'd') then
            print *, '  FAIL: contained name "', ref_name, '" expected "d"'
            all_passed = .false.
        end if
        if (ref_return_type /= 'double precision') then
            print *, '  FAIL: contained return_type "', ref_return_type, &
                '" expected "double precision"'
            all_passed = .false.
        end if
    end if

    ! A file-scope external double precision function must parse identically to a
    ! real(8) external function (a named "d" function_def, not a placeholder).
    block
        character(len=:), allocatable :: dp_name, dp_rt, r8_name, r8_rt
        logical :: dp_ok, r8_ok
        dp_ok = probe_function( &
            'double precision function d()' // new_line('A') // &
            '  d = 1.5d0' // new_line('A') // &
            'end function', dp_name, dp_rt)
        r8_ok = probe_function( &
            'real(8) function d()' // new_line('A') // &
            '  d = 1.5d0' // new_line('A') // &
            'end function', r8_name, r8_rt)
        if (.not. (dp_ok .and. r8_ok)) then
            print *, '  FAIL: external function did not parse'
            all_passed = .false.
        else
            if (dp_name /= 'd') then
                print *, '  FAIL: external name "', dp_name, '" expected "d"'
                all_passed = .false.
            end if
            if (dp_name /= r8_name .or. dp_rt /= r8_rt) then
                print *, '  FAIL: external double precision differs from real(8)'
                all_passed = .false.
            end if
        end if
    end block

    if (all_passed) then
        print *, 'All issue #2858 tests passed'
        stop 0
    else
        error stop 1
    end if

contains

    ! Parse source, returning the name and return_type of the single named
    ! function_def in the arena. Fails if the placeholder unnamed_function node
    ! is produced or no named function_def is found.
    logical function probe_function(source, func_name, return_type) result(ok)
        character(len=*), intent(in) :: source
        character(len=:), allocatable, intent(out) :: func_name, return_type
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg
        integer :: prog_index, i

        ok = .false.
        func_name = ""
        return_type = ""

        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") return

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (function_def_node)
                if (trim(node%name) == 'unnamed_function') return
                func_name = trim(node%name)
                return_type = trim(node%return_type)
                ok = .true.
            end select
        end do
    end function probe_function

end program test_issue_2858_double_precision_contained_function
