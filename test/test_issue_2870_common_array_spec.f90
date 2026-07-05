program test_issue_2870_common_array_spec
    ! Regression for #2870: a COMMON member's array-spec, e.g.
    !   common /b/ myarr(10)
    ! must upgrade the companion type declaration to a rank-1 array with the
    ! given extent. Previously the "(10)" tokens were discarded and the
    ! declaration stayed scalar.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: declaration_node
    use ast_nodes_core, only: literal_node
    implicit none

    integer :: failures

    failures = 0

    call check_single_array()
    call check_mixed_members()

    if (failures /= 0) then
        print *, 'FAIL: ', failures, ' check(s) failed'
        error stop 1
    end if
    print *, 'PASS: COMMON array-spec upgrades companion declaration'

contains

    subroutine check_single_array()
        character(:), allocatable :: src
        type(ast_arena_t) :: arena

        src = 'program p'//new_line('a')// &
              '    implicit none'//new_line('a')// &
              '    integer myarr'//new_line('a')// &
              '    integer sc'//new_line('a')// &
              '    common /b/ myarr(10), sc'//new_line('a')// &
              'end program'

        arena = parse_src(src)
        call expect_array(arena, 'myarr', 10, 'single: myarr')
        call expect_scalar(arena, 'sc', 'single: sc')
    end subroutine check_single_array

    subroutine check_mixed_members()
        character(:), allocatable :: src
        type(ast_arena_t) :: arena

        src = 'program p'//new_line('a')// &
              '    implicit none'//new_line('a')// &
              '    integer a'//new_line('a')// &
              '    integer arr'//new_line('a')// &
              '    integer c'//new_line('a')// &
              '    common /b/ a, arr(3), c'//new_line('a')// &
              'end program'

        arena = parse_src(src)
        call expect_scalar(arena, 'a', 'mixed: a')
        call expect_array(arena, 'arr', 3, 'mixed: arr')
        call expect_scalar(arena, 'c', 'mixed: c')
    end subroutine check_mixed_members

    function parse_src(src) result(arena)
        character(len=*), intent(in) :: src
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(:), allocatable :: error_msg
        integer :: prog_index

        arena = create_ast_arena()
        call lex_source(src, tokens, error_msg)
        call fail_on_error(error_msg, 'lex')
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call fail_on_error(error_msg, 'parse')
    end function parse_src

    subroutine expect_array(arena, name, extent, label)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name, label
        integer, intent(in) :: extent
        integer :: idx, dim_idx, got
        logical :: found

        idx = find_declaration(arena, name)
        found = .false.
        if (idx > 0) then
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_array .and. allocated(decl%dimension_indices)) then
                    if (size(decl%dimension_indices) == 1) then
                        dim_idx = decl%dimension_indices(1)
                        got = literal_int(arena, dim_idx)
                        if (got == extent) found = .true.
                    end if
                end if
            end select
        end if
        if (.not. found) then
            print *, 'FAIL ', label, ': expected rank-1 extent ', extent
            failures = failures + 1
        end if
    end subroutine expect_array

    subroutine expect_scalar(arena, name, label)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name, label
        integer :: idx
        logical :: ok

        idx = find_declaration(arena, name)
        ok = .false.
        if (idx > 0) then
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                ok = .not. decl%is_array
            end select
        end if
        if (.not. ok) then
            print *, 'FAIL ', label, ': expected scalar declaration'
            failures = failures + 1
        end if
    end subroutine expect_scalar

    integer function find_declaration(arena, name) result(idx)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: i

        idx = 0
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (decl => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == name) then
                        idx = i
                        return
                    end if
                end if
            end select
        end do
    end function find_declaration

    integer function literal_int(arena, idx) result(val)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer :: io_stat

        val = -1
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return
        select type (lit => arena%entries(idx)%node)
        type is (literal_node)
            if (allocated(lit%value)) read (lit%value, *, iostat=io_stat) val
        end select
    end function literal_int

    subroutine fail_on_error(error_msg, phase)
        character(len=:), allocatable, intent(in) :: error_msg
        character(len=*), intent(in) :: phase
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: ', phase, ' error: ', trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine fail_on_error

end program test_issue_2870_common_array_spec
