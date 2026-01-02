program test_issue_2287_where_elsewhere
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: program_index
    integer :: pos_where, pos_elsewhere, pos_endwhere
    integer :: pos_assignment1, pos_assignment2

    call read_example('examples/f90/issue_2287_where_elsewhere.f90', source_code)

    arena = create_ast_arena()

    call lex_source(source_code, tokens, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: lex_source error: ' // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, program_index, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: parse_tokens error: ' // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, program_index, output_code)
    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: emit_fortran produced no output'
        error stop 1
    end if

    ! Check for WHERE construct
    pos_where = index(output_code, 'where (b == 0)')
    if (pos_where == 0) then
        write (error_unit, '(A)') 'FAIL: WHERE construct missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    ! Check for ELSEWHERE block - search after the WHERE construct
    ! Note: searching from pos_where to avoid matching program name
    pos_elsewhere = index(output_code(pos_where:), char(10)//'    elsewhere')
    if (pos_elsewhere > 0) then
        pos_elsewhere = pos_elsewhere + pos_where - 1  ! Adjust to global position
    else
        write (error_unit, '(A)') 'FAIL: ELSEWHERE block missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    ! Check for END WHERE
    pos_endwhere = index(output_code, 'end where')
    if (pos_endwhere == 0) then
        write (error_unit, '(A)') 'FAIL: END WHERE missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    ! Check WHERE body assignment (c = a * 10)
    pos_assignment1 = index(output_code, 'c = a * 10')
    if (pos_assignment1 == 0) then
        pos_assignment1 = index(output_code, 'c = a*10')
    end if
    if (pos_assignment1 == 0) then
        write (error_unit, '(A)') 'FAIL: WHERE body assignment missing'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    ! Check ELSEWHERE assignment (c = a)
    pos_assignment2 = index(output_code(pos_elsewhere:), 'c = a')
    if (pos_assignment2 == 0) then
        write (error_unit, '(A)') 'FAIL: ELSEWHERE assignment missing'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    ! Verify structure order: where < elsewhere < endwhere
    if (.not. (pos_where < pos_elsewhere .and. pos_elsewhere < pos_endwhere)) then
        write (error_unit, '(A)') 'FAIL: WHERE/ELSEWHERE/END WHERE out of order'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2287 WHERE/ELSEWHERE preserved'


contains


    include 'common/read_example.inc'
end program test_issue_2287_where_elsewhere
