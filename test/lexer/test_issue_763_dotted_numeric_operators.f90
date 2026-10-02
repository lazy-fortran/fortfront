program test_issue_763_dotted_numeric_operators
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core, TK_NUMBER, TK_OPERATOR
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, INPUT_MODE_STANDARD
    use ast_nodes_core, only: binary_op_node
    implicit none

    call check_relation('1.lt.2', '1', '.lt.', '2')
    call check_relation('2.LE.1', '2', '.LE.', '1')
    call check_relation('2.gt.1', '2', '.gt.', '1')
    call check_relation('3.ge.3', '3', '.ge.', '3')
    call check_relation('2.eq.2', '2', '.eq.', '2')
    call check_relation('2.ne.3', '2', '.ne.', '3')
    call check_relation('1_8.lt.2_8', '1_8', '.lt.', '2_8')
    call check_relation('1.e2.lt.100.', '1.e2', '.lt.', '100.')
    call check_relation('1.D+2.eq.100._8', '1.D+2', '.eq.', '100._8')
    call check_relation('1._8.lt.2._8', '1._8', '.lt.', '2._8')
    call check_relation('.5.gt..2', '.5', '.gt.', '.2')
    call check_relation('1.combine.2', '1', '.combine.', '2')
    call check_number('1.')
    call check_number('.5')
    call check_number('1.e2')
    call check_number('1.D+2')
    call check_number('1._8')
    call check_number('1.e2_8')
    call check_operator_case()
    print *, 'PASS: dotted operators and numeric literal boundaries'

contains

    subroutine check_relation(source, left, operator, right)
        character(len=*), intent(in) :: source, left, operator, right
        type(token_t), allocatable :: tokens(:)

        call tokenize_core(source, tokens)
        call require(size(tokens) == 4, source//' token count')
        call require(tokens(1)%kind == TK_NUMBER, source//' left number')
        call require(tokens(1)%text == left, source//' left literal text')
        call require(tokens(2)%kind == TK_OPERATOR, source//' operator token')
        call require(tokens(2)%text == operator, source//' operator text')
        call require(tokens(3)%kind == TK_NUMBER, source//' right number')
        call require(tokens(3)%text == right, source//' right literal text')
    end subroutine check_relation

    subroutine check_number(source)
        character(len=*), intent(in) :: source
        type(token_t), allocatable :: tokens(:)

        call tokenize_core(source, tokens)
        call require(size(tokens) == 2, source//' standalone token count')
        call require(tokens(1)%kind == TK_NUMBER, source//' number token')
        call require(tokens(1)%text == source, source//' full literal text')
    end subroutine check_number

    subroutine check_operator_case()
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        integer :: i, operator_count

        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string('program p'//new_line('a')// &
            'print *, 1.LT.2, 2.LE.2, 2.GT.1, 3.GE.3, 2.EQ.2, 2.NE.3'// &
            new_line('a')//'end program', result, options)
        call require(result%success(), 'uppercase dotted comparisons accepted')
        operator_count = 0
        do i = 1, result%arena%size
            if (.not. result%arena%has_node_at(i)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (binary_op_node)
                operator_count = operator_count + 1
                call require(index(node%operator, 'L') == 0, 'LT/LE case normalized')
                call require(index(node%operator, 'G') == 0, 'GT/GE case normalized')
                call require(index(node%operator, 'E') == 0, 'EQ/NE case normalized')
            end select
        end do
        call require(operator_count == 6, 'all dotted comparisons retained')
    end subroutine check_operator_case

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a)') 'FAIL: '//message
        stop 1
    end subroutine require

end program test_issue_763_dotted_numeric_operators
