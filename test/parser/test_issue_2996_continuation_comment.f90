program test_issue_2996_continuation_comment
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    use lexer_core, only: token_t, TK_COMMENT
    use ast_nodes_conditional, only: select_case_node, case_block_node
    use ast_nodes_core, only: literal_node
    implicit none

    character(len=:), allocatable :: source
    integer :: failures

    failures = 0
    call read_example('examples/f90/issue_2996_select_case_comment_continuation.f90', &
        source)
    call check_select_case(source, 'full-line comment between continuations', failures)

    call read_example('examples/f90/issue_2996_select_case_plain_continuation.f90', &
        source)
    call check_select_case(source, 'neighbor without comment', failures)

    call read_example('examples/f90/issue_2996_select_case_missing_continuation.f90', &
        source)
    call check_rejected(source, 'missing continuation marker', failures)

    call read_example('examples/f90/issue_2996_assignment_comment_continuation.f90', &
        source)
    call check_continued_assignment(source, 'comment in expression continuation', &
        failures)

    if (failures > 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, &
            ' issue #2996 parser/semantic checks'
        error stop 1
    end if
    print '(A)', 'PASS: issue #2996 continuation comments preserve SELECT CASE'

contains

    subroutine check_select_case(source, label, failures)
        character(len=*), intent(in) :: source, label
        integer, intent(inout) :: failures
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        integer :: i, j, select_count, value_count
        logical :: found_one, found_two

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%parse_ok) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' did not parse'
            write (error_unit, '(A)') trim(result%diagnostic_text)
            failures = failures + 1
            return
        end if
        if (.not. result%semantic_ok) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' did not pass semantic analysis'
            write (error_unit, '(A)') trim(result%diagnostic_text)
            failures = failures + 1
            return
        end if

        select_count = 0
        value_count = 0
        found_one = .false.
        found_two = .false.
        do i = 1, result%arena%size
            if (.not. allocated(result%arena%entries(i)%node)) cycle
            select type (node => result%arena%entries(i)%node)
                type is (select_case_node)
                select_count = select_count + 1
                if (.not. allocated(node%case_indices)) cycle
                do j = 1, size(node%case_indices)
                    if (node%case_indices(j) <= 0) cycle
                    if (.not. allocated(result%arena%entries( &
                        node%case_indices(j))%node)) cycle
                    select type (block => result%arena%entries( &
                            node%case_indices(j))%node)
                        type is (case_block_node)
                        if (.not. allocated(block%value_indices)) cycle
                        value_count = value_count + size(block%value_indices)
                        call inspect_values(result, block%value_indices, &
                            found_one, found_two)
                    class default
                        cycle
                    end select
                end do
            class default
                cycle
            end select
        end do

        if (select_count /= 1 .or. value_count /= 2 .or. &
            .not. found_one .or. .not. found_two) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' produced an incomplete CASE value AST'
            failures = failures + 1
        end if
    end subroutine check_select_case

    subroutine check_rejected(source, label, failures)
        character(len=*), intent(in) :: source, label
        integer, intent(inout) :: failures
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)

        if (result%parse_ok .or. result%semantic_ok) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' was accepted without a continuation marker'
            failures = failures + 1
        end if
    end subroutine check_rejected

    subroutine check_continued_assignment(source, label, failures)
        character(len=*), intent(in) :: source, label
        integer, intent(inout) :: failures
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: lex_error
        integer :: i, comment_count

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.

        call lex_source(source, tokens, lex_error)
        comment_count = 0
        if (allocated(tokens)) then
            do i = 1, size(tokens)
                if (tokens(i)%kind /= TK_COMMENT) cycle
                if (.not. allocated(tokens(i)%text)) cycle
                if (index(tokens(i)%text, &
                    'between continued expression lines') <= 0) cycle
                comment_count = comment_count + 1
                if (tokens(i)%line /= 6 .or. tokens(i)%column /= 9) then
                    write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                        ' changed comment location'
                    failures = failures + 1
                end if
            end do
        end if
        if (allocated(lex_error) .and. len_trim(lex_error) > 0) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' could not lex its source'
            failures = failures + 1
        else if (comment_count /= 1) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' dropped its comment trivia'
            failures = failures + 1
        end if

        call compile_frontend_from_string(source, result, options)

        if (.not. result%parse_ok .or. .not. result%semantic_ok) then
            write (error_unit, '(A,A,A)') 'FAIL: ', trim(label), &
                ' was not accepted'
            write (error_unit, '(A)') trim(result%diagnostic_text)
            failures = failures + 1
            return
        end if

    end subroutine check_continued_assignment

    subroutine inspect_values(result, indices, found_one, found_two)
        type(compiler_frontend_result_t), intent(in) :: result
        integer, intent(in) :: indices(:)
        logical, intent(inout) :: found_one, found_two
        integer :: i, index

        do i = 1, size(indices)
            index = indices(i)
            if (index <= 0 .or. index > result%arena%size) cycle
            if (.not. allocated(result%arena%entries(index)%node)) cycle
            select type (value => result%arena%entries(index)%node)
                type is (literal_node)
                if (.not. allocated(value%value)) cycle
                if (trim(value%value) == '1') found_one = .true.
                if (trim(value%value) == '2') found_two = .true.
            class default
                cycle
            end select
        end do
    end subroutine inspect_values

    include '../common/read_example.inc'

end program test_issue_2996_continuation_comment
