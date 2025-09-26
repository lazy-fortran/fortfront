program test_issue_1330_missing_declarations
    use frontend, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call test_missing_variable_declarations()
    print *, 'Issue #1330 regression test passed.'

contains

    subroutine test_missing_variable_declarations()
        logical :: has_real_kind
        integer :: real_decl_pos
        integer :: pos_n
        integer :: pos_count
        integer :: pos_i
        integer :: first_new_pos
        integer :: last_new_pos
        integer :: output_len
        integer :: comment_pos
        integer :: body_pos
        integer :: next_real_pos
        integer :: search_start
        character(len=:), allocatable :: existing_decl_line

        input = 'real(kind=8) :: x, y' // new_line('a') // new_line('a') // &
                '! header comment before loop' // new_line('a') // &
                'n = 1000000' // new_line('a') // &
                'count = 0' // new_line('a') // &
                'do i = 1, n' // new_line('a') // &
                '    call random_number(x)' // new_line('a') // &
                '    call random_number(y)' // new_line('a') // &
                '    if (x*x + y*y <= 1.0) count = count + 1' // new_line('a') // &
                'end do' // new_line('a') // &
                'pi_estimate = 4.0 * real(count) / real(n)' // new_line('a') // &
                'print *, "Number of points:", n' // new_line('a') // &
                'print *, "Estimated value of pi:", pi_estimate'

        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'FAIL: transform error:', trim(error_msg)
                error stop 1
            end if
        end if

        call assert_contains(output, 'integer :: n', 'missing integer decl for n')
        call assert_contains(output, 'integer :: count', &
            'missing integer decl for count')
        call assert_contains(output, 'integer :: i', 'missing integer decl for i')

        real_decl_pos = index(output, 'real(kind=8) :: x, y')
        if (real_decl_pos > 0) then
            existing_decl_line = 'real(kind=8) :: x, y'
        else
            real_decl_pos = index(output, 'real(8) :: x, y')
            if (real_decl_pos > 0) then
                existing_decl_line = 'real(8) :: x, y'
            else
                print *, 'FAIL: missing explicit real(kind=8) declaration for x,y'
                print *, 'Output:'
                print *, trim(output)
                error stop 1
            end if
        end if
        call assert_contains(output, existing_decl_line, &
            'missing explicit real declaration for x,y')

        has_real_kind = index(output, 'real :: pi_estimate') > 0
        if (.not. has_real_kind) then
            has_real_kind = index(output, 'real(8) :: pi_estimate') > 0
        end if
        if (.not. has_real_kind) then
            print *, 'FAIL: missing real declaration for pi_estimate'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if

        output_len = len(output)
        first_new_pos = output_len + 1
        last_new_pos = 0
        pos_n = index(output, 'integer :: n')
        if (pos_n > 0) then
            if (pos_n < first_new_pos) first_new_pos = pos_n
            if (pos_n > last_new_pos) last_new_pos = pos_n
        end if
        pos_count = index(output, 'integer :: count')
        if (pos_count > 0) then
            if (pos_count < first_new_pos) first_new_pos = pos_count
            if (pos_count > last_new_pos) last_new_pos = pos_count
        end if
        pos_i = index(output, 'integer :: i')
        if (pos_i > 0) then
            if (pos_i < first_new_pos) first_new_pos = pos_i
            if (pos_i > last_new_pos) last_new_pos = pos_i
        end if

        if (real_decl_pos > 0 .and. first_new_pos <= real_decl_pos) then
            print *, 'FAIL: inferred declarations inserted before existing declarations'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if

        comment_pos = index(output, 'header comment before loop')
        if (comment_pos == 0) then
            print *, 'FAIL: header comment missing after transformation'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        if (comment_pos <= last_new_pos) then
            print *, 'FAIL: header comment moved ahead of inferred declarations'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        body_pos = index(output, 'n = 1000000')
        if (body_pos > 0 .and. comment_pos >= body_pos) then
            print *, 'FAIL: header comment moved into executable section'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if

        if (real_decl_pos > 0) then
            if (real_decl_pos + len(existing_decl_line) <= output_len) then
                search_start = real_decl_pos + len(existing_decl_line)
                next_real_pos = index(output(search_start:), existing_decl_line)
            else
                next_real_pos = 0
            end if
            if (next_real_pos > 0) then
                print *, 'FAIL: explicit declarations duplicated'
                print *, 'Output:'
                print *, trim(output)
                error stop 1
            end if
        end if
    end subroutine test_missing_variable_declarations

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) == 0) then
            print *, 'FAIL:', trim(message)
            print *, 'Pattern:', trim(pattern)
            print *, 'Output:'
            print *, trim(text)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_1330_missing_declarations
