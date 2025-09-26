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
        integer :: header_comment_pos
        integer :: body_pos
        integer :: next_real_pos
        integer :: search_start
        integer :: use1_pos
        integer :: use2_pos
        integer :: use_comment_pos
        integer :: implicit_pos
        character(len=:), allocatable :: existing_decl_line
        character(len=32) :: real_decl_variants(2)

        input = 'use iso_fortran_env' // new_line('a') // &
                '! comment between use statements' // new_line('a') // &
                'use iso_c_binding' // new_line('a') // &
                'real(kind=8) :: x, y' // new_line('a') // new_line('a') // &
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

        call assert_contains(output, 'use iso_fortran_env', &
            'missing iso_fortran_env use statement')
        call assert_contains(output, 'use iso_c_binding', &
            'missing iso_c_binding use statement')
        call assert_contains(output, 'integer :: n', 'missing integer decl for n')
        call assert_contains(output, 'integer :: count', &
            'missing integer decl for count')
        call assert_contains(output, 'integer :: i', 'missing integer decl for i')

        use1_pos = index(output, 'use iso_fortran_env')
        use2_pos = index(output, 'use iso_c_binding')
        use_comment_pos = index(output, 'comment between use statements')
        if (use1_pos == 0 .or. use2_pos == 0) then
            print *, 'FAIL: use statements missing after transformation'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        if (use1_pos >= use2_pos) then
            print *, 'FAIL: use statements reordered'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        if (use_comment_pos == 0) then
            print *, 'FAIL: use block comment missing'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        implicit_pos = index(output, 'implicit none')
        if (implicit_pos == 0) then
            print *, 'FAIL: implicit none missing from output'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        if (.not. (use_comment_pos > use1_pos .and. use_comment_pos < implicit_pos)) then
            print *, 'FAIL: use block comment moved out of declaration header'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if

        real_decl_variants = [ character(len=32) :: &
            'real(kind=8) :: x, y', &
            'real(8) :: x, y' ]
        call assert_contains_any(output, real_decl_variants, &
            'missing explicit real declaration for x,y')

        real_decl_pos = index(output, 'real(kind=8) :: x, y')
        existing_decl_line = 'real(kind=8) :: x, y'
        if (real_decl_pos == 0) then
            real_decl_pos = index(output, 'real(8) :: x, y')
            existing_decl_line = 'real(8) :: x, y'
        end if

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

        if (implicit_pos <= use2_pos) then
            print *, 'FAIL: implicit none inserted before use block'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if

        header_comment_pos = index(output, 'header comment before loop')
        if (header_comment_pos == 0) then
            print *, 'FAIL: header comment missing after transformation'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        if (header_comment_pos <= last_new_pos) then
            print *, 'FAIL: header comment moved ahead of inferred declarations'
            print *, 'Output:'
            print *, trim(output)
            error stop 1
        end if
        body_pos = index(output, 'n = 1000000')
        if (body_pos > 0 .and. header_comment_pos >= body_pos) then
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

    subroutine assert_contains_any(text, patterns, message)
        character(len=*), intent(in) :: text
        character(len=*), dimension(:), intent(in) :: patterns
        character(len=*), intent(in) :: message
        integer :: i

        do i = 1, size(patterns)
            if (len_trim(patterns(i)) == 0) cycle
            if (index(text, trim(patterns(i))) > 0) return
        end do

        print *, 'FAIL:', trim(message)
        print *, 'Patterns:'
        do i = 1, size(patterns)
            print *, trim(patterns(i))
        end do
        print *, 'Output:'
        print *, trim(text)
        error stop 1
    end subroutine assert_contains_any

end program test_issue_1330_missing_declarations
