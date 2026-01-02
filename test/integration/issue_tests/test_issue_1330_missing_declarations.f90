program test_issue_1330_missing_declarations
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call test_missing_variable_declarations()
    print *, 'Issue #1330 regression test passed.'

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    subroutine test_missing_variable_declarations()
        character(len=:), allocatable :: input
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

        call read_example('examples/lf/issue_1330_missing_declarations.lf', &
                          input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'FAIL: transform error: ' // &
                    trim(error_msg)
                error stop 1
            end if
        end if

        call assert_contains(output, 'use iso_fortran_env', &
                             'missing iso_fortran_env use statement')
        call assert_contains(output, 'use iso_c_binding', &
                             'missing iso_c_binding use statement')
        if (.not. has_integer_declaration(output, [character(len=16) :: &
                                                   'n', 'count', 'i'])) then
            write (error_unit, '(A)') &
                'FAIL: missing integer declarations for n/count/i'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        use1_pos = index(output, 'use iso_fortran_env')
        use2_pos = index(output, 'use iso_c_binding')
        use_comment_pos = index(output, 'comment between use statements')
        if (use1_pos == 0 .or. use2_pos == 0) then
            write (error_unit, '(A)') &
                'FAIL: use statements missing after transformation'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        if (use1_pos >= use2_pos) then
            write (error_unit, '(A)') 'FAIL: use statements reordered'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        if (use_comment_pos == 0) then
            write (error_unit, '(A)') 'FAIL: use block comment missing'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        implicit_pos = index(output, 'implicit none')
        if (implicit_pos == 0) then
            write (error_unit, '(A)') 'FAIL: implicit none missing from output'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        if (.not. (use_comment_pos > use1_pos .and. use_comment_pos < &
                   implicit_pos)) then
            write (error_unit, '(A)') &
                'FAIL: use block comment moved out of declaration header'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        real_decl_variants = [character(len=32) :: &
                              'real(kind=8) :: x, y', &
                              'real(8) :: x, y']
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
            write (error_unit, '(A)') 'FAIL: missing real declaration for pi_estimate'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        pos_n = index(output, 'integer :: n')
        pos_count = index(output, 'integer :: count')
        pos_i = index(output, 'integer :: i')
        if (.not. (pos_n == 0 .or. pos_count == 0 .or. pos_i == 0)) then
            if (.not. (pos_n < pos_count .and. pos_count < pos_i)) then
                write (error_unit, '(A)') 'FAIL: integer declarations out of order'
                write (error_unit, '(A)') trim(output)
                error stop 1
            end if
        end if

        if (.not. has_integer_declaration(output, [character(len=16) :: 'n'])) then
            write (error_unit, '(A)') 'FAIL: inferred declaration for n missing'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        first_new_pos = index(output, 'integer :: n')
        if (first_new_pos == 0) first_new_pos = index(output, 'integer ::')
        last_new_pos = index(output, 'real :: pi_estimate')
        if (last_new_pos == 0) then
            last_new_pos = index(output, 'real(8) :: pi_estimate')
        end if
        if (first_new_pos == 0 .or. last_new_pos == 0) then
            write (error_unit, '(A)') 'FAIL: inferred declarations missing'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        output_len = len_trim(output)
        if (last_new_pos + 1 > output_len) then
            write (error_unit, '(A)') 'FAIL: malformed declaration block'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        if (implicit_pos <= use2_pos) then
            write (error_unit, '(A)') &
                'FAIL: implicit none inserted before use block'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        header_comment_pos = index(output, 'header comment before loop')
        if (header_comment_pos == 0) then
            write (error_unit, '(A)') 'FAIL: header comment missing'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        if (header_comment_pos <= last_new_pos) then
            write (error_unit, '(A)') &
                'FAIL: header comment moved ahead of inferred declarations'
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if
        body_pos = index(output, 'n = 1000000')
        if (body_pos > 0 .and. header_comment_pos >= body_pos) then
            write (error_unit, '(A)') &
                'FAIL: header comment moved into executable section'
            write (error_unit, '(A)') trim(output)
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
                write (error_unit, '(A)') 'FAIL: explicit declarations duplicated'
                write (error_unit, '(A)') trim(output)
                error stop 1
            end if
        end if
    end subroutine test_missing_variable_declarations

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(message)
            write (error_unit, '(A)') 'Pattern: ' // trim(pattern)
            write (error_unit, '(A)') 'Output:'
            write (error_unit, '(A)') trim(text)
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

        write (error_unit, '(A)') 'FAIL: ' // trim(message)
        write (error_unit, '(A)') 'Patterns:'
        do i = 1, size(patterns)
            write (error_unit, '(A)') trim(patterns(i))
        end do
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') trim(text)
        error stop 1
    end subroutine assert_contains_any

    logical function has_integer_declaration(text, names)
        character(len=*), intent(in) :: text
        character(len=*), dimension(:), intent(in) :: names
        integer :: pos
        integer :: start_pos
        integer :: end_pos
        integer :: i
        integer :: text_len
        character(len=:), allocatable :: line
        character(1), parameter :: nl = new_line('a')

        has_integer_declaration = .false.
        text_len = len(text)
        pos = index(text, 'integer ::')

        do while (pos > 0)
            start_pos = pos
            do while (start_pos > 1 .and. text(start_pos - 1:start_pos - 1) /= nl)
                start_pos = start_pos - 1
            end do

            end_pos = pos
            do while (end_pos <= text_len .and. text(end_pos:end_pos) /= nl)
                end_pos = end_pos + 1
            end do

            if (end_pos > text_len) then
                line = text(start_pos:)
            else
                line = text(start_pos:end_pos - 1)
            end if

            line = adjustl(line)
            if (index(line, 'integer ::') == 1) then
                has_integer_declaration = .true.
                do i = 1, size(names)
                    if (index(line, trim(names(i))) == 0) then
                        has_integer_declaration = .false.
                        exit
                    end if
                end do
                if (has_integer_declaration) return
            end if

            if (end_pos > text_len) exit
            pos = index(text(end_pos:), 'integer ::')
            if (pos > 0) pos = pos + end_pos - 1
        end do
    end function has_integer_declaration

end program test_issue_1330_missing_declarations
