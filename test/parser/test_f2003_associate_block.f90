program test_f2003_associate_block
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    type(transform_context_t) :: ctx
    logical :: all_passed

    all_passed = .true.

    call test_associate_construct(all_passed)
    call test_block_construct(all_passed)

    if (all_passed) then
        print *, 'PASS: F2003 associate and block constructs parsed correctly'
    else
        error stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


    subroutine test_associate_construct(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lower_output
        type(transform_context_t) :: ctx

        call read_example('examples/f90/f2003_associate_construct.f90', source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'f2003_associate_construct'

        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: associate transform error: ' &
                & // trim(error_msg)
            all_passed = .false.
            return
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: associate no output produced'
            all_passed = .false.
            return
        end if

        lower_output = to_lower(output_code)

        call assert_contains(lower_output, 'associate', &
            & 'FAIL: associate keyword not preserved', all_passed)

        call assert_contains(lower_output, 'end associate', &
            & 'FAIL: end associate not preserved', all_passed)

        call assert_contains(lower_output, 'px => pt%x', &
            & 'FAIL: associate with component access not preserved', all_passed)

        call assert_contains(lower_output, 'py => pt%y', &
            & 'FAIL: second associate binding not preserved', all_passed)

        call assert_contains(lower_output, 'scaled_val => val', &
            & 'FAIL: associate with expression not preserved', all_passed)

    end subroutine test_associate_construct

    subroutine test_block_construct(all_passed)
        logical, intent(inout) :: all_passed
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lower_output
        type(transform_context_t) :: ctx
        integer :: block_count
        integer :: pos
        integer :: search_start

        call read_example('examples/f90/f2003_block_construct.f90', source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%has_filename = .true.
        ctx%source_name = 'f2003_block_construct'

        call transform_with_context(source_code, output_code, error_msg, ctx)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: block transform error: ' &
                & // trim(error_msg)
            all_passed = .false.
            return
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: block no output produced'
            all_passed = .false.
            return
        end if

        lower_output = to_lower(output_code)

        block_count = 0
        search_start = 1
        do
            pos = index(lower_output(search_start:), 'end block')
            if (pos == 0) exit
            block_count = block_count + 1
            search_start = search_start + pos + 8
        end do

        if (block_count < 2) then
            write (error_unit, '(A,I0,A)') &
                'FAIL: expected 2 block constructs, found ', &
                & block_count, ''
            all_passed = .false.
            return
        end if

        call assert_contains(lower_output, 'real :: partial_sum', &
            & 'FAIL: block-local variable declaration not preserved', all_passed)

    end subroutine test_block_construct

    subroutine assert_contains(text, pattern, failure_message, passed)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message
        logical, intent(inout) :: passed

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            passed = .false.
        end if
    end subroutine assert_contains

end program test_f2003_associate_block
