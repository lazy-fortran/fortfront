program test_basic_function_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    logical :: basic_pass
    logical :: reorder_pass

    basic_pass = run_basic_function_test()
    reorder_pass = run_reordered_function_test()

    if (basic_pass .and. reorder_pass) then
        write (*, '(A)') &
            & 'PASS: lazy function inference emits integer signatures'
        stop 0
    end if

    if (.not. basic_pass) then
        write (error_unit, '(A)') &
            & 'FAIL: basic inference skipped integer emission'
    end if
    if (.not. reorder_pass) then
        write (error_unit, '(A)') &
            & 'FAIL: reordered inference fell back to real types'
    end if
    stop 1

contains

    include '../common/read_example.inc'

    function run_basic_function_test() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=40), parameter :: required_fragments(3) = [ &
                                        character(len=32) :: &
                                        'integer function square', &
                                        'integer, intent(in) :: x', &
                                        'integer :: val, squared']
        character(len=32), parameter :: forbidden_fragments(4) = [ &
                                        character(len=32) :: &
                                        'real function square', &
                                        'real :: square', &
                                        'real(8) function square', &
                                        'real, external :: square']

        call read_example('examples/lf/function_inference_basic.lf', source)

        passed = run_function_inference_case(source, required_fragments, &
                                             forbidden_fragments, 'basic')
    end function run_basic_function_test

    function run_reordered_function_test() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=48), parameter :: required_fragments(4) = [ &
                                        character(len=48) :: &
                                        'integer :: val, squared', &
                                        'integer, external :: square', &
                                        'integer function square', &
                                        'integer, intent(in) :: x']
        character(len=40), parameter :: forbidden_fragments(4) = [ &
                                        character(len=40) :: &
                                        'real :: squared', &
                                        'real, external :: square', &
                                        'real function square', &
                                        'real(8) function square']

        call read_example('examples/lf/function_inference_reordered.lf', source)

        passed = run_function_inference_case(source, required_fragments, &
                                             forbidden_fragments, 'reordered')
    end function run_reordered_function_test

    function run_function_inference_case(source, required_fragments, &
                                         forbidden_fragments, context) &
        result(passed)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: required_fragments(:)
        character(len=*), intent(in) :: forbidden_fragments(:)
        character(len=*), intent(in) :: context
        logical :: passed
        character(len=:), allocatable :: descriptor
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        descriptor = ''
        if (len_trim(context) > 0) then
            descriptor = ' (' // trim(context) // ')'
        end if

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors' // descriptor &
                // ':'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (.not. verify_fragments(generated, required_fragments, .true., &
                                   context)) passed = .false.
        if (.not. verify_fragments(generated, forbidden_fragments, .false., &
                                   context)) passed = .false.
    end function run_function_inference_case

    logical function verify_fragments(generated, fragments, expect_present, &
                                      context) &
        result(all_valid)
        character(len=*), intent(in) :: generated
        character(len=*), intent(in) :: fragments(:)
        logical, intent(in) :: expect_present
        character(len=*), intent(in) :: context
        integer :: i
        character(len=:), allocatable :: fragment
        character(len=:), allocatable :: descriptor

        all_valid = .true.
        descriptor = ''
        if (len_trim(context) > 0) descriptor = ' (' // trim(context) // ')'
        do i = 1, size(fragments)
            fragment = trim(fragments(i))
            if (expect_present) then
                if (index(generated, fragment) == 0) then
                    write (error_unit, '(A)') 'missing fragment' // descriptor &
                        // ': ' // fragment
                    all_valid = .false.
                end if
            else
                if (index(generated, fragment) > 0) then
                    write (error_unit, '(A)') 'unexpected fragment' // &
                        descriptor // ': ' // fragment
                    all_valid = .false.
                end if
            end if
        end do
    end function verify_fragments


end program test_basic_function_inference
