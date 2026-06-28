program test_cmplx_intrinsic_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        iostat_end, iostat_eor, output_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    integer :: status

    status = 0

    call run_case('examples/lf/issue_2073_basic_cmplx.lf', &
        'basic cmplx assignment', status)
    call run_case('examples/lf/issue_2073_cmplx_aimag.lf', &
        'cmplx with aimag usage', status)
    call run_case('examples/lf/issue_2073_cmplx_real.lf', &
        'cmplx with real usage', status)
    call run_case('examples/lf/issue_2073_cmplx_abs.lf', &
        'cmplx with abs usage', status)

    if (status /= 0) then
        error stop 'cmplx intrinsic inference tests FAILED'
    end if

    write (output_unit, '(A)') 'PASS: cmplx intrinsic inference tests'

contains

    include '../common/read_example.inc'


    subroutine run_case(example_path, description, status)
        character(len=*), intent(in) :: example_path
        character(len=*), intent(in) :: description
        integer, intent(inout) :: status
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        logical :: success

        call read_example(example_path, source)
        call transform_lazy_fortran_string(source, output_code, error_msg)

        success = .false.
        if (len_trim(error_msg) == 0) then
            success = has_complex_declaration(output_code)
        end if

        if (.not. success) then
            status = 1
            call report_failure(description, output_code, error_msg)
        else
            write (output_unit, '(A)') 'PASS: ' // trim(description)
        end if
    end subroutine run_case

    subroutine report_failure(description, output_code, error_msg)
        character(len=*), intent(in) :: description
        character(len=:), allocatable, intent(in) :: output_code
        character(len=:), allocatable, intent(in) :: error_msg

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(description)
            write (error_unit, '(A)') 'Transform error: ' // trim(error_msg)
        else
            write (error_unit, '(A)') 'FAIL: ' // trim(description)
            write (error_unit, '(A)') 'Complex declaration missing for z'
            write (error_unit, '(A)') trim(output_code)
        end if
    end subroutine report_failure


    logical function has_complex_declaration(output_code)
        character(len=:), allocatable, intent(in) :: output_code
        integer :: search_start, decl_pos, z_pos

        has_complex_declaration = .false.
        if (.not. allocated(output_code)) return
        search_start = 1

        do
            decl_pos = index(output_code(search_start:), 'complex ::')
            if (decl_pos == 0) exit
            decl_pos = decl_pos + search_start - 1
            z_pos = index(output_code(decl_pos:), ' z')
            if (z_pos /= 0) then
                has_complex_declaration = .true.
                return
            end if
            search_start = decl_pos + 1
        end do
    end function has_complex_declaration

end program test_cmplx_intrinsic_inference
