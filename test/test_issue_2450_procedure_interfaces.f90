program test_issue_2450_procedure_interfaces
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_context_t, transform_with_context
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2450_procedure_interfaces.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2450_procedure_interfaces'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            & trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: transform_with_context produced no output'
        error stop 1
    end if

    if (index(output_code, 'real, external :: external_scale') == 0) then
        write (error_unit, '(A)') 'FAIL: external declaration was lost'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, 'procedure(real), pointer :: proc_ptr') == 0) then
        write (error_unit, '(A)') 'FAIL: procedure pointer declaration missing'
        error stop 1
    end if

    ! ISO/IEC 1539-1:2018 Section 15.4.3.2: Procedure dummy arguments
    ! cannot have INTENT attribute - check that intent is NOT added
    if (index(output_code, 'procedure(real) :: fn') == 0) then
        write (error_unit, '(A)') 'FAIL: dummy procedure specification missing'
        error stop 1
    end if

    if (index(output_code, 'procedure(real), intent') /= 0) then
        write (error_unit, '(A)') 'FAIL: procedure dummy argument incorrectly has intent attribute'
        write (error_unit, '(A)') 'ISO/IEC 1539-1:2018 Section 15.4.3.2: procedure dummy arguments cannot have INTENT'
        error stop 1
    end if

    if (index(output_code, 'procedure(real), pointer :: p') == 0) then
        write (error_unit, '(A)') 'FAIL: procedure pointer result missing'
        error stop 1
    end if

    print *, 'PASS: Issue #2450 external and procedure interface parsing'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2450_procedure_interfaces
