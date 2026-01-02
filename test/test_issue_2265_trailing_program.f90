program test_issue_2265_trailing_program
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_context_t, transform_with_context
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    integer :: module_pos, program_pos

    call read_example('examples/f90/issue_2265_module_program.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2265_module_program'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    module_pos = index(output_code, 'module issue_2265_module_program_mod')
    program_pos = index(output_code, 'program issue_2265_roundtrip_app')

    if (module_pos == 0) then
        write (error_unit, '(A)') 'FAIL: module unit missing in output'
        error stop 1
    end if

    if (program_pos == 0) then
        write (error_unit, '(A)') 'FAIL: trailing program missing in output'
        error stop 1
    end if

    if (program_pos <= module_pos) then
        write (error_unit, '(A)') 'FAIL: program emitted before module unexpectedly'
        error stop 1
    end if

    print *, 'PASS: module and trailing program both round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2265_trailing_program
