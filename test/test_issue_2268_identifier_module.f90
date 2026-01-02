program test_issue_2268_identifier_module
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_declaration, has_assignment, has_print, failure_stub_present

    call read_example('examples/f90/issue_2268_identifier_module.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2268_identifier_module'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    failure_stub_present = index(output_code, '! COMPILATION FAILED') > 0

    if (failure_stub_present) then
        write (error_unit, '(A)') 'FAIL: output contains compilation failed stub'
        error stop 1
    end if

    has_declaration = index(output_code, 'integer :: module') > 0
    has_assignment = index(output_code, 'module = 3') > 0
    has_print = index(output_code, 'print *, module') > 0

    if (.not. has_declaration) then
        write (error_unit, '(A)') 'FAIL: missing integer :: module declaration'
        error stop 1
    end if

    if (.not. has_assignment) then
        write (error_unit, '(A)') 'FAIL: missing module = 3 assignment'
        error stop 1
    end if

    if (.not. has_print) then
        write (error_unit, '(A)') 'FAIL: missing print *, module statement'
        error stop 1
    end if

    print *, 'PASS: identifier named module survives round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2268_identifier_module
