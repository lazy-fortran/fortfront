program test_issue_2246_identifier_implicit
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_assignment, has_print, has_declaration

    call read_example('examples/f90/issue_2246_identifier_implicit.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2246_identifier_implicit'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_declaration = index(output_code, 'integer :: implicit') > 0
    has_assignment = index(output_code, 'implicit = 5') > 0
    has_print = index(output_code, 'print *, implicit') > 0

    if (.not. has_declaration) then
        write (error_unit, '(A)') 'FAIL: missing integer :: implicit declaration'
        error stop 1
    end if

    if (.not. has_assignment) then
        write (error_unit, '(A)') 'FAIL: missing implicit = 5 assignment'
        error stop 1
    end if

    if (.not. has_print) then
        write (error_unit, '(A)') 'FAIL: missing print *, implicit statement'
        error stop 1
    end if

    print *, 'PASS: identifier named implicit survives round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2246_identifier_implicit
