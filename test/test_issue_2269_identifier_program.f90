program test_issue_2269_identifier_program
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_integer_decl, has_assignment, has_print_stmt
    logical :: has_failure_stub

    call read_example('examples/f90/issue_2269_identifier_program.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2269_identifier_program'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_integer_decl = index(output_code, 'integer :: program') > 0
    has_assignment = index(output_code, 'program = 10') > 0
    has_print_stmt = index(output_code, 'print *, program') > 0
    has_failure_stub = index(output_code, 'COMPILATION FAILED') > 0

    if (.not. has_integer_decl) then
        write (error_unit, '(A)') 'FAIL: missing integer :: program declaration'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_assignment) then
        write (error_unit, '(A)') 'FAIL: missing program = 10 assignment'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_print_stmt) then
        write (error_unit, '(A)') 'FAIL: missing print *, program statement'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (has_failure_stub) then
        write (error_unit, '(A)') 'FAIL: fallback program stub detected'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: identifier named program survives validation and round-trip'


contains


    include 'common/read_example.inc'
end program test_issue_2269_identifier_program
