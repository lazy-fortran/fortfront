program test_issue_2274_identifier_cycle
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_declaration, has_assignment, has_print, has_real_decl, &
        & has_cycle_stmt, has_failure_stub

    call read_example('examples/f90/issue_2274_identifier_cycle.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2274_identifier_cycle'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_declaration = index(output_code, 'integer :: cycle') > 0
    has_assignment = index(output_code, 'cycle = 1') > 0
    has_print = index(output_code, 'print *, cycle') > 0
    has_real_decl = index(output_code, 'real :: cycle') > 0
    has_cycle_stmt = index(output_code, new_line('a')//'    cycle'// &
        & new_line('a')) > 0 .or. index(output_code, new_line('a')//'cycle'// &
        & new_line('a')) > 0
    has_failure_stub = index(output_code, 'COMPILATION FAILED') > 0

    if (.not. has_declaration) then
        write (error_unit, '(A)') 'FAIL: missing integer :: cycle declaration'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_assignment) then
        write (error_unit, '(A)') 'FAIL: missing cycle = 1 assignment'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_print) then
        write (error_unit, '(A)') 'FAIL: missing print *, cycle statement'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (has_real_decl) then
        write (error_unit, '(A)') 'FAIL: unexpected real :: cycle declaration'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (has_cycle_stmt) then
        write (error_unit, '(A)') 'FAIL: stray CYCLE statement detected'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (has_failure_stub) then
        write (error_unit, '(A)') 'FAIL: fallback stub detected in cycle test'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: identifier named cycle survives round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2274_identifier_cycle
