program test_issue_2247_contains_identifier
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_real_decl
    logical :: has_array_assignment
    logical :: has_element_assignment
    logical :: has_index_assignment
    logical :: has_print_stmt

    call read_example('examples/f90/issue_2247_contains_identifier.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2247_contains_identifier'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            & trim(error_msg)
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    has_real_decl = index(output_code, 'real :: contains(2)') > 0
    has_array_assignment = index(output_code, 'contains = 2.0') > 0
    has_element_assignment = index(output_code, &
        'contains(2) = contains(1) + 3.0') > 0
    has_index_assignment = index(output_code, &
        'contains(int(contains(1))) = contains(2) - 1.0') > 0
    has_print_stmt = index(output_code, 'print *, contains(1), contains(2)') > 0

    if (.not. has_real_decl) then
        write (error_unit, '(A)') 'FAIL: missing real :: contains(2) declaration'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_array_assignment) then
        write (error_unit, '(A)') 'FAIL: missing contains = 2.0 assignment'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_element_assignment) then
        write (error_unit, '(A)') &
            'FAIL: missing contains(2) = contains(1) + 3.0 assignment'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_index_assignment) then
        write (error_unit, '(A)') &
            'FAIL: missing contains(int(contains(1))) assignment'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_print_stmt) then
        write (error_unit, '(A)') &
            'FAIL: missing print *, contains(1), contains(2) statement'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: identifier named contains survives round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2247_contains_identifier
