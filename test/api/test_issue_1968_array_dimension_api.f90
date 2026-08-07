program test_issue_1968_array_dimension_api
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        emit_fortran, query_program_unit, &
        query_declaration, program_unit_query_t, declaration_query_t
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(program_unit_query_t) :: function_query
    type(declaration_query_t) :: array_query
    integer :: i
    integer :: j
    logical :: found_function

    call read_example('examples/lf/issue_1968_lazy_function_result.lf', source)

    options%run_semantics = .true.
    options%standardize = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        error_msg = result%diagnostic_text
        write (error_unit, '(A)') 'FAIL: frontend rejected issue #1968'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    found_function = .false.
    do i = 1, result%arena%size
        function_query = query_program_unit(result%arena, i)
        if (trim(function_query%unit_kind) /= 'function') cycle
        if (trim(function_query%name) /= 'array_sum') cycle
        found_function = .true.
        array_query%found = .false.
        do j = 1, size(function_query%body_indices)
            array_query = query_declaration(result%arena, &
                function_query%body_indices(j))
            if (array_query%found .and. trim(array_query%name) == 'arr') exit
        end do
        if (.not. array_query%found .or. .not. array_query%is_array .or. &
            array_query%is_allocatable) then
            write (error_unit, '(A,L1,A,L1)') 'found=', array_query%found, &
                ' array=', array_query%is_array
            write (error_unit, '(A)') &
                'FAIL: arr was not inferred as a non-allocatable array'
            error stop 1
        end if
        if (.not. allocated(array_query%dimension_indices)) then
            write (error_unit, '(A)') 'FAIL: arr has no inferred shape'
            error stop 1
        end if
        if (size(array_query%dimension_indices) /= 1 .or. &
            array_query%dimension_indices(1) /= 0) then
            write (error_unit, '(A)') &
                'FAIL: inferred shape is not the deferred (:) sentinel'
            error stop 1
        end if
    end do

    if (.not. found_function) then
        write (error_unit, '(A)') 'FAIL: array_sum function not found'
        error stop 1
    end if

    call emit_fortran(result%arena, result%root_index, output)
    if (index(output, 'real, intent(in) :: arr(:)') == 0 .and. &
        index(output, 'real(dp), intent(in) :: arr(:)') == 0) then
        write (error_unit, '(A)') &
            'FAIL: deferred shape was not emitted as (:)'
        error stop 1
    end if

    write (*, '(A)') 'PASS: issue #1968 array shape API and codegen'

contains

    include '../common/read_example.inc'
end program test_issue_1968_array_dimension_api
