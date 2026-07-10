program test_compiler_expression_queries
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: ast_arena_t, compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, array_slice_query_t, array_bounds_query_t, &
        range_expression_query_t, component_access_query_t, &
        array_literal_query_t, pointer_assignment_query_t, nullify_query_t, &
        query_array_slice, query_array_bounds, query_range_expression, &
        query_component_access, query_array_literal, query_pointer_assignment, &
        query_nullify, get_identifier_name, get_literal_info
    implicit none

    character(len=:), allocatable :: source
    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options

    call read_example('examples/f90/compiler_expression_queries.f90', source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) call fail('frontend rejected expression source')

    call test_positive_queries(result%arena)
    call test_absent_queries(result%arena, result%root_index)
    call test_absent_queries(result%arena, 0)
    print *, 'PASS: compiler expression queries'

contains

    include '../common/read_example.inc'

    subroutine test_positive_queries(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: i
        integer :: counts(8)

        counts = 0
        do i = 1, arena%size
            call inspect_expression_node(arena, i, counts)
            call inspect_statement_node(arena, i, counts)
        end do

        call require_equal(counts(1), 1, 'array slice count')
        call require_equal(counts(2), 1, 'array bounds count')
        call require_equal(counts(3), 2, 'range expression count')
        call require_equal(counts(4), 1, 'component access count')
        call require_equal(counts(5), 1, 'array literal count')
        call require_equal(counts(6), 1, 'pointer assignment count')
        call require_equal(counts(7), 1, 'NULLIFY count')
        call require_equal(counts(8), 1, 'explicit array bounds count')
    end subroutine test_positive_queries

    subroutine inspect_expression_node(arena, node_index, counts)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(inout) :: counts(8)
        type(array_slice_query_t) :: slice
        type(array_bounds_query_t) :: bounds
        type(range_expression_query_t) :: range
        type(component_access_query_t) :: component
        type(array_literal_query_t) :: literal

        slice = query_array_slice(arena, node_index)
        if (slice%found) then
            counts(1) = counts(1) + 1
            call check_slice(arena, slice)
        end if
        bounds = query_array_bounds(arena, node_index)
        if (bounds%found) then
            counts(2) = counts(2) + 1
            call check_bounds(bounds)
        end if
        range = query_range_expression(arena, node_index)
        if (range%found) then
            counts(3) = counts(3) + 1
            call check_range(arena, range)
            if (literal_text(arena, range%start_node_index) == '2') then
                counts(8) = counts(8) + 1
            end if
        end if
        component = query_component_access(arena, node_index)
        if (component%found) then
            counts(4) = counts(4) + 1
            call check_component(arena, component)
        end if
        literal = query_array_literal(arena, node_index)
        if (literal%found) then
            counts(5) = counts(5) + 1
            call check_array_literal(arena, literal)
        end if
    end subroutine inspect_expression_node

    subroutine inspect_statement_node(arena, node_index, counts)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(inout) :: counts(8)
        type(pointer_assignment_query_t) :: pointer_assignment
        type(nullify_query_t) :: nullify_statement

        pointer_assignment = query_pointer_assignment(arena, node_index)
        if (pointer_assignment%found) then
            counts(6) = counts(6) + 1
            call check_pointer_assignment(arena, pointer_assignment)
        end if
        nullify_statement = query_nullify(arena, node_index)
        if (nullify_statement%found) then
            counts(7) = counts(7) + 1
            call check_nullify(arena, nullify_statement)
        end if
    end subroutine inspect_statement_node

    subroutine check_slice(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_query_t), intent(in) :: query
        type(range_expression_query_t) :: range

        call require_identifier(arena, query%base_node_index, 'values')
        if (.not. allocated(query%bounds_node_indices)) then
            call fail('slice bounds are not initialized')
        end if
        call require_equal(size(query%bounds_node_indices), 1, 'slice rank')
        range = query_range_expression(arena, query%bounds_node_indices(1))
        if (.not. range%found) call fail('slice bound is not a range expression')
        call require_range(arena, range, '1', '5', '2')
        if (query%is_character_substring) then
            call fail('integer array slice reported as substring')
        end if
    end subroutine check_slice

    subroutine check_bounds(query)
        type(array_bounds_query_t), intent(in) :: query

        call require_equal(query%lower_bound_node_index, 0, 'bounds lower')
        call require_equal(query%upper_bound_node_index, 0, 'bounds upper')
        call require_equal(query%stride_node_index, 0, 'bounds stride')
        if (.not. query%is_assumed_size) call fail('assumed-size bound lost')
        if (query%is_assumed_shape) call fail('unexpected assumed-shape bound')
        if (query%is_deferred_shape) call fail('unexpected deferred-shape bound')
        if (query%is_assumed_rank) call fail('unexpected assumed-rank bound')
    end subroutine check_bounds

    subroutine check_range(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(range_expression_query_t), intent(in) :: query
        character(len=:), allocatable :: start_value

        start_value = literal_text(arena, query%start_node_index)
        select case (start_value)
        case ('1')
            call require_range(arena, query, '1', '5', '2')
        case ('2')
            call require_range(arena, query, '2', '6', '')
        case default
            call fail('unexpected range lower bound: '//start_value)
        end select
    end subroutine check_range

    subroutine check_component(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(component_access_query_t), intent(in) :: query

        call require_identifier(arena, query%base_node_index, 'box')
        if (.not. allocated(query%component_name)) then
            call fail('component name is not initialized')
        end if
        if (query%component_name /= 'value') call fail('component name mismatch')
    end subroutine check_component

    subroutine check_array_literal(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_query_t), intent(in) :: query

        if (.not. allocated(query%element_node_indices)) then
            call fail('array literal elements are not initialized')
        end if
        call require_equal(size(query%element_node_indices), 3, &
            'array literal size')
        call require_literal(arena, query%element_node_indices(1), '1')
        call require_literal(arena, query%element_node_indices(2), '2')
        call require_literal(arena, query%element_node_indices(3), '3')
        call require_string(query%element_type, '', 'array element type')
        call require_string(query%type_spec, 'integer', 'array type spec')
        call require_string(query%syntax_style, 'modern', 'array syntax style')
    end subroutine check_array_literal

    subroutine check_pointer_assignment(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(pointer_assignment_query_t), intent(in) :: query

        call require_identifier(arena, query%pointer_node_index, 'ptr')
        if (query%target_node_index <= 0) then
            call fail('pointer assignment target is absent')
        end if
        call require_identifier(arena, query%target_node_index, 'scalar_target')
    end subroutine check_pointer_assignment

    subroutine check_nullify(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(nullify_query_t), intent(in) :: query

        if (.not. allocated(query%pointer_node_indices)) then
            call fail('NULLIFY pointers are not initialized')
        end if
        call require_equal(size(query%pointer_node_indices), 1, 'NULLIFY size')
        call require_identifier(arena, query%pointer_node_indices(1), 'ptr')
    end subroutine check_nullify

    subroutine test_absent_queries(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        call check_absent_slice_bounds(arena, node_index)
        call check_absent_expression_values(arena, node_index)
        call check_absent_statements(arena, node_index)
    end subroutine test_absent_queries

    subroutine check_absent_slice_bounds(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(array_slice_query_t) :: slice
        type(array_bounds_query_t) :: bounds

        slice = query_array_slice(arena, node_index)
        if (slice%found) call fail('wrong-kind slice query succeeded')
        call require_equal(slice%base_node_index, 0, 'absent slice base')
        if (.not. allocated(slice%bounds_node_indices)) then
            call fail('absent slice bounds are not initialized')
        end if
        call require_equal(size(slice%bounds_node_indices), 0, &
            'absent slice bounds')
        if (slice%is_character_substring) then
            call fail('absent slice has substring flag')
        end if

        bounds = query_array_bounds(arena, node_index)
        if (bounds%found) call fail('wrong-kind bounds query succeeded')
        call require_equal(bounds%lower_bound_node_index, 0, &
            'absent bounds lower')
        call require_equal(bounds%upper_bound_node_index, 0, &
            'absent bounds upper')
        call require_equal(bounds%stride_node_index, 0, 'absent bounds stride')
        if (bounds%is_assumed_shape) call fail('absent bounds are assumed shape')
        if (bounds%is_deferred_shape) call fail('absent bounds are deferred shape')
        if (bounds%is_assumed_size) call fail('absent bounds are assumed size')
        if (bounds%is_assumed_rank) call fail('absent bounds are assumed rank')
    end subroutine check_absent_slice_bounds

    subroutine check_absent_expression_values(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(range_expression_query_t) :: range
        type(component_access_query_t) :: component
        type(array_literal_query_t) :: literal

        range = query_range_expression(arena, node_index)
        if (range%found) call fail('wrong-kind range query succeeded')
        call require_equal(range%start_node_index, 0, 'absent range start')
        call require_equal(range%end_node_index, 0, 'absent range end')
        call require_equal(range%stride_node_index, 0, 'absent range stride')

        component = query_component_access(arena, node_index)
        if (component%found) call fail('wrong-kind component query succeeded')
        call require_equal(component%base_node_index, 0, 'absent component base')
        call require_string(component%component_name, '', &
            'absent component name')

        literal = query_array_literal(arena, node_index)
        if (literal%found) call fail('wrong-kind array literal query succeeded')
        if (.not. allocated(literal%element_node_indices)) then
            call fail('absent literal elements are not initialized')
        end if
        call require_equal(size(literal%element_node_indices), 0, &
            'absent literal elements')
        call require_string(literal%element_type, '', 'absent element type')
        call require_string(literal%type_spec, '', 'absent type spec')
        call require_string(literal%syntax_style, '', 'absent syntax style')
    end subroutine check_absent_expression_values

    subroutine check_absent_statements(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(pointer_assignment_query_t) :: pointer_assignment
        type(nullify_query_t) :: nullify_statement

        pointer_assignment = query_pointer_assignment(arena, node_index)
        if (pointer_assignment%found) then
            call fail('wrong-kind pointer assignment query succeeded')
        end if
        call require_equal(pointer_assignment%pointer_node_index, 0, &
            'absent pointer assignment pointer')
        call require_equal(pointer_assignment%target_node_index, 0, &
            'absent pointer assignment target')

        nullify_statement = query_nullify(arena, node_index)
        if (nullify_statement%found) call fail('wrong-kind NULLIFY query succeeded')
        if (.not. allocated(nullify_statement%pointer_node_indices)) then
            call fail('absent NULLIFY pointers are not initialized')
        end if
        call require_equal(size(nullify_statement%pointer_node_indices), 0, &
            'absent NULLIFY pointers')
    end subroutine check_absent_statements

    subroutine require_identifier(arena, node_index, expected)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected
        character(len=:), allocatable :: name, error_msg

        call get_identifier_name(arena, node_index, name, error_msg)
        if (len_trim(error_msg) > 0) call fail(error_msg)
        if (name /= expected) call fail('identifier mismatch: '//name)
    end subroutine require_identifier

    subroutine require_literal(arena, node_index, expected)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected
        character(len=:), allocatable :: value, literal_type, error_msg

        call get_literal_info(arena, node_index, value, literal_type, error_msg)
        if (len_trim(error_msg) > 0) call fail(error_msg)
        if (value /= expected) call fail('literal mismatch: '//value)
    end subroutine require_literal

    subroutine require_range(arena, query, start_value, end_value, stride_value)
        type(ast_arena_t), intent(in) :: arena
        type(range_expression_query_t), intent(in) :: query
        character(len=*), intent(in) :: start_value, end_value, stride_value

        call require_literal(arena, query%start_node_index, start_value)
        call require_literal(arena, query%end_node_index, end_value)
        if (len(stride_value) > 0) then
            call require_literal(arena, query%stride_node_index, stride_value)
        else
            call require_equal(query%stride_node_index, 0, 'range stride')
        end if
    end subroutine require_range

    function literal_text(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: value
        character(len=:), allocatable :: literal_type, error_msg

        call get_literal_info(arena, node_index, value, literal_type, error_msg)
        if (len_trim(error_msg) > 0) call fail(error_msg)
    end function literal_text

    subroutine require_string(actual, expected, label)
        character(len=:), allocatable, intent(in) :: actual
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: label

        if (.not. allocated(actual)) call fail(label//' is not initialized')
        if (actual /= expected) call fail(label//' mismatch')
    end subroutine require_string

    subroutine require_equal(actual, expected, label)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: label

        if (actual /= expected) call fail(label//' mismatch')
    end subroutine require_equal

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_compiler_expression_queries
