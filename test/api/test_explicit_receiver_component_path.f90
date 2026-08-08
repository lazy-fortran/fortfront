program test_explicit_receiver_component_path
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, query_type_bound_call, type_bound_call_query_t, &
        query_declaration, declaration_query_t
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(type_bound_call_query_t) :: call_query
    type(declaration_query_t) :: declaration
    character(len=:), allocatable :: source
    integer :: i, explicit_calls

    call read_example('examples/f90/type_bound_nested_receiver_query.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'nested receiver example was rejected')

    explicit_calls = 0
    do i = 1, result%arena%size
        call_query = query_type_bound_call(result%arena, i)
        if (trim(call_query%receiver_name) /= 'outer%inner') cycle
        if (call_query%receiver_node_index /= 0) cycle
        explicit_calls = explicit_calls + 1
        call require(call_query%receiver_path%found, &
            'explicit receiver component path was not exposed')
        call require(size(call_query%receiver_path%component_names) == 1, &
            'explicit receiver component path has the wrong length')
        call require(trim(call_query%receiver_path%component_names(1)) == &
            'inner', 'explicit receiver component name is wrong')
        call require(size(call_query%receiver_path%component_declaration_indices) == 1, &
            'explicit receiver declaration identity is missing')
        declaration = query_declaration(result%arena, &
            call_query%receiver_path%component_declaration_indices(1))
        call require(declaration%found, &
            'explicit receiver declaration query failed')
        call require(trim(declaration%name) == 'inner', &
            'explicit receiver declaration identity is wrong')
        call require(size(call_query%receiver_path%component_node_indices) == 1, &
            'explicit receiver AST identity array is missing')
        call require(call_query%receiver_path%component_node_indices(1) == 0 .and. &
            call_query%receiver_path%base_node_index == 0, &
            'explicit receiver invented an AST component node')
        call require(call_query%receiver_path%base_rank == -1 .and. &
            call_query%receiver_path%rank == -1 .and. &
            .not. call_query%receiver_path%is_array_element .and. &
            .not. call_query%receiver_path%is_array_section, &
            'source-only receiver claimed unavailable shape facts')
    end do

    call require(explicit_calls == 4, &
        'explicit nested receiver calls were not enumerated exactly')
    print *, 'PASS: explicit receiver component path contract'

contains

    include '../common/read_example.inc'

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_explicit_receiver_component_path
