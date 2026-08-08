program test_polymorphic_array_type_bound_call
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, query_storage, storage_query_t, &
        type_bound_call_query_t, query_type_bound_call
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(type_bound_call_query_t) :: call_query
    type(storage_query_t) :: receiver_storage
    character(len=:), allocatable :: source
    integer :: i, values_calls, children_calls

    call read_example('examples/f90/polymorphic_array_storage_query.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    call require(result%success(), 'polymorphic array example was rejected')

    values_calls = 0
    children_calls = 0
    do i = 1, result%arena%size
        if (.not. result%arena%has_node_at(i)) cycle
        call_query = query_type_bound_call(result%arena, i)
        if (.not. call_query%found) cycle
        if (trim(call_query%binding_name) /= 'run') cycle

        select case (trim(call_query%receiver_name))
        case ('values(i)')
            values_calls = values_calls + 1
            call require(call_query%receiver_node_index == 0, &
                'explicit array call invented a receiver AST node')
            call require(call_query%receiver_declaration_index > 0 .and. &
                trim(call_query%declared_type_name) == 'base_t' .and. &
                call_query%is_deferred .and. .not. call_query%is_resolved, &
                'polymorphic array receiver binding facts are incomplete')
            receiver_storage = query_storage(result%arena, &
                call_query%receiver_declaration_index)
            call require(receiver_storage%found .and. &
                receiver_storage%rank == 1 .and. &
                receiver_storage%is_polymorphic .and. &
                receiver_storage%is_abstract_type, &
                'polymorphic array receiver storage identity is incomplete')
            call require(size(call_query%dispatch_target_type_indices) == 1 .and. &
                trim(call_query%dispatch_target_implementations(1)) == &
                'run_child', &
                'polymorphic array dispatch target was not exposed')
        case ('children(i)')
            children_calls = children_calls + 1
            call require(call_query%receiver_declaration_index > 0 .and. &
                trim(call_query%declared_type_name) == 'child_t' .and. &
                call_query%is_resolved .and. &
                trim(call_query%implementation) == 'run_child', &
                'concrete array receiver binding facts are incomplete')
            receiver_storage = query_storage(result%arena, &
                call_query%receiver_declaration_index)
            call require(receiver_storage%found .and. &
                receiver_storage%rank == 1 .and. &
                .not. receiver_storage%is_polymorphic .and. &
                receiver_storage%is_concrete_derived, &
                'concrete array receiver storage identity is incomplete')
        end select
    end do

    call require(values_calls == 1 .and. children_calls == 1, &
        'explicit polymorphic-array type-bound calls were not enumerated')
    print *, 'PASS: polymorphic array type-bound call query contract'

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

end program test_polymorphic_array_type_bound_call
