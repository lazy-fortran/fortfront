module ast_factory_expressions
    use ast_core
    use ast_factory_core, only: validate_arena, validate_node_index
    implicit none
    private

    ! Public expression node creation functions
    public :: push_call_or_subscript, push_subroutine_call
    public :: push_call_or_subscript_with_slice_detection
    public :: build_ast_from_nodes

contains

    ! Create call_or_subscript node and add to stack
    function push_call_or_subscript(arena, name, arg_indices, line, column, &
                                   parent_index) result(call_index)
        use intrinsic_registry, only: get_intrinsic_info
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(call_or_subscript_node) :: call_node

        call_node = create_call_or_subscript(name, arg_indices, line, column)
        
        ! Set intrinsic function information efficiently
        call get_intrinsic_info(name, call_node%is_intrinsic, &
            call_node%intrinsic_signature)
        
        call arena%push(call_node, "call_or_subscript", parent_index)
        call_index = arena%size
    end function push_call_or_subscript

    ! Create subroutine call node and add to stack
    function push_subroutine_call(arena, name, arg_indices, line, column, &
                                 parent_index) result(call_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: call_index
        type(subroutine_call_node) :: call_node

        call_node = create_subroutine_call(name, arg_indices, line, column)
        call arena%push(call_node, "subroutine_call", parent_index)
        call_index = arena%size
    end function push_subroutine_call

    ! Create call or subscript with array slice detection
    function push_call_or_subscript_with_slice_detection(arena, name, &
        arg_indices, line, column, parent_index) result(node_index)
        use ast_factory_arrays, only: push_array_slice
        use ast_factory_core, only: push_identifier
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: node_index
        logical :: has_slice
        integer :: i
        
        ! Check if any argument is a range expression (array slice)
        has_slice = .false.
        do i = 1, size(arg_indices)
            if (arg_indices(i) > 0 .and. arg_indices(i) <= arena%size) then
                if (allocated(arena%entries(arg_indices(i))%node)) then
                    select type (node => arena%entries(arg_indices(i))%node)
                    type is (range_expression_node)
                        has_slice = .true.
                        exit
                    end select
                end if
            end if
        end do
        
        if (has_slice) then
            ! Always create array slice at parse time
            ! Semantic analysis will set is_character_substring flag for character types
            block
                integer :: array_name_index
                array_name_index = push_identifier(arena, name, line, column, &
                    parent_index)
                node_index = push_array_slice(arena, array_name_index, &
                    arg_indices, size(arg_indices), line, column, parent_index)
            end block
        else
            ! Regular function call or array indexing
            node_index = push_call_or_subscript(arena, name, arg_indices, &
                                               line, column, parent_index)
        end if
    end function push_call_or_subscript_with_slice_detection

    ! Build AST from individual nodes (helper function)
    subroutine build_ast_from_nodes(arena, node_specs, indices)
        use ast_factory_core, only: push_identifier, push_literal
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: node_specs(:)  ! Array of "type:name" specs
        integer, intent(out) :: indices(:)  ! Output indices
        integer :: i

        do i = 1, size(node_specs)
            block
                character(len=:), allocatable :: spec
                integer :: colon_pos
                character(len=:), allocatable :: node_type, node_name

                spec = trim(node_specs(i))
                colon_pos = index(spec, ':')

                if (colon_pos > 0) then
                    node_type = spec(1:colon_pos - 1)
                    node_name = spec(colon_pos + 1:)

                    select case (trim(node_type))
                    case ('identifier')
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    case ('literal_int')
                      indices(i) = push_literal(arena, node_name, LITERAL_INTEGER, i, 1)
                    case ('literal_real')
                        indices(i) = push_literal(arena, node_name, LITERAL_REAL, i, 1)
                    case ('literal_string')
                       indices(i) = push_literal(arena, node_name, LITERAL_STRING, i, 1)
                    case default
                        indices(i) = push_identifier(arena, node_name, i, 1)
                    end select
                else
                    indices(i) = push_identifier(arena, spec, i, 1)
                end if
            end block
        end do
    end subroutine build_ast_from_nodes

end module ast_factory_expressions