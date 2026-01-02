module codegen_arena_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node
    implicit none
    private

    public :: check_result_is_simple_scalar
    public :: find_node_index_in_arena
    public :: same_node

contains

    subroutine check_result_is_simple_scalar(arena, func_node, result_name, is_scalar)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        logical, intent(out) :: is_scalar
        integer :: i
        integer :: decl_index

        is_scalar = .true.
        if (len_trim(result_name) == 0) return
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            decl_index = func_node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (trim(decl%var_name) /= trim(result_name)) cycle
                if (decl%is_array) then
                    is_scalar = .false.
                    return
                end if
                if (allocated(decl%dimension_indices)) then
                    if (size(decl%dimension_indices) > 0) then
                        is_scalar = .false.
                        return
                    end if
                end if
            end select
        end do
    end subroutine check_result_is_simple_scalar

    function find_node_index_in_arena(arena, target_node) result(index)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: target_node
        integer :: index
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            if (same_node(arena%entries(i)%node, target_node)) then
                index = i
                return
            end if
        end do
    end function find_node_index_in_arena

    function same_node(node1, node2) result(is_same)
        class(ast_node), intent(in) :: node1
        class(ast_node), intent(in) :: node2
        logical :: is_same

        is_same = .false.

        select type (n1 => node1)
        type is (identifier_node)
            select type (n2 => node2)
            type is (identifier_node)
                is_same = (n1%name == n2%name)
            end select
        type is (literal_node)
            select type (n2 => node2)
            type is (literal_node)
                is_same = (n1%literal_type == n2%literal_type)
            end select
        class default
            is_same = .false.
        end select
    end function same_node

end module codegen_arena_utils
