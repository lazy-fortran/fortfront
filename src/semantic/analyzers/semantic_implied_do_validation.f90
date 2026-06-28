module semantic_implied_do_validation
    ! Validates implied-DO index locality in array constructors (F2018 R773).
    ! An implied-DO variable is local to its controlling implied-DO. Two rules
    ! are enforced here:
    !   1. The index is not in scope within its own controlling triplet, so a
    !      reference to the index name inside that loop's bound expressions is
    !      an out-of-scope reference unless an enclosing implied-DO defines it.
    !   2. A nested implied-DO must not reuse the index name of an enclosing
    !      implied-DO, which would shadow the outer index within the same
    !      array constructor.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: array_literal_node, identifier_node, &
        binary_op_node, call_or_subscript_node
    use ast_nodes_loops, only: do_loop_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_implied_do_array

contains

    ! Validate an array_literal_node for implied-DO index locality. Array
    ! literals without implied-DO elements are accepted unchanged.
    subroutine validate_implied_do_array(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: enclosing(:)
        integer :: i

        if (.not. allocated(node%element_indices)) return

        allocate (character(len=0) :: enclosing(0))
        do i = 1, size(node%element_indices)
            call check_element(arena, node%element_indices(i), enclosing, errors)
        end do
    end subroutine validate_implied_do_array

    ! Inspect one array-constructor element. An implied-DO element is a
    ! do_loop_node; other elements are ordinary expressions and skipped.
    recursive subroutine check_element(arena, elem_index, enclosing, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: elem_index
        character(len=*), intent(in) :: enclosing(:)
        type(error_collection_t), intent(inout) :: errors

        if (.not. arena%has_node_at(elem_index)) return

        select type (elem => arena%entries(elem_index)%node)
            type is (do_loop_node)
            call check_implied_do(arena, elem, enclosing, errors)
        end select
    end subroutine check_element

    ! Validate a single implied-DO loop and recurse into any nested implied-DO
    ! it carries. enclosing holds the index names of the implied-DOs that
    ! contain this one.
    recursive subroutine check_implied_do(arena, loop, enclosing, errors)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: loop
        character(len=*), intent(in) :: enclosing(:)
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: index_name
        character(len=:), allocatable :: inner(:)
        integer :: i

        index_name = ''
        if (allocated(loop%var_name)) index_name = trim(loop%var_name)
        if (len(index_name) == 0) return

        if (name_in_set(index_name, enclosing)) then
            call report_duplicate_index(errors, index_name, loop%line, loop%column)
        end if

        ! The index is not in scope within its own controlling triplet.
        if (bounds_reference_index(arena, loop, index_name)) then
            call report_out_of_scope(errors, index_name, loop%line, loop%column)
        end if

        inner = extend_set(enclosing, index_name)
        if (allocated(loop%body_indices)) then
            do i = 1, size(loop%body_indices)
                call check_body_node(arena, loop%body_indices(i), inner, errors)
            end do
        end if
    end subroutine check_implied_do

    ! Recurse into a body node looking for nested implied-DO loops. A nested
    ! array literal carries its own implied-DO element as a do_loop_node.
    recursive subroutine check_body_node(arena, node_index, enclosing, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: enclosing(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (do_loop_node)
            call check_implied_do(arena, node, enclosing, errors)
            type is (array_literal_node)
            if (allocated(node%element_indices)) then
                do i = 1, size(node%element_indices)
                    call check_body_node(arena, node%element_indices(i), &
                        enclosing, errors)
                end do
            end if
        end select
    end subroutine check_body_node

    ! Whether the loop's bound expressions reference index_name.
    function bounds_reference_index(arena, loop, index_name) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: loop
        character(len=*), intent(in) :: index_name
        logical :: found

        found = expr_references_name(arena, loop%start_expr_index, index_name)
        if (found) return
        found = expr_references_name(arena, loop%end_expr_index, index_name)
        if (found) return
        found = expr_references_name(arena, loop%step_expr_index, index_name)
    end function bounds_reference_index

    ! Whether the expression subtree rooted at expr_index references name.
    recursive function expr_references_name(arena, expr_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: name
        logical :: found
        integer :: i

        found = .false.
        if (expr_index <= 0) return
        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) found = trim(node%name) == name
            type is (binary_op_node)
            found = expr_references_name(arena, node%left_index, name)
            if (.not. found) &
                found = expr_references_name(arena, node%right_index, name)
            type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    found = expr_references_name(arena, node%arg_indices(i), name)
                    if (found) return
                end do
            end if
        end select
    end function expr_references_name

    ! Whether name is present in a set of index names.
    function name_in_set(name, names) result(is_present)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: names(:)
        logical :: is_present
        integer :: i

        is_present = .false.
        do i = 1, size(names)
            if (trim(names(i)) == name) then
                is_present = .true.
                return
            end if
        end do
    end function name_in_set

    ! Return names with name appended.
    function extend_set(names, name) result(extended)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: extended(:)
        integer :: new_len
        integer :: i

        new_len = max(len(names), len(name))
        allocate (character(len=new_len) :: extended(size(names) + 1))
        do i = 1, size(names)
            extended(i) = names(i)
        end do
        extended(size(names) + 1) = name
    end function extend_set

    subroutine report_duplicate_index(errors, name, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: name
        integer, intent(in) :: line, column

        call errors%add_error( &
            message='implied-DO index "'//name//'" shadows an enclosing '// &
            'implied-DO index of the same name', &
            code=ERROR_SEMANTIC, &
            component='semantic_implied_do_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='rename the nested implied-DO index')
    end subroutine report_duplicate_index

    subroutine report_out_of_scope(errors, name, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: name
        integer, intent(in) :: line, column

        call errors%add_error( &
            message='implied-DO index "'//name//'" referenced in its own '// &
            'controlling triplet, where it is not in scope', &
            code=ERROR_SEMANTIC, &
            component='semantic_implied_do_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='use a variable in scope for the implied-DO bounds')
    end subroutine report_out_of_scope

end module semantic_implied_do_validation
