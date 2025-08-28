module constant_transformation
    ! Module for compile-time constant folding and evaluation
    use ast_core
    use ast_arena_modern
    implicit none
    private
    
    public :: fold_constants_in_arena
    
contains
    
    subroutine fold_constants_in_arena(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i
        
        ! Process all nodes in the arena
        do i = 1, arena%size
            call fold_node_constants(arena, i)
        end do
    end subroutine fold_constants_in_arena
    
    recursive subroutine fold_node_constants(arena, node_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        
        if (node_index <= 0 .or. node_index > arena%size) return
        
        select type(node => arena%entries(node_index)%node)
        type is (literal_node)
            call fold_literal_node(node)
        type is (binary_op_node)
            call fold_binary_op_node(arena, node)
        type is (if_node)
            call fold_if_node(arena, node)
        type is (declaration_node)
            call fold_declaration_node(arena, node)
        type is (identifier_node)
            call fold_identifier_node(arena, node)
        end select
    end subroutine fold_node_constants
    
    subroutine fold_literal_node(node)
        type(literal_node), intent(inout) :: node
        integer :: read_status
        
        ! Mark literals as constants and store their values
        node%is_constant = .true.
        
        ! Store the constant value based on type  
        if (node%value == ".false." .or. node%value == ".FALSE.") then
            node%constant_logical = .false.
            node%constant_type = LITERAL_LOGICAL
        else if (node%value == ".true." .or. node%value == ".TRUE.") then
            node%constant_logical = .true.
            node%constant_type = LITERAL_LOGICAL
        else if (node%literal_kind == LITERAL_INTEGER) then
            read(node%value, *, iostat=read_status) node%constant_integer
            if (read_status /= 0) then
                node%is_constant = .false.
                return
            end if
            node%constant_type = LITERAL_INTEGER
        else if (node%literal_kind == LITERAL_REAL) then
            read(node%value, *, iostat=read_status) node%constant_real
            if (read_status /= 0) then
                node%is_constant = .false.
                return
            end if
            node%constant_type = LITERAL_REAL
        end if
    end subroutine fold_literal_node
    
    subroutine fold_binary_op_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(inout) :: node
        
        ! Validate indices
        if (node%left_index <= 0 .or. node%left_index > arena%size) return
        if (node%right_index <= 0 .or. node%right_index > arena%size) return
        
        ! First ensure operands are folded
        call fold_node_constants(arena, node%left_index)
        call fold_node_constants(arena, node%right_index)
        
        ! Check if both operands are constants
        associate(left_node => arena%entries(node%left_index)%node, &
                  right_node => arena%entries(node%right_index)%node)
            if (.not. left_node%is_constant .or. .not. right_node%is_constant) return
            
            ! Fold comparison operators
            select case(trim(node%operator))
            case (">")
                call fold_comparison_gt(node, left_node, right_node)
            case ("<")
                call fold_comparison_lt(node, left_node, right_node)
            case (">=")
                call fold_comparison_ge(node, left_node, right_node)
            case ("<=")
                call fold_comparison_le(node, left_node, right_node)
            case ("==", ".eq.")
                call fold_comparison_eq(node, left_node, right_node)
            case ("/=", ".ne.")
                call fold_comparison_ne(node, left_node, right_node)
            end select
        end associate
    end subroutine fold_binary_op_node
    
    subroutine fold_comparison_gt(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer > right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real > right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_gt
    
    subroutine fold_comparison_lt(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer < right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real < right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_lt
    
    subroutine fold_comparison_ge(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer >= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real >= right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_ge
    
    subroutine fold_comparison_le(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer <= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_real <= right%constant_real
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_le
    
    subroutine fold_comparison_eq(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer == right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = &
                abs(left%constant_real - right%constant_real) < 1e-10
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_LOGICAL .and. &
                 right%constant_type == LITERAL_LOGICAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_logical .eqv. right%constant_logical
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_eq
    
    subroutine fold_comparison_ne(node, left, right)
        type(binary_op_node), intent(inout) :: node
        class(ast_node), intent(in) :: left, right
        
        if (left%constant_type == LITERAL_INTEGER .and. &
            right%constant_type == LITERAL_INTEGER) then
            node%is_constant = .true.
            node%constant_logical = left%constant_integer /= right%constant_integer
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_REAL .and. &
                 right%constant_type == LITERAL_REAL) then
            node%is_constant = .true.
            node%constant_logical = &
                abs(left%constant_real - right%constant_real) >= 1e-10
            node%constant_type = LITERAL_LOGICAL
        else if (left%constant_type == LITERAL_LOGICAL .and. &
                 right%constant_type == LITERAL_LOGICAL) then
            node%is_constant = .true.
            node%constant_logical = left%constant_logical .neqv. right%constant_logical
            node%constant_type = LITERAL_LOGICAL
        end if
    end subroutine fold_comparison_ne
    
    subroutine fold_if_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(inout) :: node
        
        ! Ensure the condition is folded
        if (node%condition_index > 0) then
            call fold_node_constants(arena, node%condition_index)
        end if
    end subroutine fold_if_node
    
    subroutine fold_declaration_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_node), intent(inout) :: node
        
        ! Check if this is a parameter declaration
        ! For now, assume parameter declarations have initializers that should be folded
        if (node%has_initializer .and. node%initializer_index > 0 .and. &
            node%initializer_index <= arena%size) then
            ! First ensure the initializer is folded
            call fold_node_constants(arena, node%initializer_index)
            
            ! If the initializer is now constant, mark this declaration as constant
            select type(init => arena%entries(node%initializer_index)%node)
            class default
                if (init%is_constant) then
                    node%is_constant = .true.
                    node%constant_type = init%constant_type
                    node%constant_integer = init%constant_integer
                    node%constant_real = init%constant_real  
                    node%constant_logical = init%constant_logical
                end if
            end select
        end if
    end subroutine fold_declaration_node
    
    subroutine fold_identifier_node(arena, node)
        type(ast_arena_t), intent(inout) :: arena
        type(identifier_node), intent(inout) :: node
        integer :: i
        
        ! Early exit if node name is not allocated
        if (.not. allocated(node%name)) return
        
        ! Look for declarations with matching name (including parameters)
        ! Only check nodes that come before this one in the arena
        do i = 1, arena%size
            select type(decl => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == trim(node%name)) then
                        ! Propagate the declaration's constant value
                        if (decl%is_constant) then
                            node%is_constant = .true.
                            node%constant_type = decl%constant_type
                            node%constant_integer = decl%constant_integer
                            node%constant_real = decl%constant_real
                            node%constant_logical = decl%constant_logical
                            return  ! Found match, no need to continue
                        end if
                    end if
                end if
            end select
        end do
    end subroutine fold_identifier_node
    
end module constant_transformation