module codegen_control_flow
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_control
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    implicit none
    private

    public :: generate_code_if
    public :: generate_code_do_loop
    public :: generate_code_do_while
    public :: generate_code_select_case
    public :: generate_code_where
    public :: generate_code_forall
    public :: generate_code_associate

contains

    ! Generate code for if statements
    function generate_code_if(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate condition
        if (node%condition > 0 .and. node%condition <= arena%size) then
            cond_code = generate_code_polymorphic_internal(arena, node%condition)
        else
            cond_code = ""
        end if

        ! Generate if statement
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": if (" // cond_code // ") then"
        else
            code = "if (" // cond_code // ") then"
        end if

        ! Generate then body
        if (allocated(node%then_body)) then
            body_code = generate_grouped_body_internal(arena, node%then_body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate else if blocks
        if (allocated(node%elseif_conditions)) then
            do i = 1, size(node%elseif_conditions)
                cond_code = generate_code_polymorphic_internal(arena, node%elseif_conditions(i))
                code = code // new_line('A') // repeat("    ", indent_level) // "else if (" // cond_code // ") then"
                
                if (allocated(node%elseif_bodies)) then
                    if (i <= size(node%elseif_bodies)) then
                        if (allocated(node%elseif_bodies(i)%body)) then
                            body_code = generate_grouped_body_internal(arena, node%elseif_bodies(i)%body, indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code // new_line('A') // body_code
                            end if
                        end if
                    end if
                end if
            end do
        end if

        ! Generate else block
        if (allocated(node%else_body)) then
            code = code // new_line('A') // repeat("    ", indent_level) // "else"
            body_code = generate_grouped_body_internal(arena, node%else_body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end if
        code = code // new_line('A') // repeat("    ", indent_level) 
        if (allocated(node%construct_name)) then
            code = code // "end if " // node%construct_name
        else
            code = code // "end if"
        end if
    end function generate_code_if

    ! Generate code for do loops
    function generate_code_do_loop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: var_code, start_code, end_code, step_code
        character(len=:), allocatable :: body_code
        integer :: indent_level

        indent_level = 0

        ! Generate loop variable
        if (allocated(node%var_name)) then
            var_code = node%var_name
        else
            var_code = ""
        end if

        ! Generate loop bounds
        if (node%start_expr > 0) then
            start_code = generate_code_polymorphic_internal(arena, node%start_expr)
        else
            start_code = ""
        end if

        if (node%end_expr > 0) then
            end_code = generate_code_polymorphic_internal(arena, node%end_expr)
        else
            end_code = ""
        end if

        if (node%step_expr > 0) then
            step_code = generate_code_polymorphic_internal(arena, node%step_expr)
        else
            step_code = ""
        end if

        ! Generate do statement
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": do " // var_code // " = " // start_code // ", " // end_code
        else
            code = "do " // var_code // " = " // start_code // ", " // end_code
        end if

        if (len(step_code) > 0) then
            code = code // ", " // step_code
        end if

        ! Generate body
        if (allocated(node%body)) then
            body_code = generate_grouped_body_internal(arena, node%body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end do
        code = code // new_line('A') // repeat("    ", indent_level)
        if (allocated(node%construct_name)) then
            code = code // "end do " // node%construct_name
        else
            code = code // "end do"
        end if
    end function generate_code_do_loop

    ! Generate code for do while loops
    function generate_code_do_while(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_while_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: cond_code, body_code
        integer :: indent_level

        indent_level = 0

        ! Generate condition
        if (node%condition > 0) then
            cond_code = generate_code_polymorphic_internal(arena, node%condition)
        else
            cond_code = ""
        end if

        ! Generate do while statement
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": do while (" // cond_code // ")"
        else
            code = "do while (" // cond_code // ")"
        end if

        ! Generate body
        if (allocated(node%body)) then
            body_code = generate_grouped_body_internal(arena, node%body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end do
        code = code // new_line('A') // repeat("    ", indent_level)
        if (allocated(node%construct_name)) then
            code = code // "end do " // node%construct_name
        else
            code = code // "end do"
        end if
    end function generate_code_do_while

    ! Generate code for select case statements
    function generate_code_select_case(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, case_code, body_code
        integer :: i, j, indent_level

        indent_level = 0

        ! Generate case expression
        if (node%case_expr > 0) then
            expr_code = generate_code_polymorphic_internal(arena, node%case_expr)
        else
            expr_code = ""
        end if

        ! Generate select case statement
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": select case (" // expr_code // ")"
        else
            code = "select case (" // expr_code // ")"
        end if

        ! Generate case blocks
        if (allocated(node%case_blocks)) then
            do i = 1, size(node%case_blocks)
                if (node%case_blocks(i) > 0 .and. node%case_blocks(i) <= arena%size) then
                    select type (case_node => arena%entries(node%case_blocks(i))%node)
                    type is (case_block_node)
                        ! Generate case statement
                        code = code // new_line('A') // repeat("    ", indent_level)
                        
                        if (case_node%is_default) then
                            code = code // "case default"
                        else
                            code = code // "case ("
                            
                            ! Generate case values
                            if (allocated(case_node%case_values)) then
                                do j = 1, size(case_node%case_values)
                                    if (j > 1) code = code // ", "
                                    
                                    ! Check for range
                                    if (case_node%case_values(j) > 0) then
                                        case_code = generate_code_polymorphic_internal(arena, case_node%case_values(j))
                                        code = code // case_code
                                    end if
                                end do
                            end if
                            
                            code = code // ")"
                        end if
                        
                        ! Generate case body
                        if (allocated(case_node%body)) then
                            body_code = generate_grouped_body_internal(arena, case_node%body, indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code // new_line('A') // body_code
                            end if
                        end if
                    end select
                end if
            end do
        end if

        ! Handle default case if separate
        if (allocated(node%default_body)) then
            code = code // new_line('A') // repeat("    ", indent_level) // "case default"
            body_code = generate_grouped_body_internal(arena, node%default_body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end select
        code = code // new_line('A') // repeat("    ", indent_level)
        if (allocated(node%construct_name)) then
            code = code // "end select " // node%construct_name
        else
            code = code // "end select"
        end if
    end function generate_code_select_case

    ! Generate code for where constructs
    function generate_code_where(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: mask_code, body_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate mask expression
        if (node%mask_expr > 0) then
            mask_code = generate_code_polymorphic_internal(arena, node%mask_expr)
        else
            mask_code = ""
        end if

        ! Check if single statement or construct
        if (node%is_single_statement .and. allocated(node%where_body)) then
            if (size(node%where_body) == 1) then
                body_code = generate_code_polymorphic_internal(arena, node%where_body(1))
                code = "where (" // mask_code // ") " // body_code
                return
            end if
        end if

        ! Generate where construct
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": where (" // mask_code // ")"
        else
            code = "where (" // mask_code // ")"
        end if

        ! Generate where body
        if (allocated(node%where_body)) then
            body_code = generate_grouped_body_internal(arena, node%where_body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate elsewhere blocks
        if (allocated(node%elsewhere_masks)) then
            do i = 1, size(node%elsewhere_masks)
                mask_code = generate_code_polymorphic_internal(arena, node%elsewhere_masks(i))
                code = code // new_line('A') // repeat("    ", indent_level) // "elsewhere (" // mask_code // ")"
                
                if (allocated(node%elsewhere_bodies)) then
                    if (i <= size(node%elsewhere_bodies)) then
                        if (allocated(node%elsewhere_bodies(i)%body)) then
                            body_code = generate_grouped_body_internal(arena, node%elsewhere_bodies(i)%body, indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code // new_line('A') // body_code
                            end if
                        end if
                    end if
                end if
            end do
        end if

        ! Generate elsewhere default
        if (allocated(node%elsewhere_default)) then
            code = code // new_line('A') // repeat("    ", indent_level) // "elsewhere"
            body_code = generate_grouped_body_internal(arena, node%elsewhere_default, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end where
        if (.not. node%is_single_statement) then
            code = code // new_line('A') // repeat("    ", indent_level)
            if (allocated(node%construct_name)) then
                code = code // "end where " // node%construct_name
            else
                code = code // "end where"
            end if
        end if
    end function generate_code_where

    ! Generate code for forall constructs
    function generate_code_forall(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(forall_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: triplet_code, mask_code, body_code
        character(len=:), allocatable :: var_code, start_code, end_code, stride_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate forall header
        if (allocated(node%construct_name)) then
            code = node%construct_name // ": forall ("
        else
            code = "forall ("
        end if

        ! Generate triplet specifications
        if (allocated(node%triplets)) then
            do i = 1, size(node%triplets)
                if (i > 1) code = code // ", "
                
                ! Variable name
                if (allocated(node%triplets(i)%var_name)) then
                    var_code = node%triplets(i)%var_name
                else
                    var_code = ""
                end if
                
                ! Range
                if (node%triplets(i)%start_expr > 0) then
                    start_code = generate_code_polymorphic_internal(arena, node%triplets(i)%start_expr)
                else
                    start_code = ""
                end if
                
                if (node%triplets(i)%end_expr > 0) then
                    end_code = generate_code_polymorphic_internal(arena, node%triplets(i)%end_expr)
                else
                    end_code = ""
                end if
                
                if (node%triplets(i)%stride_expr > 0) then
                    stride_code = generate_code_polymorphic_internal(arena, node%triplets(i)%stride_expr)
                    code = code // var_code // " = " // start_code // ":" // end_code // ":" // stride_code
                else
                    code = code // var_code // " = " // start_code // ":" // end_code
                end if
            end do
        end if

        ! Generate mask if present
        if (node%mask_expr > 0) then
            mask_code = generate_code_polymorphic_internal(arena, node%mask_expr)
            code = code // ", " // mask_code
        end if

        code = code // ")"

        ! Check if single statement or construct
        if (node%is_single_statement .and. allocated(node%body)) then
            if (size(node%body) == 1) then
                body_code = generate_code_polymorphic_internal(arena, node%body(1))
                code = code // " " // body_code
                return
            end if
        end if

        ! Generate body for construct form
        if (allocated(node%body)) then
            body_code = generate_grouped_body_internal(arena, node%body, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end forall for construct form
        if (.not. node%is_single_statement) then
            code = code // new_line('A') // repeat("    ", indent_level)
            if (allocated(node%construct_name)) then
                code = code // "end forall " // node%construct_name
            else
                code = code // "end forall"
            end if
        end if
    end function generate_code_forall

    ! Generate code for associate constructs
    function generate_code_associate(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(associate_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: assoc_code, body_code
        integer :: i, indent_level

        indent_level = 0

        ! Generate associate statement
        code = "associate ("

        ! Generate associations
        if (allocated(node%associations)) then
            do i = 1, size(node%associations)
                if (i > 1) code = code // ", "
                
                if (allocated(node%associations(i)%name)) then
                    code = code // node%associations(i)%name // " => "
                    
                    if (node%associations(i)%expr_index > 0) then
                        assoc_code = generate_code_polymorphic_internal(arena, node%associations(i)%expr_index)
                        code = code // assoc_code
                    end if
                end if
            end do
        end if

        code = code // ")"

        ! Generate body
        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal(arena, node%body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end associate
        code = code // new_line('A') // repeat("    ", indent_level)
        code = code // "end associate"
    end function generate_code_associate

    ! Internal function to generate grouped body
    function generate_grouped_body_internal(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code
        
        ! Import the utility function
        interface
            function generate_grouped_body(arena, body_indices, indent) result(code)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: body_indices(:)
                integer, intent(in) :: indent
                character(len=:), allocatable :: code
            end function generate_grouped_body
        end interface
        
        code = generate_grouped_body(arena, body_indices, indent)
    end function generate_grouped_body_internal

    ! Internal polymorphic code generator
    function generate_code_polymorphic_internal(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        ! Import the main dispatcher from codegen_core
        interface
            function generate_code_polymorphic(arena, node_index) result(code)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
                character(len=:), allocatable :: code
            end function generate_code_polymorphic
        end interface
        
        code = generate_code_polymorphic(arena, node_index)
    end function generate_code_polymorphic_internal

end module codegen_control_flow