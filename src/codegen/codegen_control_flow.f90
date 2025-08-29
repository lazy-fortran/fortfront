module codegen_control_flow
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_control
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_utilities, only: generate_grouped_body
    use codegen_arena_interface, only: generate_code_from_arena
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
        if (node%condition_index > 0 .and. node%condition_index <= arena%size) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ""
        end if

        ! Generate if statement
        code = "if (" // cond_code // ") then"

        ! Generate then body
        if (allocated(node%then_body_indices)) then
            body_code = generate_grouped_body_internal(arena, node%then_body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate else if blocks
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                cond_code = generate_code_from_arena(arena, node%elseif_blocks(i)%condition_index)
                code = code // new_line('A') // repeat("    ", indent_level) // "else if (" // cond_code // ") then"
                
                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    body_code = generate_grouped_body_internal(arena, node%elseif_blocks(i)%body_indices, indent_level + 1)
                    if (len(body_code) > 0) then
                        code = code // new_line('A') // body_code
                    end if
                end if
            end do
        end if

        ! Generate else block
        if (allocated(node%else_body_indices)) then
            code = code // new_line('A') // repeat("    ", indent_level) // "else"
            body_code = generate_grouped_body_internal(arena, node%else_body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end if
        code = code // new_line('A') // repeat("    ", indent_level) // "end if"
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
        if (node%start_expr_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
        else
            start_code = ""
        end if

        if (node%end_expr_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
        else
            end_code = ""
        end if

        if (node%step_expr_index > 0) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
        else
            step_code = ""
        end if

        ! Generate do statement
        code = "do " // var_code // " = " // start_code // ", " // end_code

        if (len(step_code) > 0) then
            code = code // ", " // step_code
        end if

        ! Generate body
        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal(arena, node%body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end do
        code = code // new_line('A') // repeat("    ", indent_level) // "end do"
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
        if (node%condition_index > 0) then
            cond_code = generate_code_from_arena(arena, node%condition_index)
        else
            cond_code = ""
        end if

        ! Generate do while statement
        code = "do while (" // cond_code // ")"

        ! Generate body
        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_internal(arena, node%body_indices, indent_level + 1)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end do
        code = code // new_line('A') // repeat("    ", indent_level) // "end do"
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
        if (node%selector_index > 0) then
            expr_code = generate_code_from_arena(arena, node%selector_index)
        else
            expr_code = ""
        end if

        ! Generate select case statement
        code = "select case (" // expr_code // ")"

        ! Generate case blocks
        if (allocated(node%case_indices)) then
            do i = 1, size(node%case_indices)
                if (node%case_indices(i) > 0 .and. node%case_indices(i) <= arena%size) then
                    select type (case_node => arena%entries(node%case_indices(i))%node)
                    type is (case_block_node)
                        ! Generate case statement
                        code = code // new_line('A') // repeat("    ", indent_level)
                        
                        code = code // "case ("
                        
                        ! Generate case values
                        if (allocated(case_node%value_indices)) then
                            do j = 1, size(case_node%value_indices)
                                if (j > 1) code = code // ", "
                                
                                ! Check for range
                                if (case_node%value_indices(j) > 0) then
                                    case_code = generate_code_from_arena(arena, case_node%value_indices(j))
                                    code = code // case_code
                                end if
                            end do
                        end if
                        
                        code = code // ")"
                        
                        ! Generate case body
                        if (allocated(case_node%body_indices)) then
                            body_code = generate_grouped_body_internal(arena, case_node%body_indices, indent_level + 1)
                            if (len(body_code) > 0) then
                                code = code // new_line('A') // body_code
                            end if
                        end if
                    end select
                end if
            end do
        end if

        ! Handle default case if present
        if (node%default_index > 0) then
            code = code // new_line('A') // repeat("    ", indent_level) // "case default"
            body_code = generate_code_from_arena(arena, node%default_index)
            if (len(body_code) > 0) then
                code = code // new_line('A') // body_code
            end if
        end if

        ! Generate end select
        code = code // new_line('A') // repeat("    ", indent_level) // "end select"
    end function generate_code_select_case

    ! Generate code for where constructs
    function generate_code_where(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: mask_code

        ! Generate mask expression
        if (node%mask_expr_index > 0) then
            mask_code = generate_code_from_arena(arena, node%mask_expr_index)
        else
            mask_code = ".true."
        end if

        ! Generate simplified where construct
        ! TODO: Implement proper where body and elsewhere clause generation
        code = "where (" // mask_code // ")" // new_line('A') // &
               "    ! TODO: implement where body" // new_line('A') // &
               "end where"
    end function generate_code_where

    ! Generate code for forall constructs
    function generate_code_forall(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(forall_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate simplified forall construct
        ! TODO: Implement proper forall triplet and mask generation
        code = "forall (i = 1:10)" // new_line('A') // &
               "    ! TODO: implement forall body" // new_line('A') // &
               "end forall"
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
                        assoc_code = generate_code_from_arena(arena, node%associations(i)%expr_index)
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
        
        ! Use stub implementation
        code = generate_grouped_body(arena, body_indices, indent)
    end function generate_grouped_body_internal

end module codegen_control_flow