module frontend_compiler_node_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_control, only: goto_node
    use ast_nodes_data, only: declaration_node, derived_type_node
    implicit none
    private

    public :: is_declaration_node
    public :: is_derived_type_node
    public :: get_declaration_var_name
    public :: get_declaration_type_name
    public :: get_declaration_has_initializer
    public :: get_declaration_initializer_index
    public :: get_declaration_initializer_was_overridden
    public :: get_declaration_shape_was_overridden
    public :: get_derived_type_name
    public :: get_node_stmt_label
    public :: get_goto_label
    public :: goto_is_computed
    public :: get_goto_label_list
    public :: get_goto_selector_index

contains

    logical function is_declaration_node(arena, node_index) result(is_decl)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_decl = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            is_decl = .true.
        end select
    end function is_declaration_node

    logical function is_derived_type_node(arena, node_index) result(is_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_type = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (derived_type_node)
            is_type = .true.
        end select
    end function is_derived_type_node

    subroutine get_declaration_var_name(arena, decl_index, var_name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        character(len=:), allocatable, intent(out) :: var_name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(var_name)
        if (.not. arena%has_node_at(decl_index)) then
            error_msg = 'declaration index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (allocated(node%var_name)) var_name = node%var_name
            if (len_trim(var_name) == 0 .and. node%is_multi_declaration .and. &
                allocated(node%var_names)) then
                if (size(node%var_names) == 1) var_name = node%var_names(1)
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a declaration'
        end select
    end subroutine get_declaration_var_name

    subroutine get_declaration_type_name(arena, decl_index, type_name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        character(len=:), allocatable, intent(out) :: type_name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(type_name)
        if (.not. arena%has_node_at(decl_index)) then
            error_msg = 'declaration index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (allocated(node%type_name)) type_name = node%type_name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a declaration'
        end select
    end subroutine get_declaration_type_name

    logical function get_declaration_has_initializer(arena, decl_index) &
            result(has_init)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index

        has_init = .false.
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            has_init = node%has_initializer
        end select
    end function get_declaration_has_initializer

    logical function get_declaration_initializer_was_overridden(arena, &
            decl_index) result(overridden)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index

        overridden = .false.
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            overridden = node%initializer_was_overridden
        end select
    end function get_declaration_initializer_was_overridden

    logical function get_declaration_shape_was_overridden(arena, decl_index) &
            result(overridden)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index

        overridden = .false.
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            overridden = node%shape_was_overridden
        end select
    end function get_declaration_shape_was_overridden

    integer function get_declaration_initializer_index(arena, decl_index) &
            result(init_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index

        init_index = 0
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (node%has_initializer .and. node%initializer_index > 0) then
                init_index = node%initializer_index
            end if
        end select
    end function get_declaration_initializer_index

    subroutine get_derived_type_name(arena, type_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(type_index)) then
            error_msg = 'derived type index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            if (allocated(node%name)) name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a derived type definition'
        end select
    end subroutine get_derived_type_name

    function get_node_stmt_label(arena, node_index) result(label)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: label

        call set_empty(label)
        if (.not. arena%has_node_at(node_index)) return
        if (allocated(arena%entries(node_index)%node%stmt_label)) then
            label = arena%entries(node_index)%node%stmt_label
        end if
    end function get_node_stmt_label

    function get_goto_label(arena, node_index) result(label)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: label

        call set_empty(label)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (goto_node)
            if (allocated(node%label)) label = node%label
        end select
    end function get_goto_label

    logical function goto_is_computed(arena, node_index) result(is_computed)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_computed = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (goto_node)
            is_computed = node%selector_index /= 0 .or. allocated(node%label_list)
        end select
    end function goto_is_computed

    function get_goto_label_list(arena, node_index) result(label_list)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: label_list

        call set_empty(label_list)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (goto_node)
            if (allocated(node%label_list)) label_list = node%label_list
        end select
    end function get_goto_label_list

    integer function get_goto_selector_index(arena, node_index) result(idx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        idx = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (goto_node)
            idx = node%selector_index
        end select
    end function get_goto_selector_index

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_node_queries
