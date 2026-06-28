module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
                                   subroutine_def_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
                               array_literal_node, program_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
                               parameter_declaration_node, module_node, &
                               submodule_node
    use ast_nodes_misc, only: interface_block_node, import_statement_node, &
                               use_statement_node
    use ast_nodes_conditional, only: select_case_node, case_block_node, &
                                     case_default_node, case_range_node, &
                                     select_type_node, type_guard_block_node
    implicit none
    private

    public :: is_subroutine_call_statement
    public :: get_subroutine_call_name
    public :: get_subroutine_call_arg_indices
    public :: is_binary_op
    public :: get_binary_op_info
    public :: is_literal
    public :: get_literal_info
    public :: is_identifier
    public :: get_identifier_name
    public :: get_declaration_initializer
    public :: get_derived_type_components
    public :: get_array_literal_elements
    public :: get_import_list
    public :: get_interface_block_body
    public :: has_bind_c_attribute
    public :: get_bind_c_name
    public :: get_select_case_info
    public :: get_case_block_info
    public :: get_case_default_body
    public :: get_case_range_info
    public :: get_select_type_info
    public :: get_type_guard_info
    public :: get_dummy_allocatable_attribute
    public :: get_program_body_info
    public :: get_module_body_info
    public :: get_function_body_info
    public :: get_subroutine_body_info
    public :: get_used_modules
    public :: get_defined_module
    public :: used_module_t
    public :: defined_module_t

    type :: used_module_t
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only = .false.
        logical :: is_intrinsic = .false.
    end type used_module_t

    type :: defined_module_t
        character(len=:), allocatable :: name
        logical :: is_submodule = .false.
        character(len=:), allocatable :: parent_identifier
    end type defined_module_t

contains

    logical function is_identifier(arena, node_index) result(is_id)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_id = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            is_id = .true.
        end select
    end function is_identifier

    subroutine get_identifier_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'identifier index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an identifier'
        end select
    end subroutine get_identifier_name

    logical function is_literal(arena, node_index) result(is_lit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_lit = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            is_lit = .true.
        end select
    end function is_literal

    subroutine get_literal_info(arena, node_index, value, literal_type, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: value
        character(len=:), allocatable, intent(out) :: literal_type
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(value)
        call set_empty(literal_type)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'literal index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            if (allocated(node%value)) value = node%value
            if (allocated(node%literal_type)) literal_type = node%literal_type
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a literal'
        end select
    end subroutine get_literal_info

    logical function is_binary_op(arena, node_index) result(is_op)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_op = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            is_op = .true.
        end select
    end function is_binary_op

    subroutine get_binary_op_info(arena, node_index, operator, left_index, &
                                  right_index, line, column, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: operator
        integer, intent(out) :: left_index
        integer, intent(out) :: right_index
        integer, intent(out) :: line
        integer, intent(out) :: column
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(operator)
        left_index = 0
        right_index = 0
        line = 0
        column = 0
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'binary op index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (allocated(node%operator)) operator = node%operator
            left_index = node%left_index
            right_index = node%right_index
            line = node%line
            column = node%column
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a binary operation'
        end select
    end subroutine get_binary_op_info

    logical function is_subroutine_call_statement(arena, node_index) result(is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_call = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            is_call = .true.
        end select
    end function is_subroutine_call_statement

    subroutine get_subroutine_call_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            if (.not. allocated(node%name)) then
                error_msg = 'subroutine call node has no callee name'
                return
            end if
            name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_name

    subroutine get_subroutine_call_arg_indices(arena, node_index, arg_indices, &
                                               error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: arg_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        allocate (arg_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            if (allocated(node%arg_indices)) then
                if (size(node%arg_indices) > 0) arg_indices = node%arg_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_arg_indices

    ! Compiler-facing query: return the arena index of a declaration's
    ! initializer expression, or 0 if the declaration has none.
    function get_declaration_initializer(arena, decl_index) result(init_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        integer :: init_index

        init_index = 0
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (node%has_initializer .and. node%initializer_index > 0) then
                init_index = node%initializer_index
            end if
        end select
    end function get_declaration_initializer

    ! Compiler-facing query: copy the component_indices of a derived
    ! type definition into an allocatable result.
    subroutine get_derived_type_components(arena, type_index, components)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        integer, allocatable, intent(out) :: components(:)

        if (.not. arena%has_node_at(type_index)) then
            allocate (components(0))
            return
        end if
        select type (node => arena%entries(type_index)%node)
        type is (derived_type_node)
            if (allocated(node%component_indices)) then
                components = node%component_indices
            else
                allocate (components(0))
            end if
        class default
            allocate (components(0))
        end select
    end subroutine get_derived_type_components

    ! Compiler-facing query: copy the element_indices of an array
    ! literal (Fortran array constructor) into an allocatable result.
    subroutine get_array_literal_elements(arena, node_index, elements)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: elements(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (elements(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (array_literal_node)
            if (allocated(node%element_indices)) then
                elements = node%element_indices
            else
                allocate (elements(0))
            end if
        class default
            allocate (elements(0))
        end select
    end subroutine get_array_literal_elements

    ! Compiler-facing query: copy the import_list of an import_statement_node
    ! into an allocatable character array.
    subroutine get_import_list(arena, node_index, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: i, max_len

        if (.not. arena%has_node_at(node_index)) then
            allocate (character(len=1) :: names(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (import_statement_node)
            if (.not. allocated(node%import_list)) then
                allocate (character(len=1) :: names(0))
                return
            end if
            max_len = 1
            do i = 1, size(node%import_list)
                if (allocated(node%import_list(i)%s)) then
                    if (len(node%import_list(i)%s) > max_len) &
                        max_len = len(node%import_list(i)%s)
                end if
            end do
            allocate (character(len=max_len) :: names(size(node%import_list)))
            do i = 1, size(node%import_list)
                if (allocated(node%import_list(i)%s)) then
                    names(i) = node%import_list(i)%s
                else
                    names(i) = ''
                end if
            end do
        class default
            allocate (character(len=1) :: names(0))
        end select
    end subroutine get_import_list

    ! Compiler-facing query: return whether an interface_block_node has
    ! body indices and copy them out.
    subroutine get_interface_block_body(arena, node_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (interface_block_node)
            if (allocated(node%procedure_indices)) then
                body_indices = node%procedure_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_interface_block_body

    ! Compiler-facing query: whether a function/subroutine definition has
    ! a bind(c) clause.
    function has_bind_c_attribute(arena, node_index) result(present_flag)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: present_flag

        present_flag = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (function_def_node)
            present_flag = allocated(node%bind_c_clause)
        type is (subroutine_def_node)
            present_flag = allocated(node%bind_c_clause)
        end select
    end function has_bind_c_attribute

    ! Compiler-facing query: return the bind-name from a bind(c, name="...")
    ! clause if specified; empty string otherwise.
    function get_bind_c_name(arena, node_index) result(bind_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: bind_name

        bind_name = ''
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (function_def_node)
            if (allocated(node%bind_c_clause)) bind_name = node%bind_c_clause
        type is (subroutine_def_node)
            if (allocated(node%bind_c_clause)) bind_name = node%bind_c_clause
        end select
    end function get_bind_c_name

    ! Compiler-facing query: return the selector index, the case-arm arena
    ! indices, and the optional default-arm arena index of a select_case_node.
    subroutine get_select_case_info(arena, node_index, selector_index, &
                                    case_indices, default_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: selector_index
        integer, allocatable, intent(out) :: case_indices(:)
        integer, intent(out) :: default_index

        selector_index = 0
        default_index = 0
        if (.not. arena%has_node_at(node_index)) then
            allocate (case_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (select_case_node)
            selector_index = node%selector_index
            default_index = node%default_index
            if (allocated(node%case_indices)) then
                case_indices = node%case_indices
            else
                allocate (case_indices(0))
            end if
        class default
            allocate (case_indices(0))
        end select
    end subroutine get_select_case_info

    ! Compiler-facing query: copy a case_block_node's case-value and body
    ! indices.
    subroutine get_case_block_info(arena, node_index, value_indices, &
                                   body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: value_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (value_indices(0))
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (case_block_node)
            if (allocated(node%value_indices)) then
                value_indices = node%value_indices
            else
                allocate (value_indices(0))
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (value_indices(0))
            allocate (body_indices(0))
        end select
    end subroutine get_case_block_info

    ! Compiler-facing query: copy a case_default_node's body indices.
    subroutine get_case_default_body(arena, node_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (case_default_node)
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_case_default_body

    ! Compiler-facing query: return the inclusive start/end integer values of
    ! a case_range_node.
    subroutine get_case_range_info(arena, node_index, start_value, end_value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: start_value
        integer, intent(out) :: end_value

        start_value = 0
        end_value = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (case_range_node)
            start_value = node%start_value
            end_value = node%end_value
        end select
    end subroutine get_case_range_info

    ! Compiler-facing query: return the selector index, the type-guard arena
    ! indices, and the optional class-default arena index of a
    ! select_type_node.
    subroutine get_select_type_info(arena, node_index, selector_index, &
                                    guard_indices, default_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: selector_index
        integer, allocatable, intent(out) :: guard_indices(:)
        integer, intent(out) :: default_index

        selector_index = 0
        default_index = 0
        if (.not. arena%has_node_at(node_index)) then
            allocate (guard_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (select_type_node)
            selector_index = node%selector_index
            default_index = node%default_index
            if (allocated(node%guard_indices)) then
                guard_indices = node%guard_indices
            else
                allocate (guard_indices(0))
            end if
        class default
            allocate (guard_indices(0))
        end select
    end subroutine get_select_type_info

    ! Compiler-facing query: return the guard kind string, the type-name
    ! identifier arena index, and the body statement indices of a
    ! type_guard_block_node.
    subroutine get_type_guard_info(arena, node_index, guard_type, &
                                   type_name_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: guard_type
        integer, intent(out) :: type_name_index
        integer, allocatable, intent(out) :: body_indices(:)

        type_name_index = 0
        guard_type = ''
        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (type_guard_block_node)
            guard_type = trim(node%guard_type)
            type_name_index = node%type_name_index
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_type_guard_info

    ! Compiler-facing query: whether a declaration carries the allocatable
    ! attribute.
    function get_dummy_allocatable_attribute(arena, node_index) result(is_alloc)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_alloc

        is_alloc = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            is_alloc = node%is_allocatable
        type is (parameter_declaration_node)
            is_alloc = .false.
        end select
    end function get_dummy_allocatable_attribute

    ! Compiler-facing query: return the name and body statement indices of a
    ! program_node.  Wrong node kind returns zero-length body_indices, an empty
    ! name, and a non-empty error_msg naming the expected node kind.
    subroutine get_program_body_info(arena, node_index, name, body_indices, &
                                      error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'program body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a program; expected program_node'
        end select
    end subroutine get_program_body_info

    ! Compiler-facing query: return the name, declaration indices, and
    ! procedure (contains-section) indices of a module_node.
    subroutine get_module_body_info(arena, node_index, name, &
                                     declaration_indices, procedure_indices, &
                                     error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: declaration_indices(:)
        integer, allocatable, intent(out) :: procedure_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(declaration_indices)) deallocate (declaration_indices)
        if (allocated(procedure_indices)) deallocate (procedure_indices)
        allocate (declaration_indices(0))
        allocate (procedure_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'module body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (module_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%declaration_indices)) then
                declaration_indices = node%declaration_indices
            end if
            if (allocated(node%procedure_indices)) then
                procedure_indices = node%procedure_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a module; expected module_node'
        end select
    end subroutine get_module_body_info

    ! Compiler-facing query: return the name, parameter indices, body indices,
    ! and result-variable name of a function_def_node.
    subroutine get_function_body_info(arena, node_index, name, param_indices, &
                                       body_indices, result_name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: param_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: result_name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        call set_empty(result_name)
        if (allocated(param_indices)) deallocate (param_indices)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (param_indices(0))
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'function body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (function_def_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%param_indices)) then
                param_indices = node%param_indices
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            if (allocated(node%result_variable)) result_name = node%result_variable
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a function; expected function_def_node'
        end select
    end subroutine get_function_body_info

    ! Compiler-facing query: return the name, parameter indices, and body
    ! indices of a subroutine_def_node.
    subroutine get_subroutine_body_info(arena, node_index, name, param_indices, &
                                         body_indices, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: param_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(param_indices)) deallocate (param_indices)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (param_indices(0))
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
        type is (subroutine_def_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%param_indices)) then
                param_indices = node%param_indices
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a subroutine; expected subroutine_def_node'
        end select
    end subroutine get_subroutine_body_info

    subroutine get_used_modules(arena, modules)
        type(ast_arena_t), intent(in) :: arena
        type(used_module_t), allocatable, intent(out) :: modules(:)
        integer :: i, count, k

        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (use_statement_node)
                count = count + 1
            end select
        end do

        allocate (modules(count))
        k = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (use_statement_node)
                k = k + 1
                if (allocated(node%module_name)) then
                    modules(k)%module_name = node%module_name
                else
                    modules(k)%module_name = ''
                end if
                modules(k)%has_only = node%has_only
                modules(k)%is_intrinsic = node%is_intrinsic
                call copy_string_t_array(node%only_list, modules(k)%only_list)
                call copy_string_t_array(node%rename_list, modules(k)%rename_list)
            end select
        end do
    end subroutine get_used_modules

    subroutine get_defined_module(arena, module_info, error_msg)
        type(ast_arena_t), intent(in) :: arena
        type(defined_module_t), intent(out) :: module_info
        character(len=:), allocatable, intent(out) :: error_msg
        integer :: i

        call set_empty(error_msg)
        module_info%name = ''
        module_info%is_submodule = .false.
        module_info%parent_identifier = ''

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (allocated(node%name)) then
                    module_info%name = node%name
                else
                    module_info%name = ''
                end if
                module_info%is_submodule = .false.
                call set_empty(error_msg)
                return
            type is (submodule_node)
                if (allocated(node%name)) then
                    module_info%name = node%name
                else
                    module_info%name = ''
                end if
                module_info%is_submodule = .true.
                if (allocated(node%parent_identifier)) then
                    module_info%parent_identifier = node%parent_identifier
                end if
                call set_empty(error_msg)
                return
            end select
        end do

        error_msg = 'no module or submodule definition found in arena'
    end subroutine get_defined_module

    subroutine copy_string_t_array(src, dst)
        type(string_t), allocatable, intent(in) :: src(:)
        character(len=:), allocatable, intent(out) :: dst(:)
        integer :: i, max_len

        if (.not. allocated(src)) then
            allocate (character(len=1) :: dst(0))
            return
        end if
        if (size(src) == 0) then
            allocate (character(len=1) :: dst(0))
            return
        end if
        max_len = 1
        do i = 1, size(src)
            if (allocated(src(i)%s)) then
                if (len(src(i)%s) > max_len) max_len = len(src(i)%s)
            end if
        end do
        allocate (character(len=max_len) :: dst(size(src)))
        do i = 1, size(src)
            if (allocated(src(i)%s)) then
                dst(i) = src(i)%s
            else
                dst(i) = ''
            end if
        end do
    end subroutine copy_string_t_array

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_queries
