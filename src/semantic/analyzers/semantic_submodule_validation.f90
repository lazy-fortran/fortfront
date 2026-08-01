module semantic_submodule_validation
    ! Consistency between a module procedure interface body and the separate
    ! module subprogram that implements it (Fortran 2018 15.6.2.5).
    !
    ! Two constraints are checked, and both need the ancestor module, so this
    ! is the earliest layer with enough information: the parser only ever sees
    ! one program unit at a time.
    !
    !   * A subprogram in a submodule whose name matches a module procedure
    !     interface in the ancestor module implements that interface and must
    !     carry the MODULE prefix.
    !   * C1550: when the interface body specifies a binding label, the
    !     separate module subprogram must specify the same one.
    !
    ! Nothing is reported when the ancestor module is not in the same file,
    ! because then there is no interface to compare against.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: module_node, submodule_node
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    public :: validate_submodule_interfaces

contains

    ! Name of the ancestor module, i.e. the part of parent_identifier before
    ! any ":parent-submodule-name".
    function ancestor_module_name(parent_identifier) result(name)
        character(len=*), intent(in) :: parent_identifier
        character(len=:), allocatable :: name
        integer :: colon_pos

        colon_pos = index(parent_identifier, ':')
        if (colon_pos > 0) then
            name = to_lower(trim(parent_identifier(1:colon_pos - 1)))
        else
            name = to_lower(trim(parent_identifier))
        end if
    end function ancestor_module_name

    logical function has_module_prefix(prefix_keywords) result(has_prefix)
        character(len=16), allocatable, intent(in) :: prefix_keywords(:)
        integer :: i

        has_prefix = .false.
        if (.not. allocated(prefix_keywords)) return
        do i = 1, size(prefix_keywords)
            if (to_lower(trim(prefix_keywords(i))) == 'module') then
                has_prefix = .true.
                return
            end if
        end do
    end function has_module_prefix

    ! Explicit binding label of a bind(c, name="...") clause. The result is
    ! unallocated when the clause is absent or carries no NAME= specifier,
    ! because then there is no label to compare.
    function binding_label(bind_c_clause) result(label)
        character(len=:), allocatable, intent(in) :: bind_c_clause
        character(len=:), allocatable :: label
        character(len=:), allocatable :: lowered
        integer :: name_pos, first_quote, last_quote
        character :: quote_char

        if (.not. allocated(bind_c_clause)) return
        lowered = to_lower(bind_c_clause)
        name_pos = index(lowered, 'name')
        if (name_pos == 0) return
        if (index(lowered(name_pos:), '=') == 0) return

        first_quote = 0
        do first_quote = name_pos, len(bind_c_clause)
            quote_char = bind_c_clause(first_quote:first_quote)
            if (quote_char == '"' .or. quote_char == "'") exit
        end do
        if (first_quote > len(bind_c_clause)) return

        quote_char = bind_c_clause(first_quote:first_quote)
        last_quote = index(bind_c_clause(first_quote + 1:), quote_char)
        if (last_quote == 0) return
        label = bind_c_clause(first_quote + 1:first_quote + last_quote - 1)
    end function binding_label

    ! Characteristics of one module procedure interface body.
    subroutine interface_procedure_facts(arena, node_index, name, is_module, &
            label)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: is_module
        character(len=:), allocatable, intent(out) :: label

        is_module = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (function_def_node)
            if (allocated(node%name)) name = to_lower(trim(node%name))
            is_module = has_module_prefix(node%prefix_keywords)
            label = binding_label(node%bind_c_clause)
        type is (subroutine_def_node)
            if (allocated(node%name)) name = to_lower(trim(node%name))
            is_module = has_module_prefix(node%prefix_keywords)
            label = binding_label(node%bind_c_clause)
        end select
    end subroutine interface_procedure_facts

    ! Locate the interface body in module_index that declares a module
    ! procedure called proc_name. Returns 0 when there is none.
    function find_interface_body(arena, module_index, proc_name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: module_index
        character(len=*), intent(in) :: proc_name
        integer :: found
        integer :: i, j, decl_index, proc_index
        character(len=:), allocatable :: name
        character(len=:), allocatable :: label
        logical :: is_module

        found = 0
        if (.not. arena%has_node_at(module_index)) return

        select type (host => arena%entries(module_index)%node)
        type is (module_node)
            if (.not. allocated(host%declaration_indices)) return
            do i = 1, size(host%declaration_indices)
                decl_index = host%declaration_indices(i)
                if (.not. arena%has_node_at(decl_index)) cycle
                select type (decl => arena%entries(decl_index)%node)
                type is (interface_block_node)
                    if (.not. allocated(decl%procedure_indices)) cycle
                    do j = 1, size(decl%procedure_indices)
                        proc_index = decl%procedure_indices(j)
                        call interface_procedure_facts(arena, proc_index, name, &
                            is_module, label)
                        if (.not. is_module) cycle
                        if (.not. allocated(name)) cycle
                        if (name /= proc_name) cycle
                        found = proc_index
                        return
                    end do
                end select
            end do
        end select
    end function find_interface_body

    ! Index of the module_node named module_name, or 0 when the ancestor
    ! module is not part of this compilation unit.
    function find_module(arena, module_name) result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_name
        integer :: found
        integer :: i

        found = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) /= module_name) cycle
                found = i
                return
            end select
        end do
    end function find_module

    subroutine report(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: suggestion
        integer, intent(in) :: line, column

        call errors%add_error(message=message, code=ERROR_SEMANTIC, &
            component='semantic_submodule_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), suggestion=suggestion, &
            line=line, column=column, end_line=line, end_column=column + 1)
    end subroutine report

    ! Compare one separate module subprogram against its interface body.
    subroutine check_definition(arena, def_index, module_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: def_index
        integer, intent(in) :: module_index
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: name, label, interface_label
        character(len=:), allocatable :: interface_name
        logical :: is_module, interface_is_module
        integer :: interface_index, line, column

        if (.not. arena%has_node_at(def_index)) return

        call interface_procedure_facts(arena, def_index, name, is_module, label)
        if (.not. allocated(name)) return

        interface_index = find_interface_body(arena, module_index, name)
        if (interface_index == 0) return

        line = arena%entries(def_index)%node%line
        column = arena%entries(def_index)%node%column

        if (.not. is_module) then
            call report(errors, 'separate module subprogram "'//name// &
                '" requires the MODULE prefix', &
                'add the MODULE prefix to the subprogram statement', &
                line, column)
            return
        end if

        call interface_procedure_facts(arena, interface_index, interface_name, &
            interface_is_module, interface_label)
        if (.not. allocated(interface_label)) return
        if (.not. allocated(label)) return
        ! A MODULE PROCEDURE body carries no binding label of its own; it
        ! inherits the one from its interface body (F2023 C1554).
        if (len_trim(label) == 0) return
        if (label == interface_label) return

        call report(errors, 'mismatch in BIND(C) names ("'//label//'"/"'// &
            interface_label//'") for module procedure "'//name//'"', &
            'use the binding label of the module procedure interface body', &
            line, column)
    end subroutine check_definition

    ! Entry point: check every submodule in the arena whose ancestor module is
    ! present in the same file.
    subroutine validate_submodule_interfaces(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i, j, module_index
        character(len=:), allocatable :: parent_name

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (submodule_node)
                if (.not. allocated(node%parent_identifier)) cycle
                if (.not. allocated(node%procedure_indices)) cycle
                parent_name = ancestor_module_name(node%parent_identifier)
                if (len(parent_name) == 0) cycle
                module_index = find_module(arena, parent_name)
                if (module_index == 0) cycle
                do j = 1, size(node%procedure_indices)
                    call check_definition(arena, node%procedure_indices(j), &
                        module_index, errors)
                end do
            end select
        end do
    end subroutine validate_submodule_interfaces

end module semantic_submodule_validation
