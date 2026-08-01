module frontend_compiler_resolution
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_core, only: call_or_subscript_node, identifier_node, &
                              program_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
                              mixed_construct_container_node, module_node, &
                              parameter_declaration_node, submodule_node
    use ast_nodes_misc, only: interface_block_node, statement_function_node, &
                              use_statement_node, visibility_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    implicit none
    private

    integer, parameter, public :: BINDING_NONE = 0
    integer, parameter, public :: BINDING_DECLARATION = 1
    integer, parameter, public :: BINDING_NAMED_CONSTANT = 2
    integer, parameter, public :: BINDING_DUMMY_ARGUMENT = 3
    integer, parameter, public :: BINDING_DERIVED_TYPE = 4
    integer, parameter, public :: BINDING_FUNCTION = 5
    integer, parameter, public :: BINDING_SUBROUTINE = 6
    integer, parameter, public :: BINDING_FUNCTION_RESULT = 7
    integer, parameter, public :: BINDING_STATEMENT_FUNCTION = 8
    integer, parameter, public :: BINDING_GENERIC_INTERFACE = 9
    integer, parameter, public :: BINDING_ASSOCIATE_NAME = 10

    integer, parameter, public :: ASSOCIATION_NONE = 0
    integer, parameter, public :: ASSOCIATION_DIRECT = 1
    integer, parameter, public :: ASSOCIATION_HOST = 2
    integer, parameter, public :: ASSOCIATION_USE = 3

    type, public :: declaration_binding_t
        logical :: found = .false.
        character(len=:), allocatable :: name
        character(len=:), allocatable :: remote_name
        character(len=:), allocatable :: module_name
        integer :: declaration_node_index = 0
        integer :: declaration_entity_index = 0
        integer :: node_index = 0
        integer :: scope_node_index = 0
        integer :: binding_kind = BINDING_NONE
        integer :: association = ASSOCIATION_NONE
    end type declaration_binding_t

    public :: get_scope_bindings
    ! Scope enumeration primitives, shared with the semantic validators that
    ! diagnose per-scoping-unit conflicts (issues #2887 and #2888).
    public :: is_scope_node
    public :: get_scope_statement_indices
    public :: resolve_name_in_scope
    public :: resolve_name_at_node
    public :: resolve_identifier_binding
    public :: find_module_index
    ! Association-aware lookups used by the scope collision validator (#2888).
    public :: find_enclosing_scope
    public :: find_host_scope
    public :: resolve_use_binding

contains

    subroutine get_scope_bindings(arena, scope_node_index, bindings, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        type(declaration_binding_t), allocatable, intent(out) :: bindings(:)
        character(len=:), allocatable, intent(out) :: error_msg
        type(declaration_binding_t), allocatable :: tmp(:)
        integer :: count

        call set_empty(error_msg)
        if (.not. arena%has_node_at(scope_node_index)) then
            allocate (bindings(0))
            error_msg = 'scope index does not reference an AST node'
            return
        end if
        if (.not. is_scope_node(arena, scope_node_index)) then
            allocate (bindings(0))
            error_msg = 'AST node is not a lexical scope'
            return
        end if

        count = 0
        call collect_scope_bindings(arena, scope_node_index, ASSOCIATION_DIRECT, &
                                    tmp, count)
        call finish_bindings(tmp, count, bindings)
    end subroutine get_scope_bindings

    subroutine resolve_name_in_scope(arena, scope_node_index, name, binding, &
                                     error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable, intent(out) :: error_msg

        call clear_binding(binding)
        call set_empty(error_msg)
        if (.not. arena%has_node_at(scope_node_index)) then
            error_msg = 'scope index does not reference an AST node'
            return
        end if
        if (.not. is_scope_node(arena, scope_node_index)) then
            error_msg = 'AST node is not a lexical scope'
            return
        end if
        if (len_trim(name) == 0) return

        call resolve_visible_binding(arena, scope_node_index, trim(name), &
                                     binding, error_msg)
    end subroutine resolve_name_in_scope

    subroutine resolve_name_at_node(arena, reference_node_index, name, binding, &
                                    error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable, intent(out) :: error_msg
        integer :: scope_node_index

        call clear_binding(binding)
        call set_empty(error_msg)
        if (.not. arena%has_node_at(reference_node_index)) then
            error_msg = 'reference index does not reference an AST node'
            return
        end if
        if (len_trim(name) == 0) return

        scope_node_index = find_enclosing_scope(arena, reference_node_index)
        if (scope_node_index <= 0) return
        if (is_associate_selector_reference(arena, scope_node_index, &
                                            reference_node_index)) then
            scope_node_index = find_host_scope(arena, scope_node_index)
            if (scope_node_index <= 0) return
        end if
        call resolve_name_in_scope(arena, scope_node_index, name, binding, &
                                   error_msg)
    end subroutine resolve_name_at_node

    subroutine resolve_identifier_binding(arena, use_node_index, binding, &
                                          error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: use_node_index
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable, intent(out) :: error_msg
        character(len=:), allocatable :: name
        integer :: scope_node_index

        call clear_binding(binding)
        call set_empty(error_msg)
        if (.not. arena%has_node_at(use_node_index)) then
            error_msg = 'use index does not reference an AST node'
            return
        end if
        call node_reference_name(arena, use_node_index, name, error_msg)
        if (len_trim(error_msg) > 0) return
        if (len_trim(name) == 0) return

        call resolve_name_at_node(arena, use_node_index, name, binding, &
                                  error_msg)
    end subroutine resolve_identifier_binding

    recursive subroutine resolve_visible_binding(arena, scope_node_index, name, &
                                                 binding, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable, intent(out) :: error_msg
        integer :: host_scope

        call clear_binding(binding)
        call set_empty(error_msg)

        call resolve_local_binding(arena, scope_node_index, name, binding)
        if (binding%found) return

        call resolve_use_binding(arena, scope_node_index, name, binding)
        if (binding%found) return

        host_scope = find_host_scope(arena, scope_node_index)
        if (host_scope <= 0) return
        call resolve_visible_binding(arena, host_scope, name, binding, error_msg)
        if (binding%found) then
            if (binding%association == ASSOCIATION_DIRECT) then
                binding%association = ASSOCIATION_HOST
            end if
        end if
    end subroutine resolve_visible_binding

    subroutine resolve_local_binding(arena, scope_node_index, name, binding)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        type(declaration_binding_t), allocatable :: bindings(:)
        character(len=:), allocatable :: error_msg
        integer :: i

        call clear_binding(binding)
        call get_scope_bindings(arena, scope_node_index, bindings, error_msg)
        if (len_trim(error_msg) > 0) return
        do i = size(bindings), 1, -1
            if (.not. same_name(bindings(i)%name, name)) cycle
            binding = bindings(i)
            return
        end do
    end subroutine resolve_local_binding

    subroutine resolve_use_binding(arena, scope_node_index, name, binding)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        integer, allocatable :: indices(:)
        integer :: i

        call clear_binding(binding)
        call get_scope_statement_indices(arena, scope_node_index, indices)
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (use_statement_node)
                call resolve_from_use_statement(arena, node, name, binding)
                if (binding%found) return
            end select
        end do
    end subroutine resolve_use_binding

    subroutine resolve_from_use_statement(arena, node, name, binding)
        type(ast_arena_t), intent(in) :: arena
        type(use_statement_node), intent(in) :: node
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable :: remote_name
        character(len=:), allocatable :: module_name

        call clear_binding(binding)
        if (node%is_intrinsic) return
        if (.not. allocated(node%module_name)) return
        module_name = trim(node%module_name)
        if (len_trim(module_name) == 0) return

        if (find_rename_remote(node, name, remote_name)) then
            call resolve_module_export(arena, module_name, remote_name, binding)
        else if (node%has_only) then
            if (.not. list_contains(node%only_list, name)) return
            call resolve_module_export(arena, module_name, name, binding)
            remote_name = trim(name)
        else
            if (is_renamed_remote(node, name)) return
            call resolve_module_export(arena, module_name, name, binding)
            remote_name = trim(name)
        end if

        if (.not. binding%found) return
        binding%association = ASSOCIATION_USE
        binding%name = trim(name)
        binding%remote_name = trim(remote_name)
        binding%module_name = trim(module_name)
    end subroutine resolve_from_use_statement

    subroutine resolve_module_export(arena, module_name, name, binding)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        integer :: module_index

        call clear_binding(binding)
        module_index = find_module_index(arena, module_name)
        if (module_index <= 0) return
        call resolve_local_binding(arena, module_index, name, binding)
        if (.not. binding%found) return
        if (.not. module_binding_is_public(arena, module_index, name)) then
            call clear_binding(binding)
        end if
    end subroutine resolve_module_export

    subroutine collect_scope_bindings(arena, scope_node_index, association, &
                                      bindings, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count

        if (.not. arena%has_node_at(scope_node_index)) return
        select type (node => arena%entries(scope_node_index)%node)
        type is (program_node)
            call collect_from_indices(arena, node%body_indices, scope_node_index, &
                                      association, bindings, count)
        type is (module_node)
            call collect_from_indices(arena, node%declaration_indices, &
                                      scope_node_index, association, bindings, count)
            call collect_from_indices(arena, node%procedure_indices, &
                                      scope_node_index, association, bindings, count)
        type is (submodule_node)
            call collect_from_indices(arena, node%declaration_indices, &
                                      scope_node_index, association, bindings, count)
            call collect_from_indices(arena, node%procedure_indices, &
                                      scope_node_index, association, bindings, count)
        type is (function_def_node)
            call append_function_result(scope_node_index, node, association, &
                                        bindings, count)
            call collect_from_indices(arena, node%param_indices, scope_node_index, &
                                      association, bindings, count)
            call collect_from_indices(arena, node%body_indices, scope_node_index, &
                                      association, bindings, count)
        type is (subroutine_def_node)
            call collect_from_indices(arena, node%param_indices, scope_node_index, &
                                      association, bindings, count)
            call collect_from_indices(arena, node%body_indices, scope_node_index, &
                                      association, bindings, count)
        type is (associate_node)
            call append_associate_names(scope_node_index, node, association, &
                                        bindings, count)
            call collect_from_indices(arena, node%body_indices, scope_node_index, &
                                      association, bindings, count)
        type is (block_construct_node)
            call collect_from_indices(arena, node%body_indices, scope_node_index, &
                                      association, bindings, count)
        type is (mixed_construct_container_node)
            call collect_from_indices(arena, node%implicit_declaration_indices, &
                                      scope_node_index, association, bindings, count)
        end select
    end subroutine collect_scope_bindings

    subroutine collect_from_indices(arena, indices, scope_node_index, association, &
                                    bindings, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in), allocatable :: indices(:)
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        integer :: i

        if (.not. allocated(indices)) return
        do i = 1, size(indices)
            call collect_one_binding(arena, indices(i), scope_node_index, &
                                     association, bindings, count)
        end do
    end subroutine collect_from_indices

    subroutine collect_one_binding(arena, node_index, scope_node_index, &
                                   association, bindings, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count

        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            call append_declaration_names(node_index, scope_node_index, node, &
                                          association, bindings, count)
        type is (parameter_declaration_node)
            if (allocated(node%name)) then
                call append_named_binding(bindings, count, node%name, node_index, &
                                  scope_node_index, BINDING_DUMMY_ARGUMENT, association)
            end if
        type is (derived_type_node)
            if (allocated(node%name)) then
                call append_named_binding(bindings, count, node%name, node_index, &
                                    scope_node_index, BINDING_DERIVED_TYPE, association)
            end if
        type is (function_def_node)
            if (allocated(node%name)) then
                call append_named_binding(bindings, count, node%name, node_index, &
                                        scope_node_index, BINDING_FUNCTION, association)
            end if
        type is (subroutine_def_node)
            if (allocated(node%name)) then
                call append_named_binding(bindings, count, node%name, node_index, &
                                      scope_node_index, BINDING_SUBROUTINE, association)
            end if
        type is (statement_function_node)
            if (allocated(node%name)) then
                call append_named_binding(bindings, count, node%name, node_index, &
                              scope_node_index, BINDING_STATEMENT_FUNCTION, association)
            end if
        type is (interface_block_node)
            if (allocated(node%name)) then
                if (len_trim(node%name) > 0) then
                    call append_named_binding(bindings, count, node%name, &
                              node_index, scope_node_index, BINDING_GENERIC_INTERFACE, &
                                              association)
                end if
            end if
        end select
    end subroutine collect_one_binding

    subroutine append_declaration_names(node_index, scope_node_index, node, &
                                        association, bindings, count)
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_node_index
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        integer :: kind
        integer :: i

        kind = BINDING_DECLARATION
        if (node%is_parameter) kind = BINDING_NAMED_CONSTANT
        if (node%is_multi_declaration) then
            if (.not. allocated(node%var_names)) return
            do i = 1, size(node%var_names)
                call append_named_binding(bindings, count, node%var_names(i), &
                                        node_index, scope_node_index, kind, &
                                        association, i)
            end do
            return
        end if
        if (.not. allocated(node%var_name)) return
        call append_named_binding(bindings, count, node%var_name, node_index, &
                                  scope_node_index, kind, association, 1)
    end subroutine append_declaration_names

    subroutine append_function_result(scope_node_index, node, association, &
                                      bindings, count)
        integer, intent(in) :: scope_node_index
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        character(len=:), allocatable :: result_name

        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            result_name = trim(node%name)
        else
            return
        end if
        if (len_trim(result_name) == 0) return
        call append_named_binding(bindings, count, result_name, scope_node_index, &
                                 scope_node_index, BINDING_FUNCTION_RESULT, association)
    end subroutine append_function_result

    subroutine append_associate_names(scope_node_index, node, association, bindings, &
                                      count)
        integer, intent(in) :: scope_node_index
        type(associate_node), intent(in) :: node
        integer, intent(in) :: association
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        integer :: i

        if (.not. allocated(node%associations)) return
        do i = 1, size(node%associations)
            if (.not. allocated(node%associations(i)%name)) cycle
            call append_named_binding(bindings, count, node%associations(i)%name, &
                           scope_node_index, scope_node_index, BINDING_ASSOCIATE_NAME, &
                                      association, i)
        end do
    end subroutine append_associate_names

    logical function is_associate_selector_reference(arena, scope_node_index, &
            reference_node_index) result(is_selector)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: reference_node_index
        integer :: current
        integer :: i

        is_selector = .false.
        if (.not. arena%has_node_at(scope_node_index)) return
        select type (node => arena%entries(scope_node_index)%node)
        type is (associate_node)
            if (.not. allocated(node%associations)) return
            current = reference_node_index
            do while (current > 0 .and. current /= scope_node_index)
                do i = 1, size(node%associations)
                    if (current /= node%associations(i)%expr_index) cycle
                    is_selector = .true.
                    return
                end do
                current = arena%entries(current)%parent_index
            end do
        end select
    end function is_associate_selector_reference

    subroutine append_named_binding(bindings, count, name, node_index, &
                                    scope_node_index, kind, association, &
                                    declaration_entity_index)
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: name
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: kind
        integer, intent(in) :: association
        integer, intent(in), optional :: declaration_entity_index
        type(declaration_binding_t) :: binding

        if (len_trim(name) == 0) return
        call make_binding(binding, name, node_index, scope_node_index, kind, &
                          association, declaration_entity_index)
        call append_binding(bindings, count, binding)
    end subroutine append_named_binding

    subroutine append_binding(bindings, count, binding)
        type(declaration_binding_t), allocatable, intent(inout) :: bindings(:)
        integer, intent(inout) :: count
        type(declaration_binding_t), intent(in) :: binding
        type(declaration_binding_t), allocatable :: tmp(:)
        integer :: new_size

        if (.not. allocated(bindings)) then
            allocate (bindings(8))
        else if (count >= size(bindings)) then
            new_size = max(8, 2 * size(bindings))
            allocate (tmp(new_size))
            if (count > 0) tmp(1:count) = bindings(1:count)
            call move_alloc(tmp, bindings)
        end if
        count = count + 1
        bindings(count) = binding
    end subroutine append_binding

    subroutine finish_bindings(tmp, count, bindings)
        type(declaration_binding_t), allocatable, intent(inout) :: tmp(:)
        integer, intent(in) :: count
        type(declaration_binding_t), allocatable, intent(out) :: bindings(:)
        type(declaration_binding_t), allocatable :: exact(:)

        if (count <= 0) then
            allocate (bindings(0))
            return
        end if
        allocate (exact(count))
        exact = tmp(1:count)
        call move_alloc(exact, bindings)
    end subroutine finish_bindings

    subroutine get_scope_statement_indices(arena, scope_node_index, indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        integer, allocatable, intent(out) :: indices(:)

        allocate (indices(0))
        if (.not. arena%has_node_at(scope_node_index)) return
        select type (node => arena%entries(scope_node_index)%node)
        type is (program_node)
            call copy_indices(node%body_indices, indices)
        type is (module_node)
            call concat_indices(node%declaration_indices, node%procedure_indices, &
                                indices)
        type is (submodule_node)
            call concat_indices(node%declaration_indices, node%procedure_indices, &
                                indices)
        type is (function_def_node)
            call concat_indices(node%param_indices, node%body_indices, indices)
        type is (subroutine_def_node)
            call concat_indices(node%param_indices, node%body_indices, indices)
        type is (associate_node)
            call copy_indices(node%body_indices, indices)
        type is (block_construct_node)
            call copy_indices(node%body_indices, indices)
        type is (mixed_construct_container_node)
            call copy_indices(node%implicit_declaration_indices, indices)
        end select
    end subroutine get_scope_statement_indices

    subroutine copy_indices(source, dest)
        integer, intent(in), allocatable :: source(:)
        integer, allocatable, intent(out) :: dest(:)

        if (allocated(source)) then
            dest = source
        else
            allocate (dest(0))
        end if
    end subroutine copy_indices

    subroutine concat_indices(first, second, dest)
        integer, intent(in), allocatable :: first(:)
        integer, intent(in), allocatable :: second(:)
        integer, allocatable, intent(out) :: dest(:)
        integer :: n1
        integer :: n2

        n1 = 0
        n2 = 0
        if (allocated(first)) n1 = size(first)
        if (allocated(second)) n2 = size(second)
        allocate (dest(n1 + n2))
        if (n1 > 0) dest(1:n1) = first
        if (n2 > 0) dest(n1 + 1:n1 + n2) = second
    end subroutine concat_indices

    logical function find_rename_remote(node, local_name, remote_name) &
        result(found)
        type(use_statement_node), intent(in) :: node
        character(len=*), intent(in) :: local_name
        character(len=:), allocatable, intent(out) :: remote_name
        integer :: i

        found = .false.
        call set_empty(remote_name)
        if (.not. allocated(node%rename_list)) return
        i = 1
        do while (i + 1 <= size(node%rename_list))
            if (same_name(node%rename_list(i)%s, local_name)) then
                remote_name = trim(node%rename_list(i + 1)%s)
                found = .true.
                return
            end if
            i = i + 2
        end do
    end function find_rename_remote

    logical function is_renamed_remote(node, remote_name) result(renamed)
        type(use_statement_node), intent(in) :: node
        character(len=*), intent(in) :: remote_name
        integer :: i

        renamed = .false.
        if (.not. allocated(node%rename_list)) return
        i = 1
        do while (i + 1 <= size(node%rename_list))
            if (same_name(node%rename_list(i + 1)%s, remote_name)) then
                renamed = .true.
                return
            end if
            i = i + 2
        end do
    end function is_renamed_remote

    logical function list_contains(list, name) result(found)
        type(string_t), allocatable, intent(in) :: list(:)
        character(len=*), intent(in) :: name
        integer :: i

        found = .false.
        if (.not. allocated(list)) return
        do i = 1, size(list)
            if (same_name(list(i)%s, name)) then
                found = .true.
                return
            end if
        end do
    end function list_contains

    integer function find_module_index(arena, module_name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_name
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(node%name)) cycle
                if (.not. same_name(node%name, module_name)) cycle
                index = i
                return
            end select
        end do
    end function find_module_index

    logical function module_binding_is_public(arena, module_index, name) &
        result(is_public)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: module_index
        character(len=*), intent(in) :: name
        integer, allocatable :: indices(:)
        integer :: i
        logical :: default_public
        logical :: named_visibility
        logical :: named_public

        default_public = .true.
        named_visibility = .false.
        named_public = .false.
        call get_scope_statement_indices(arena, module_index, indices)
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (visibility_statement_node)
                call apply_visibility(node, name, default_public, named_public)
                if (visibility_names_name(node, name)) named_visibility = .true.
            end select
        end do
        if (named_visibility) then
            is_public = named_public
        else
            is_public = default_public
        end if
    end function module_binding_is_public

    subroutine apply_visibility(node, name, default_public, named_public)
        type(visibility_statement_node), intent(in) :: node
        character(len=*), intent(in) :: name
        logical, intent(inout) :: default_public
        logical, intent(inout) :: named_public
        integer :: i

        if (.not. node%has_list) then
            default_public = .not. node%is_private
            return
        end if
        if (.not. allocated(node%names)) return
        do i = 1, size(node%names)
            if (.not. same_name(node%names(i)%s, name)) cycle
            named_public = .not. node%is_private
        end do
    end subroutine apply_visibility

    logical function visibility_names_name(node, name) result(matches)
        type(visibility_statement_node), intent(in) :: node
        character(len=*), intent(in) :: name
        integer :: i

        matches = .false.
        if (.not. node%has_list) return
        if (.not. allocated(node%names)) return
        do i = 1, size(node%names)
            if (same_name(node%names(i)%s, name)) then
                matches = .true.
                return
            end if
        end do
    end function visibility_names_name

    integer function find_enclosing_scope(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        scope_index = 0
        current = node_index
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (is_scope_node(arena, current)) then
                scope_index = current
                return
            end if
            current = arena%entries(current)%parent_index
        end do
    end function find_enclosing_scope

    integer function find_host_scope(arena, scope_node_index) result(host_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_node_index
        integer :: current

        host_index = 0
        if (.not. arena%has_node_at(scope_node_index)) return
        current = arena%entries(scope_node_index)%parent_index
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (is_scope_node(arena, current)) then
                host_index = current
                return
            end if
            current = arena%entries(current)%parent_index
        end do
    end function find_host_scope

    logical function is_scope_node(arena, node_index) result(is_scope)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_scope = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            is_scope = .true.
        type is (module_node)
            is_scope = .true.
        type is (submodule_node)
            is_scope = .true.
        type is (function_def_node)
            is_scope = .true.
        type is (subroutine_def_node)
            is_scope = .true.
        type is (associate_node)
            is_scope = .true.
        type is (block_construct_node)
            is_scope = .true.
        type is (mixed_construct_container_node)
            ! A main program without a PROGRAM statement is still a scoping
            ! unit; its statements hang off the container (issue #2888).
            is_scope = .true.
        end select
    end function is_scope_node

    subroutine node_reference_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        call set_empty(error_msg)
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) name = node%name
        type is (call_or_subscript_node)
            if (allocated(node%name)) name = node%name
        type is (subroutine_call_node)
            if (allocated(node%name)) name = node%name
        class default
            error_msg = 'AST node does not carry a resolvable name'
        end select
    end subroutine node_reference_name

    subroutine make_binding(binding, name, node_index, scope_node_index, kind, &
                            association, declaration_entity_index)
        type(declaration_binding_t), intent(out) :: binding
        character(len=*), intent(in) :: name
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_node_index
        integer, intent(in) :: kind
        integer, intent(in) :: association
        integer, intent(in), optional :: declaration_entity_index

        call clear_binding(binding)
        binding%found = .true.
        binding%name = trim(name)
        binding%remote_name = trim(name)
        binding%declaration_node_index = node_index
        binding%declaration_entity_index = 1
        if (present(declaration_entity_index)) &
            binding%declaration_entity_index = declaration_entity_index
        binding%node_index = node_index
        binding%scope_node_index = scope_node_index
        binding%binding_kind = kind
        binding%association = association
    end subroutine make_binding

    subroutine clear_binding(binding)
        type(declaration_binding_t), intent(out) :: binding

        binding%found = .false.
        call set_empty(binding%name)
        call set_empty(binding%remote_name)
        call set_empty(binding%module_name)
        binding%declaration_node_index = 0
        binding%declaration_entity_index = 0
        binding%node_index = 0
        binding%scope_node_index = 0
        binding%binding_kind = BINDING_NONE
        binding%association = ASSOCIATION_NONE
    end subroutine clear_binding

    logical function same_name(lhs, rhs)
        character(len=*), intent(in) :: lhs
        character(len=*), intent(in) :: rhs

        same_name = lowercase_text(trim(lhs)) == lowercase_text(trim(rhs))
    end function same_name

    function lowercase_text(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len_trim(text)) :: lowered
        integer :: i
        integer :: code

        do i = 1, len(lowered)
            code = iachar(text(i:i))
            if (code >= iachar('A')) then
                if (code <= iachar('Z')) then
                    lowered(i:i) = achar(code + 32)
                else
                    lowered(i:i) = text(i:i)
                end if
            else
                lowered(i:i) = text(i:i)
            end if
        end do
    end function lowercase_text

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_resolution
