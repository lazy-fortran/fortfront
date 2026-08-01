module semantic_call_signature_scopes
    ! Structural scope resolution for procedure-call signature checking.
    !
    ! The arena parent links are only populated on some parser paths, so the
    ! call checkers cannot walk upwards from a call node to its scoping unit.
    ! This module instead descends the AST structurally: every scoping unit
    ! exposes its specification part through known index arrays, so a name can
    ! be classified against the scopes that enclose a statement.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node, module_node, submodule_node
    use ast_nodes_misc, only: interface_block_node, intrinsic_statement_node, &
        use_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: ENTITY_NONE, ENTITY_SUBROUTINE, ENTITY_FUNCTION, ENTITY_VARIABLE
    public :: ENTITY_EXTERNAL, ENTITY_INTRINSIC, ENTITY_PROCEDURE
    public :: MAX_SCOPE_DEPTH
    public :: name_entity_t
    public :: is_scoping_unit
    public :: scope_specification_indices
    public :: resolve_name_in_scopes
    public :: find_dummy_procedure_interface
    public :: declaration_names_entity
    public :: declares_a_procedure

    integer, parameter :: ENTITY_NONE = 0
    integer, parameter :: ENTITY_SUBROUTINE = 1
    integer, parameter :: ENTITY_FUNCTION = 2
    integer, parameter :: ENTITY_VARIABLE = 3
    integer, parameter :: ENTITY_EXTERNAL = 4
    integer, parameter :: ENTITY_INTRINSIC = 5
    integer, parameter :: ENTITY_PROCEDURE = 6

    integer, parameter :: MAX_SCOPE_DEPTH = 32

    ! What an accessible name denotes, with the arena index of the evidence.
    type :: name_entity_t
        integer :: kind = ENTITY_NONE
        integer :: def_index = 0 ! function_def_node / subroutine_def_node
        integer :: decl_index = 0 ! declaration_node
        logical :: from_interface_body = .false.
    end type name_entity_t

contains

    logical function is_scoping_unit(arena, index) result(is_scope)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index

        is_scope = .false.
        if (.not. arena%has_node_at(index)) return

        select type (node => arena%entries(index)%node)
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
        class default
            is_scope = .false.
        end select
    end function is_scoping_unit

    ! Arena indices that make up the body plus specification part of a scoping
    ! unit, in source order.
    subroutine scope_specification_indices(arena, scope_index, indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        integer, allocatable, intent(out) :: indices(:)

        allocate (indices(0))
        if (.not. arena%has_node_at(scope_index)) return

        select type (node => arena%entries(scope_index)%node)
            type is (program_node)
            if (allocated(node%body_indices)) indices = node%body_indices
            type is (function_def_node)
            if (allocated(node%body_indices)) indices = node%body_indices
            type is (subroutine_def_node)
            if (allocated(node%body_indices)) indices = node%body_indices
            type is (module_node)
            indices = joined(node%declaration_indices, node%procedure_indices)
            type is (submodule_node)
            indices = joined(node%declaration_indices, node%procedure_indices)
        class default
            return
        end select
    end subroutine scope_specification_indices

    function joined(first, second) result(all_indices)
        integer, allocatable, intent(in) :: first(:)
        integer, allocatable, intent(in) :: second(:)
        integer, allocatable :: all_indices(:)

        allocate (all_indices(0))
        if (allocated(first)) all_indices = [all_indices, first]
        if (allocated(second)) all_indices = [all_indices, second]
    end function joined

    ! Classify a name against the open scopes, innermost first.
    function resolve_name_in_scopes(arena, scope_indices, depth, name) &
            result(entity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_indices(:)
        integer, intent(in) :: depth
        character(len=*), intent(in) :: name
        type(name_entity_t) :: entity

        character(len=:), allocatable :: lowered
        integer :: d

        entity = name_entity_t()
        lowered = to_lower(trim(name))
        if (len_trim(lowered) == 0) return

        do d = depth, 1, -1
            entity = resolve_name_in_scope(arena, scope_indices(d), lowered)
            if (entity%kind /= ENTITY_NONE) return
        end do
    end function resolve_name_in_scopes

    function resolve_name_in_scope(arena, scope_index, lowered) result(entity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: lowered
        type(name_entity_t) :: entity

        integer, allocatable :: indices(:)

        entity = name_entity_t()
        call scope_specification_indices(arena, scope_index, indices)
        entity = resolve_name_in_indices(arena, indices, lowered)
        if (entity%kind /= ENTITY_NONE) return
        entity = resolve_name_in_used_modules(arena, indices, lowered)
    end function resolve_name_in_scope

    ! Procedure evidence wins over data evidence inside one scoping unit, so a
    ! subroutine that also carries a stale declaration is never misclassified.
    recursive function resolve_name_in_indices(arena, indices, lowered) &
            result(entity)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        character(len=*), intent(in) :: lowered
        type(name_entity_t) :: entity

        type(name_entity_t) :: weak
        integer :: i
        integer :: j

        entity = name_entity_t()
        weak = name_entity_t()
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (child => arena%entries(indices(i))%node)
                type is (subroutine_def_node)
                if (names_match(child%name, lowered)) then
                    entity%kind = ENTITY_SUBROUTINE
                    entity%def_index = indices(i)
                    return
                end if
                type is (interface_block_node)
                entity = resolve_name_in_indices(arena, child%procedure_indices, &
                    lowered)
                if (entity%kind /= ENTITY_NONE) then
                    entity%from_interface_body = .true.
                    return
                end if
                type is (function_def_node)
                if (names_match(child%name, lowered)) then
                    weak%kind = ENTITY_FUNCTION
                    weak%def_index = indices(i)
                end if
                type is (intrinsic_statement_node)
                if (.not. allocated(child%procedure_names)) cycle
                do j = 1, size(child%procedure_names)
                    if (to_lower(trim(child%procedure_names(j)%s)) == lowered) then
                        entity%kind = ENTITY_INTRINSIC
                        return
                    end if
                end do
                type is (declaration_node)
                if (.not. declaration_names_entity(child, lowered)) cycle
                if (child%is_external) then
                    weak%kind = ENTITY_EXTERNAL
                    weak%decl_index = indices(i)
                else if (declares_a_procedure(child)) then
                    weak%kind = ENTITY_PROCEDURE
                    weak%decl_index = indices(i)
                else if (child%is_pointer) then
                    weak%kind = ENTITY_PROCEDURE
                    weak%decl_index = indices(i)
                else
                    weak%kind = ENTITY_VARIABLE
                    weak%decl_index = indices(i)
                end if
            class default
                cycle
            end select
        end do

        entity = weak
    end function resolve_name_in_indices

    ! Names made accessible by a USE of a module defined in the same arena.
    function resolve_name_in_used_modules(arena, indices, lowered) result(entity)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        character(len=*), intent(in) :: lowered
        type(name_entity_t) :: entity

        integer :: i
        integer :: m
        integer, allocatable :: module_indices(:)

        entity = name_entity_t()
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (child => arena%entries(indices(i))%node)
                type is (use_statement_node)
                if (.not. allocated(child%module_name)) cycle
                do m = 1, arena%size
                    if (.not. arena%has_node_at(m)) cycle
                    select type (candidate => arena%entries(m)%node)
                        type is (module_node)
                        if (.not. names_match(candidate%name, &
                            to_lower(trim(child%module_name)))) cycle
                        module_indices = joined(candidate%declaration_indices, &
                            candidate%procedure_indices)
                        entity = resolve_name_in_indices(arena, module_indices, &
                            lowered)
                        if (entity%kind /= ENTITY_NONE) return
                    class default
                        cycle
                    end select
                end do
            class default
                cycle
            end select
        end do
    end function resolve_name_in_used_modules

    ! Explicit interface of a dummy procedure of `def_index`: an interface body
    ! inside the procedure's specification part that declares the dummy name.
    integer function find_dummy_procedure_interface(arena, def_index, dummy_name) &
            result(iface_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: def_index
        character(len=*), intent(in) :: dummy_name

        integer, allocatable :: indices(:)
        type(name_entity_t) :: entity
        integer :: i

        iface_index = 0
        call scope_specification_indices(arena, def_index, indices)
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (child => arena%entries(indices(i))%node)
                type is (interface_block_node)
                entity = resolve_name_in_indices(arena, child%procedure_indices, &
                    to_lower(trim(dummy_name)))
                if (entity%kind == ENTITY_SUBROUTINE .or. &
                    entity%kind == ENTITY_FUNCTION) then
                    iface_index = entity%def_index
                    return
                end if
            class default
                cycle
            end select
        end do
    end function find_dummy_procedure_interface

    logical function declaration_names_entity(decl, lowered) result(matches)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: lowered

        integer :: j

        matches = .false.
        if (decl%is_multi_declaration) then
            if (.not. allocated(decl%var_names)) return
            do j = 1, size(decl%var_names)
                if (to_lower(trim(decl%var_names(j))) == lowered) then
                    matches = .true.
                    return
                end if
            end do
        else
            matches = names_match(decl%var_name, lowered)
        end if
    end function declaration_names_entity

    ! `procedure(iface) :: f` declares a procedure, not a typed data object.
    logical function declares_a_procedure(decl) result(is_proc)
        type(declaration_node), intent(in) :: decl

        is_proc = .false.
        if (.not. allocated(decl%type_name)) return
        if (len(decl%type_name) < 9) return
        is_proc = to_lower(decl%type_name(1:9)) == 'procedure'
    end function declares_a_procedure

    logical function names_match(node_name, lowered) result(matches)
        character(len=:), allocatable, intent(in) :: node_name
        character(len=*), intent(in) :: lowered

        matches = .false.
        if (.not. allocated(node_name)) return
        if (len_trim(node_name) == 0) return
        matches = to_lower(trim(node_name)) == lowered
    end function names_match

end module semantic_call_signature_scopes
