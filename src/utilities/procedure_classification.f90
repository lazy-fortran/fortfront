module procedure_classification
use ast_arena_modern, only: ast_arena_t
use ast_nodes_core, only: program_node
use ast_nodes_data, only: declaration_node, parameter_declaration_node
use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
    & get_procedure_params, get_procedure_name
use ast_nodes_transfer, only: entry_node
use string_utils_mod, only: to_lower
implicit none
private

public :: should_hoist_procedure
public :: procedure_has_explicit_types
public :: procedure_has_entry_statement

contains

    logical function should_hoist_procedure(arena, proc_idx, target_prog_idx) &
            & result(should_hoist)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        integer, intent(in) :: target_prog_idx

        should_hoist = .false.
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        ! ENTRY statements cannot appear in contained procedures (ISO/IEC
        ! 1539-1:2018 Section 15.6.2.6). Procedures with ENTRY must remain
        ! external, not hoisted into a programs contains block.
        if (procedure_has_entry_statement(arena, proc_idx)) then
            should_hoist = .false.
            return
        end if

        if (procedure_has_optional_arguments(arena, proc_idx)) then
            should_hoist = .true.
            return
        end if

        if (procedure_has_special_prefix(arena, proc_idx)) then
            should_hoist = .true.
            return
        end if

        if (procedure_declared_external(arena, proc_idx, target_prog_idx)) then
            should_hoist = .false.
            return
        end if

    end function should_hoist_procedure

    logical function procedure_has_optional_arguments(arena, proc_idx) &
            & result(has_optional)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        integer, allocatable :: param_indices(:)
        integer :: i, param_idx

        has_optional = .false.
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        param_indices = get_procedure_params(arena%entries(proc_idx)%node)
        if (.not. allocated(param_indices)) return

        do i = 1, size(param_indices)
            param_idx = param_indices(i)
            if (param_idx <= 0) cycle
            if (param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle
            select type (param_node => arena%entries(param_idx)%node)
                type is (parameter_declaration_node)
                if (param_node%is_optional) then
                    has_optional = .true.
                    return
                end if
                type is (declaration_node)
                if (param_node%is_optional) then
                    has_optional = .true.
                    return
                end if
            end select
        end do

        select type (proc_node => arena%entries(proc_idx)%node)
            type is (function_def_node)
            if (allocated(proc_node%body_indices)) then
                if (body_has_optional_declaration(arena, proc_node%body_indices)) then
                    has_optional = .true.
                end if
            end if
            type is (subroutine_def_node)
            if (allocated(proc_node%body_indices)) then
                if (body_has_optional_declaration(arena, proc_node%body_indices)) then
                    has_optional = .true.
                end if
            end if
        class default
        end select
    end function procedure_has_optional_arguments

    logical function procedure_has_special_prefix(arena, proc_idx) &
            & result(has_prefix)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        integer :: i
        character(len=:), allocatable :: keyword

        has_prefix = .false.
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        select type (proc_node => arena%entries(proc_idx)%node)
            type is (function_def_node)
            if (.not. allocated(proc_node%prefix_keywords)) return
            do i = 1, size(proc_node%prefix_keywords)
                keyword = to_lower(trim(proc_node%prefix_keywords(i)))
                select case (keyword)
                case ('pure', 'elemental')
                    has_prefix = .true.
                    return
                end select
            end do
            type is (subroutine_def_node)
            if (allocated(proc_node%prefix_keywords)) then
                do i = 1, size(proc_node%prefix_keywords)
                    keyword = to_lower(trim(proc_node%prefix_keywords(i)))
                    select case (keyword)
                    case ('pure', 'elemental')
                        has_prefix = .true.
                        return
                    end select
                end do
            end if
            if (allocated(proc_node%body_indices)) then
                if (body_has_special_prefix(arena, proc_node%body_indices)) then
                    has_prefix = .true.
                end if
            end if
        class default
        end select
    end function procedure_has_special_prefix

    logical function body_has_special_prefix(arena, body_indices) &
            & result(has_prefix)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        has_prefix = .false.

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (node => arena%entries(idx)%node)
                type is (function_def_node)
                if (procedure_has_special_prefix(arena, idx)) then
                    has_prefix = .true.
                    return
                end if
                type is (subroutine_def_node)
                if (procedure_has_special_prefix(arena, idx)) then
                    has_prefix = .true.
                    return
                end if
            class default
            end select
        end do
    end function body_has_special_prefix

    logical function body_has_optional_declaration(arena, body_indices) &
            & result(has_optional)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        has_optional = .false.

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
                type is (declaration_node)
                if (decl%is_optional) then
                    has_optional = .true.
                    return
                end if
            end select
        end do
    end function body_has_optional_declaration

    logical function procedure_has_explicit_types(arena, proc_idx) &
            result(has_types)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        type(function_def_node), pointer :: func
        type(subroutine_def_node), pointer :: subr
        type(parameter_declaration_node), pointer :: param
        integer, allocatable :: param_indices(:)
        integer :: i

        has_types = .false.
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        select type (node => arena%entries(proc_idx)%node)
            type is (function_def_node)
            func => node
            if (.not. allocated(func%param_indices)) return
            if (size(func%param_indices) == 0) return
            param_indices = func%param_indices
            type is (subroutine_def_node)
            subr => node
            if (.not. allocated(subr%param_indices)) return
            if (size(subr%param_indices) == 0) return
            param_indices = subr%param_indices
        class default
            return
        end select

        do i = 1, size(param_indices)
            call get_parameter_node_from_arena(param_indices(i), param)
            if (.not. associated(param)) return
            if (.not. allocated(param%type_name)) return
            if (len_trim(param%type_name) == 0) return
        end do

        has_types = .true.
    contains
        subroutine get_parameter_node_from_arena(idx, param_node)
            integer, intent(in) :: idx
            type(parameter_declaration_node), pointer :: param_node

            nullify (param_node)
            if (idx <= 0) return
            if (idx > arena%size) return
            if (.not. allocated(arena%entries(idx)%node)) return
            select type (decl => arena%entries(idx)%node)
                type is (parameter_declaration_node)
                param_node => decl
            end select
        end subroutine get_parameter_node_from_arena
    end function procedure_has_explicit_types

    logical function procedure_declared_external(arena, proc_idx, target_prog_idx) &
            result(is_external_proc)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        integer, intent(in) :: target_prog_idx
        character(len=:), allocatable :: proc_name

        is_external_proc = .false.
        if (target_prog_idx <= 0) return
        if (target_prog_idx > arena%size) return
        if (.not. allocated(arena%entries(target_prog_idx)%node)) return
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        proc_name = get_procedure_name(arena%entries(proc_idx)%node)
        if (.not. allocated(proc_name)) return
        proc_name = trim(to_lower(proc_name))
        if (len(proc_name) == 0) return

        select type (target => arena%entries(target_prog_idx)%node)
            type is (program_node)
            if (.not. allocated(target%body_indices)) return
            if (body_declares_external_with_name(arena, target%body_indices, &
                proc_name)) then
                is_external_proc = .true.
            end if
        class default
        end select
    end function procedure_declared_external

    logical function body_declares_external_with_name(arena, body_indices, name) &
            & result(has_match)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        integer :: i, idx, j
        character(len=:), allocatable :: candidate

        has_match = .false.

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
                type is (declaration_node)
                if (.not. decl%is_external) cycle
                if (allocated(decl%var_name)) then
                    candidate = trim(to_lower(decl%var_name))
                    if (candidate == name) then
                        has_match = .true.
                        return
                    end if
                end if
                if (decl%is_multi_declaration) then
                    if (allocated(decl%var_names)) then
                        do j = 1, size(decl%var_names)
                            candidate = trim(to_lower(decl%var_names(j)))
                            if (candidate == name) then
                                has_match = .true.
                                return
                            end if
                        end do
                    end if
                end if
            end select
        end do
    end function body_declares_external_with_name

    logical function procedure_has_entry_statement(arena, proc_idx) &
            & result(has_entry)
        ! Check if a procedure contains ENTRY statements. ENTRY statements cannot
        ! appear in contained (internal) procedures per ISO/IEC 1539-1:2018
        ! Section 15.6.2.6. This function is used to prevent hoisting procedures
        ! with ENTRY statements into a programs contains block.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        integer :: i, idx

        has_entry = .false.
        if (proc_idx <= 0) return
        if (proc_idx > arena%size) return
        if (.not. allocated(arena%entries(proc_idx)%node)) return

        select type (proc_node => arena%entries(proc_idx)%node)
            type is (function_def_node)
            if (.not. allocated(proc_node%body_indices)) return
            has_entry = body_has_entry_statement(arena, proc_node%body_indices)
            type is (subroutine_def_node)
            if (.not. allocated(proc_node%body_indices)) return
            has_entry = body_has_entry_statement(arena, proc_node%body_indices)
        class default
            return
        end select
    end function procedure_has_entry_statement

    logical function body_has_entry_statement(arena, body_indices) &
            & result(has_entry)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        has_entry = .false.
        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
                type is (entry_node)
                has_entry = .true.
                return
            end select
        end do
    end function body_has_entry_statement

end module procedure_classification
