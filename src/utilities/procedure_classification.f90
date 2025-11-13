module procedure_classification
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        & get_procedure_params, get_procedure_name
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: should_hoist_procedure

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

        if (procedure_has_optional_arguments(arena, proc_idx)) then
            should_hoist = .true.
            return
        end if

        if (procedure_has_special_prefix(arena, proc_idx)) then
            should_hoist = .true.
            return
        end if

        should_hoist = .true.
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

end module procedure_classification
