module codegen_program_decl_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node
    use fortfront_constants, only: MAX_PROGRAM_VARIABLES
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: TFUN, type_args_allocated, type_args_size, &
                                   type_args_element
    implicit none
    private
    public :: exists_in_list, build_function_return_type_table
    public :: initialize_program_decl_state
    public :: program_decl_state_t, program_decl_max_vars
    public :: record_declared_name, record_namelist_group
    public :: record_use_associated_name, record_use_module_name
    public :: seed_namelist_groups_from_text

    integer, parameter :: program_decl_max_vars = MAX_PROGRAM_VARIABLES

    type :: program_decl_state_t
        character(len=64) :: declared_names(program_decl_max_vars)
        character(len=64) :: var_names(program_decl_max_vars)
        character(len=64) :: var_types(program_decl_max_vars)
        character(len=64) :: func_names(program_decl_max_vars)
        character(len=64) :: func_types(program_decl_max_vars)
        character(len=64) :: internal_funcs(program_decl_max_vars)
        character(len=64) :: defined_func_names(program_decl_max_vars)
        character(len=64) :: defined_func_types(program_decl_max_vars)
        character(len=64) :: use_associated_names(program_decl_max_vars)
        character(len=64) :: use_module_names(program_decl_max_vars)
        character(len=64) :: namelist_group_names(program_decl_max_vars)
        integer :: declared_count
        integer :: var_count
        integer :: func_count
        integer :: internal_count
        integer :: defined_func_count
        integer :: use_associated_count
        integer :: use_module_count
        integer :: namelist_group_count
    end type program_decl_state_t

contains

    subroutine initialize_program_decl_state(state)
        type(program_decl_state_t), intent(out) :: state

        state%declared_names = ""
        state%var_names = ""
        state%var_types = ""
        state%func_names = ""
        state%func_types = ""
        state%internal_funcs = ""
        state%defined_func_names = ""
        state%defined_func_types = ""
        state%use_associated_names = ""
        state%use_module_names = ""
        state%namelist_group_names = ""
        state%declared_count = 0
        state%var_count = 0
        state%func_count = 0
        state%internal_count = 0
        state%defined_func_count = 0
        state%use_associated_count = 0
        state%use_module_count = 0
        state%namelist_group_count = 0
    end subroutine initialize_program_decl_state

    pure function normalize_declared_identifier(raw_name) result(normalized)
        character(len=*), intent(in) :: raw_name
        character(len=64) :: normalized
        integer :: paren_pos

        normalized = trim(to_lower(raw_name))
        paren_pos = index(normalized, '(')
        if (paren_pos > 0) then
            if (paren_pos == 1) then
                normalized = ''
            else
                normalized = trim(normalized(:paren_pos - 1))
            end if
        end if
    end function normalize_declared_identifier

    logical function exists_in_list(list, count, name)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i
        character(len=64) :: normalized_target
        character(len=64) :: normalized_entry

        exists_in_list = .false.
        normalized_target = trim(to_lower(name))
        do i = 1, count
            normalized_entry = trim(to_lower(list(i)))
            if (trim(normalized_entry) == trim(normalized_target)) then
                exists_in_list = .true.
                return
            end if
        end do
    end function exists_in_list

    subroutine build_function_return_type_table(arena, func_names, func_types, &
                                                count)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(inout) :: func_names(:)
        character(len=*), intent(inout) :: func_types(:)
        integer, intent(out) :: count
        integer :: i
        character(len=64) :: func_name
        character(len=:), allocatable :: inferred_return_type

        count = 0
        func_names = ""
        func_types = ""

        do i = 1, arena%size
            if (count >= size(func_names)) exit
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(func%name)) cycle
                func_name = trim(func%name)
                if (len_trim(func_name) == 0) cycle
                if (exists_in_list(func_names, count, func_name)) cycle
                ! Skip functions inside interface blocks - they don't have bodies
                ! Functions in interface blocks are just declarations, not definitions
                if (.not. allocated(func%body_indices)) cycle
                count = count + 1
                func_names(count) = trim(to_lower(func_name))
                ! Use explicit return_type if present (standard Fortran)
                if (allocated(func%return_type)) then
                    if (len_trim(func%return_type) > 0) then
                        func_types(count) = trim(func%return_type)
                        cycle
                    end if
                end if
                ! Fall back to inferred_type for lazy Fortran (Issue #2075)
                if (func%inferred_type%kind == TFUN .and. &
                    type_args_allocated(func%inferred_type) .and. &
                    type_args_size(func%inferred_type) >= 2) then
                    inferred_return_type = mono_type_to_string( &
                                           type_args_element(func%inferred_type, 2), &
                                           include_shape=.true., &
                                           standardize_real=.false., &
                                           fallback='')
                    if (len_trim(inferred_return_type) > 0) then
                        func_types(count) = trim(inferred_return_type)
                    end if
                end if
            end select
        end do
    end subroutine build_function_return_type_table

    subroutine record_declared_name(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=64) :: normalized_name

        normalized_name = normalize_declared_identifier(name)
        if (len_trim(normalized_name) == 0) return
        if (state%declared_count >= program_decl_max_vars) return
        if (exists_in_list(state%declared_names, state%declared_count, &
                           normalized_name)) return
        state%declared_count = state%declared_count + 1
        state%declared_names(state%declared_count) = normalized_name
    end subroutine record_declared_name

    subroutine record_namelist_group(state, group_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: group_name
        character(len=64) :: normalized_name

        normalized_name = trim(to_lower(group_name))
        if (len_trim(normalized_name) == 0) return
        if (state%namelist_group_count >= program_decl_max_vars) return
        if (exists_in_list(state%namelist_group_names, &
                           state%namelist_group_count, normalized_name)) return
        state%namelist_group_count = state%namelist_group_count + 1
        state%namelist_group_names(state%namelist_group_count) = &
            normalized_name
    end subroutine record_namelist_group

    subroutine record_use_associated_name(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=64) :: normalized_name

        normalized_name = trim(to_lower(name))
        if (len_trim(normalized_name) == 0) return
        if (state%use_associated_count >= program_decl_max_vars) return
        if (exists_in_list(state%use_associated_names, &
                           state%use_associated_count, normalized_name)) return
        state%use_associated_count = state%use_associated_count + 1
        state%use_associated_names(state%use_associated_count) = normalized_name
    end subroutine record_use_associated_name

    subroutine record_use_module_name(state, module_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: module_name
        character(len=64) :: normalized_name

        normalized_name = trim(to_lower(module_name))
        if (len_trim(normalized_name) == 0) return
        if (state%use_module_count >= program_decl_max_vars) return
        if (exists_in_list(state%use_module_names, &
                           state%use_module_count, normalized_name)) return
        state%use_module_count = state%use_module_count + 1
        state%use_module_names(state%use_module_count) = normalized_name
    end subroutine record_use_module_name

    subroutine seed_namelist_groups_from_text(state, header_code)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: header_code
        integer :: start_pos, newline_pos, code_len
        character(len=:), allocatable :: line

        if (len(header_code) == 0) return
        code_len = len(header_code)
        start_pos = 1

        do
            newline_pos = index(header_code(start_pos:), new_line('A'))
            if (newline_pos == 0) then
                line = header_code(start_pos:)
                call analyze_namelist_line(state, line)
                exit
            else
                line = header_code(start_pos:start_pos + newline_pos - 2)
                call analyze_namelist_line(state, line)
                start_pos = start_pos + newline_pos
                if (start_pos > code_len) exit
            end if
        end do
    contains
        subroutine analyze_namelist_line(state, raw_line)
            type(program_decl_state_t), intent(inout) :: state
            character(len=*), intent(in) :: raw_line
            character(len=:), allocatable :: trimmed
            character(len=:), allocatable :: lowered
            character(len=:), allocatable :: group_name
            integer :: comment_pos, slash_start, slash_end
            integer :: label_pos
            character(len=1) :: ch

            trimmed = adjustl(raw_line)
            if (len_trim(trimmed) == 0) return

            comment_pos = index(trimmed, '!')
            if (comment_pos == 1) return
            if (comment_pos > 1) then
                trimmed = trimmed(:comment_pos - 1)
            end if
            trimmed = adjustl(trimmed)
            if (len_trim(trimmed) == 0) return

            label_pos = 1
            do while (label_pos <= len_trim(trimmed))
                ch = trimmed(label_pos:label_pos)
                if (ch < '0' .or. ch > '9') exit
                label_pos = label_pos + 1
            end do
            if (label_pos > 1) then
                trimmed = adjustl(trimmed(label_pos:))
            end if
            if (len_trim(trimmed) == 0) return

            lowered = to_lower(trimmed)
            if (index(lowered, 'namelist') /= 1) return

            slash_start = index(trimmed, '/')
            if (slash_start <= 0) return
            slash_end = index(trimmed(slash_start + 1:), '/')
            if (slash_end <= 0) return
            slash_end = slash_start + slash_end
            if (slash_end <= slash_start + 1) return

            group_name = trimmed(slash_start + 1:slash_end - 1)
            group_name = adjustl(group_name)
            group_name = trim(group_name)
            if (len_trim(group_name) == 0) return

            call record_namelist_group(state, group_name)
        end subroutine analyze_namelist_line
    end subroutine seed_namelist_groups_from_text

end module codegen_program_decl_utils
