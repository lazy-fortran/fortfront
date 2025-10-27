module standardizer_subprograms
    ! Function/subroutine standardization module
    ! Handles function and subroutine transformations, wrapping, and parameter processing

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_data
    use ast_nodes_misc
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node
    use ast_factory
    use type_system_unified
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use lexer_core, only: to_lower
    use type_string_utils, only: is_character_type_string
    use semantic_validation_utils, only: rename_identifier_in_arena
    implicit none
    private

    ! Type standardization configuration (local copy)
    ! DISABLED: Converting real -> real(8) breaks generic interfaces that
    ! depend on exact type matching. Users should explicitly use real(8) or
    ! kind parameters if they want double precision.
    logical, save :: standardizer_type_standardization_enabled = .false.

    public :: standardize_subprograms
    public :: standardize_function_def
    public :: standardize_subroutine_def
    public :: standardize_function_parameters
    public :: standardize_subroutine_parameters
    public :: wrap_function_in_program
    public :: wrap_subroutine_in_program
    public :: infer_parameter_type
    public :: standardize_function_result

    type :: param_metadata_t
        character(len=:), allocatable :: names(:)
        integer, allocatable :: found(:)
        logical, allocatable :: optional(:)
        character(len=:), allocatable :: intent(:)
        character(len=:), allocatable :: type_name(:)
        logical, allocatable :: has_kind(:)
        integer, allocatable :: kind_value(:)
        logical, allocatable :: is_array(:)
        logical, allocatable :: is_allocatable(:)
        logical, allocatable :: type_inferred(:)
        integer, allocatable :: rank(:)
    end type param_metadata_t

contains

    ! Local implementation of get_standardizer_type_standardization
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    pure logical function is_type_variable_str(type_name) result(is_variable)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: trimmed
        integer :: effective_length

        trimmed = trim(type_name)
        effective_length = len_trim(trimmed)
        if (effective_length == 0) then
            is_variable = .true.
        else if (effective_length == len("type_variable") .and. &
                 trim(trimmed) == "type_variable") then
            is_variable = .true.
        else
            is_variable = trimmed(1:1) == "'"
        end if
    end function is_type_variable_str

    ! Shared helper to synchronize parameter declarations in function/subroutine bodies
    subroutine synchronize_parameter_declarations(arena, body_indices, metadata, &
                                                  default_intent, standardize_types)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        logical, intent(in) :: standardize_types
        integer :: i
        integer :: n_params

        if (.not. allocated(metadata%names)) return
        n_params = size(metadata%names)
        if (n_params == 0) return
        if (size(body_indices) == 0) return

        do i = 1, size(body_indices)
            call sync_process_body_index(arena, body_indices(i), metadata, &
                                         default_intent, standardize_types)
        end do
    end subroutine synchronize_parameter_declarations

    subroutine sync_process_body_index(arena, body_index, metadata, default_intent, &
                                       standardize_types)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_index
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        logical, intent(in) :: standardize_types
        type(declaration_node) :: stmt
        character(len=16) :: stmt_intent

        if (body_index <= 0) return
        if (body_index > arena%size) return
        if (.not. allocated(arena%entries(body_index)%node)) return

        select type (node => arena%entries(body_index)%node)
        type is (declaration_node)
            stmt = node
            if (stmt%has_intent .and. allocated(stmt%intent)) then
                stmt_intent = stmt%intent
            else
                stmt_intent = ""
            end if
            call sync_handle_declaration(stmt, body_index, metadata, default_intent, &
                                         stmt_intent, standardize_types)
            arena%entries(body_index)%node = stmt
        end select
    end subroutine sync_process_body_index

    subroutine sync_handle_declaration(stmt, decl_index, metadata, default_intent, &
                                       stmt_intent, standardize_types)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: decl_index
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        character(len=*), intent(inout) :: stmt_intent
        logical, intent(in) :: standardize_types
        integer :: param_idx

        if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
            call sync_handle_multi_declaration(stmt, decl_index, metadata, &
                                               default_intent, stmt_intent, &
                                               standardize_types)
            return
        end if

        param_idx = metadata_find_param(metadata, stmt%var_name)
        if (param_idx <= 0) return

        call sync_process_single_declaration(stmt, decl_index, param_idx, metadata, &
                                             default_intent, stmt_intent)
        call sync_infer_type_if_variable(stmt, metadata%names(param_idx))
        call apply_type_standardization_to_stmt(stmt, standardize_types)
        call sync_finalize_intent(stmt, stmt_intent, default_intent)
    end subroutine sync_handle_declaration

    subroutine sync_handle_multi_declaration(stmt, decl_index, metadata, &
                                             default_intent, stmt_intent, &
                                             standardize_types)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: decl_index
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        character(len=*), intent(inout) :: stmt_intent
        logical, intent(in) :: standardize_types
        integer :: name_idx
        integer :: param_idx
        logical :: matched

        matched = .false.
        do name_idx = 1, size(stmt%var_names)
            param_idx = metadata_find_param(metadata, stmt%var_names(name_idx))
            if (param_idx <= 0) cycle
            call sync_process_single_declaration(stmt, decl_index, param_idx, &
                                                 metadata, &
                                                 default_intent, stmt_intent)
            matched = .true.
        end do

        if (.not. matched) return
        call apply_type_standardization_to_stmt(stmt, standardize_types)
        call sync_finalize_intent(stmt, stmt_intent, default_intent)
    end subroutine sync_handle_multi_declaration

    subroutine sync_process_single_declaration(stmt, decl_index, param_idx, metadata, &
                                               default_intent, stmt_intent)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: decl_index
        integer, intent(in) :: param_idx
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        character(len=*), intent(inout) :: stmt_intent

        call sync_update_intent(metadata%intent(param_idx), stmt_intent, &
                                default_intent)
        if (metadata%optional(param_idx)) stmt%is_optional = .true.
        metadata%found(param_idx) = decl_index

        if (allocated(stmt%type_name)) then
            if (.not. is_type_variable_str(stmt%type_name)) then
                metadata%type_name(param_idx) = trim(stmt%type_name)
                metadata%type_inferred(param_idx) = .false.
            end if
        end if

        metadata%has_kind(param_idx) = stmt%has_kind
        metadata%kind_value(param_idx) = stmt%kind_value
        call sync_update_array_info(stmt, metadata, param_idx)
        metadata%is_allocatable(param_idx) = stmt%is_allocatable
    end subroutine sync_process_single_declaration

    subroutine sync_update_array_info(stmt, metadata, param_idx)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: param_idx
        integer :: rank_size

        if (metadata%is_array(param_idx)) then
            if (metadata%rank(param_idx) <= 0) then
                if (stmt%is_array .and. allocated(stmt%dimension_indices)) then
                    metadata%rank(param_idx) = size(stmt%dimension_indices)
                else
                    metadata%rank(param_idx) = 1
                end if
            end if
            call ensure_deferred_shape(stmt, metadata%rank(param_idx))
        else
            metadata%is_array(param_idx) = stmt%is_array
            if (metadata%rank(param_idx) <= 0) then
                if (stmt%is_array .and. allocated(stmt%dimension_indices)) then
                    rank_size = size(stmt%dimension_indices)
                    metadata%rank(param_idx) = rank_size
                end if
            end if
        end if
    end subroutine sync_update_array_info

    subroutine sync_update_intent(target_intent, stmt_intent, default_intent)
        character(len=*), intent(inout) :: target_intent
        character(len=*), intent(inout) :: stmt_intent
        character(len=*), intent(in) :: default_intent

        if (len_trim(stmt_intent) == 0) then
            if (len_trim(target_intent) > 0) then
                stmt_intent = trim(target_intent)
            else
                stmt_intent = trim(default_intent)
                target_intent = trim(default_intent)
            end if
        else
            if (len_trim(target_intent) == 0) target_intent = trim(stmt_intent)
        end if
    end subroutine sync_update_intent

    subroutine sync_infer_type_if_variable(stmt, param_name)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        character(len=*), intent(in) :: param_name
        character(len=32) :: inferred_type
        logical :: has_kind_local
        integer :: kind_value_local

        if (.not. allocated(stmt%type_name)) return
        if (trim(stmt%type_name) /= "type_variable") return

        call infer_parameter_type(param_name, inferred_type, has_kind_local, &
                                  kind_value_local)
        stmt%type_name = trim(inferred_type)
        if (has_kind_local) then
            stmt%has_kind = .true.
            stmt%kind_value = kind_value_local
        end if
    end subroutine sync_infer_type_if_variable

    subroutine apply_type_standardization_to_stmt(stmt, standardize_types)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        logical, intent(in) :: standardize_types

        if (.not. standardize_types) return
        if (.not. allocated(stmt%type_name)) return
        if (trim(stmt%type_name) /= "real") return

        stmt%type_name = "real"
        stmt%has_kind = .true.
        stmt%kind_value = 8
    end subroutine apply_type_standardization_to_stmt

    subroutine sync_finalize_intent(stmt, stmt_intent, default_intent)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        character(len=*), intent(inout) :: stmt_intent
        character(len=*), intent(in) :: default_intent
        character(len=16) :: final_intent

        final_intent = trim(stmt_intent)
        if (len_trim(final_intent) == 0) final_intent = trim(default_intent)
        if (len_trim(final_intent) == 0) return

        stmt%intent = final_intent
        stmt%has_intent = .true.
    end subroutine sync_finalize_intent

    integer function metadata_find_param(metadata, name) result(index)
        type(param_metadata_t), intent(in) :: metadata
        character(len=*), intent(in) :: name
        integer :: k

        index = 0
        if (.not. allocated(metadata%names)) return
        do k = 1, size(metadata%names)
            if (trim(metadata%names(k)) == trim(name)) then
                index = k
                return
            end if
        end do
    end function metadata_find_param

    subroutine ensure_deferred_shape(stmt, desired_rank)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: desired_rank
        integer :: rank_size

        if (desired_rank <= 0) return
        stmt%is_array = .true.
        if (allocated(stmt%dimension_indices)) then
            rank_size = size(stmt%dimension_indices)
            if (rank_size == desired_rank) return
            deallocate (stmt%dimension_indices)
        end if
        allocate (stmt%dimension_indices(desired_rank))
        stmt%dimension_indices = 0
    end subroutine ensure_deferred_shape

    ! Standardize function and subroutine definitions
    subroutine standardize_subprograms(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        call standardize_function_def(arena, stmt, &
                                                      prog%body_indices(i))
                    type is (subroutine_def_node)
                        call standardize_subroutine_def(arena, stmt, &
                                                        prog%body_indices(i))
                    end select
                end if
            end if
        end do
    end subroutine standardize_subprograms

    ! Standardize a function definition
    subroutine standardize_function_def(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j
        character(len=:), allocatable :: return_type_str
        logical :: standardizer_type_standardization_enabled

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)

        ! Standardize return type
        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "real") then
                if (standardizer_type_standardization_enabled) then
                    func_def%return_type = "real(8)"
                else
                    func_def%return_type = "real"
                end if
            end if
        else
            ! Function return type should be explicitly declared
            ! No default assumption - let it remain unspecified
        end if

        ! Add implicit none at the beginning of function body
        if (allocated(func_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=func_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(func_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(func_def%body_indices)
                new_body_indices(i + 1) = func_def%body_indices(i)
            end do
            func_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_function_parameters(arena, func_def, func_index)

        ! Ensure function result variable is standardized and declared
        call standardize_function_result(arena, func_def, func_index)

        ! Update the arena entry
        arena%entries(func_index)%node = func_def
    end subroutine standardize_function_def

    ! Ensure a function has a proper result(...) clause and a declared result variable
    subroutine standardize_function_result(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical :: type_std_enabled
        character(len=64) :: preferred_name

        call get_standardizer_type_standardization(type_std_enabled)
        call determine_preferred_result_name(arena, func_def, preferred_name)
        call apply_result_variable(arena, func_def, func_index, preferred_name)

        if (.not. allocated(func_def%result_variable)) return
        if (len_trim(func_def%result_variable) == 0) return

        call sync_result_declaration(arena, func_def, func_index, type_std_enabled)
    end subroutine standardize_function_result

    subroutine determine_preferred_result_name(arena, func_def, preferred_name)
        use ast_nodes_core, only: assignment_node, identifier_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        character(len=*), intent(out) :: preferred_name
        character(len=64) :: fallback_name
        character(len=64) :: function_name
        character(len=64) :: target_name
        integer :: body_index
        integer :: target_index
        integer :: i

        preferred_name = ""
        fallback_name = ""
        function_name = ""
        if (allocated(func_def%name)) function_name = trim(func_def%name)
        if (.not. allocated(func_def%body_indices)) return

        do i = 1, size(func_def%body_indices)
            body_index = func_def%body_indices(i)
            if (body_index <= 0 .or. body_index > arena%size) cycle
            if (.not. allocated(arena%entries(body_index)%node)) cycle
            select type (stmt => arena%entries(body_index)%node)
            type is (assignment_node)
                target_index = stmt%target_index
                if (target_index <= 0 .or. target_index > arena%size) cycle
                if (.not. allocated(arena%entries(target_index)%node)) cycle
                select type (target_node => arena%entries(target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target_node%name)) cycle
                    target_name = trim(target_node%name)
                    if (len_trim(target_name) == 0) cycle
                    if (len_trim(function_name) > 0) then
                        if (trim(target_name) == trim(function_name)) then
                            preferred_name = trim(function_name)
                            return
                        end if
                    end if
                    if (len_trim(fallback_name) == 0) fallback_name = trim(target_name)
                end select
            end select
        end do

        if (len_trim(preferred_name) == 0) preferred_name = trim(fallback_name)
    end subroutine determine_preferred_result_name

    subroutine apply_result_variable(arena, func_def, func_index, preferred_name)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: preferred_name
        character(len=64) :: trimmed_name
        character(len=64) :: function_name

        trimmed_name = trim(preferred_name)
        if (len_trim(trimmed_name) == 0) return

        function_name = ""
        if (allocated(func_def%name)) function_name = trim(func_def%name)

        if ((.not. allocated(func_def%result_variable)) .or. &
            len_trim(func_def%result_variable) == 0 .or. &
            (len_trim(function_name) > 0 .and. &
             trim(trimmed_name) == trim(function_name) .and. &
             trim(func_def%result_variable) /= trim(function_name))) then
            func_def%result_variable = trimmed_name
            if (trimmed_name /= "result") then
                if (allocated(func_def%body_indices)) then
                    call rename_identifier_in_arena(arena, "result", trimmed_name, &
                                                    func_def%body_indices, &
                                                    func_index)
                end if
            end if
        end if
    end subroutine apply_result_variable

    subroutine sync_result_declaration(arena, func_def, func_index, type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical, intent(in) :: type_std_enabled
        logical :: has_decl
        type(declaration_node) :: existing_decl
        integer :: decl_index

        if (has_function_name_result(func_def)) then
            call ensure_function_name_return_type(func_def, type_std_enabled)
            arena%entries(func_index)%node = func_def
            return
        end if

        call find_result_declaration(arena, func_def, has_decl, decl_index, &
                                     existing_decl)
        if (has_decl) then
            call update_return_type_from_existing(func_def, arena, func_index, &
                                                  existing_decl)
            return
        end if

        call create_result_declaration(arena, func_def, func_index, &
                                       type_std_enabled)
    end subroutine sync_result_declaration

    subroutine find_result_declaration(arena, func_def, has_decl, decl_index, &
                                       existing_decl)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        logical, intent(out) :: has_decl
        integer, intent(out) :: decl_index
        type(declaration_node), intent(out) :: existing_decl
        integer :: i
        integer :: name_pos

        has_decl = .false.
        decl_index = 0
        if (.not. allocated(func_def%body_indices)) return

        do i = 1, size(func_def%body_indices)
            decl_index = func_def%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (trim(stmt%var_name) == trim(func_def%result_variable)) then
                    has_decl = .true.
                    existing_decl = stmt
                    return
                end if
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do name_pos = 1, size(stmt%var_names)
                        if (trim(stmt%var_names(name_pos)) == &
                            trim(func_def%result_variable)) then
                            has_decl = .true.
                            existing_decl = stmt
                            return
                        end if
                    end do
                end if
            end select
        end do

        decl_index = 0
    end subroutine find_result_declaration

    subroutine update_return_type_from_existing(func_def, arena, func_index, &
                                                existing_decl)
        use ast_nodes_data, only: declaration_node
        type(function_def_node), intent(inout) :: func_def
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        type(declaration_node), intent(in) :: existing_decl
        character(len=64) :: function_name

        function_name = ""
        if (allocated(func_def%name)) function_name = trim(func_def%name)

        if (len_trim(function_name) > 0) then
            if (trim(func_def%result_variable) == trim(function_name)) then
                if (is_character_length_decl(existing_decl%type_name)) then
                    func_def%return_type = ""
                    arena%entries(func_index)%node = func_def
                    return
                end if
            end if
        end if

        if (existing_decl%is_array) then
            func_def%return_type = ""
            arena%entries(func_index)%node = func_def
            return
        end if

        if (.not. allocated(func_def%return_type) .or. &
            len_trim(func_def%return_type) == 0) then
            if (len_trim(existing_decl%type_name) > 0) then
                if (existing_decl%has_kind .and. existing_decl%kind_value > 0 .and. &
                    existing_decl%type_name /= "character") then
                    call assign_return_type_with_kind(func_def, &
                                                      existing_decl%type_name, &
                                                      existing_decl%kind_value)
                else
                    func_def%return_type = trim(existing_decl%type_name)
                end if
            end if
        end if

        arena%entries(func_index)%node = func_def
    end subroutine update_return_type_from_existing

    subroutine assign_return_type_with_kind(func_def, base_type, kind_value)
        type(function_def_node), intent(inout) :: func_def
        character(len=*), intent(in) :: base_type
        integer, intent(in) :: kind_value
        character(len=64) :: buffer

        write (buffer, '(A,"(",I0,")")') trim(base_type), kind_value
        func_def%return_type = trim(buffer)
    end subroutine assign_return_type_with_kind

    logical function has_function_name_result(func_def) result(is_match)
        type(function_def_node), intent(in) :: func_def

        is_match = .false.
        if (.not. allocated(func_def%result_variable)) return
        if (.not. allocated(func_def%name)) return
        if (len_trim(func_def%name) == 0) return
        if (len_trim(func_def%result_variable) == 0) return
        is_match = trim(func_def%result_variable) == trim(func_def%name)
    end function has_function_name_result

    subroutine ensure_function_name_return_type(func_def, type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(function_def_node), intent(inout) :: func_def
        logical, intent(in) :: type_std_enabled
        type(declaration_node) :: decl
        logical :: result_inferred

        call reset_declaration_node(decl)
        result_inferred = .false.
        if (.not. fill_decl_from_return_type(func_def, decl)) then
            call infer_result_type(func_def%result_variable, decl)
            result_inferred = .true.
        end if

        if (decl%type_name == "real" .and. type_std_enabled .and. result_inferred) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        call ensure_function_return_type(func_def, decl)
    end subroutine ensure_function_name_return_type

    subroutine create_result_declaration(arena, func_def, func_index, &
                                         type_std_enabled)
        use ast_nodes_data, only: declaration_node
        use, intrinsic :: iso_fortran_env, only: error_unit
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical, intent(in) :: type_std_enabled
        type(declaration_node) :: decl
        logical :: result_inferred

        call reset_declaration_node(decl)

        if (allocated(func_def%return_type)) then
            if (is_type_variable_str(func_def%return_type) .or. &
                func_def%return_type == "function" .or. &
                func_def%return_type == "derived_type") then
                func_def%return_type = ""
            end if
        end if

        result_inferred = .false.
        if (.not. fill_decl_from_return_type(func_def, decl)) then
            call infer_result_type(func_def%result_variable, decl)
            result_inferred = .true.
        end if

        if (decl%type_name == "real" .and. type_std_enabled .and. result_inferred) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        call ensure_function_return_type(func_def, decl)
        decl%var_name = trim(func_def%result_variable)
        decl%intent = ""
        decl%has_intent = .false.
        decl%is_optional = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1

        call arena%push(decl, "declaration", func_index)
        call insert_result_declaration_into_body(func_def, arena%size)
        arena%entries(func_index)%node = func_def
    end subroutine create_result_declaration

    subroutine reset_declaration_node(decl)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(out) :: decl

        decl%type_name = ""
        decl%has_kind = .false.
        decl%kind_value = 0
        decl%is_array = .false.
        decl%is_allocatable = .false.
        decl%is_multi_declaration = .false.
        decl%is_parameter = .false.
        decl%var_name = ""
        decl%intent = ""
        decl%has_intent = .false.
        decl%is_optional = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1
        if (allocated(decl%dimension_indices)) deallocate (decl%dimension_indices)
        if (allocated(decl%var_names)) deallocate (decl%var_names)
    end subroutine reset_declaration_node

    logical function fill_decl_from_return_type(func_def, decl)
        use ast_nodes_data, only: declaration_node
        type(function_def_node), intent(in) :: func_def
        type(declaration_node), intent(inout) :: decl
        character(len=:), allocatable :: rt
        character(len=64) :: base_text
        character(len=64) :: attr_text
        integer :: open_pos
        integer :: close_pos
        integer :: inner_close
        integer :: read_stat
        integer :: kind_val

        fill_decl_from_return_type = .false.
        if (.not. allocated(func_def%return_type)) return
        if (len_trim(func_def%return_type) == 0) return

        rt = trim(func_def%return_type)
        open_pos = index(rt, "(")
        if (open_pos > 0) then
            inner_close = index(rt(open_pos + 1:), ")")
            if (inner_close > 0) then
                close_pos = open_pos + inner_close
            else
                close_pos = 0
            end if
        else
            close_pos = 0
        end if

        if (open_pos > 0 .and. close_pos > open_pos) then
            base_text = trim(rt(1:open_pos - 1))
            attr_text = trim(rt(open_pos + 1:close_pos - 1))
            read_stat = 0
            read (attr_text, *, iostat=read_stat) kind_val
            if (read_stat == 0) then
                decl%type_name = trim(base_text)
                decl%has_kind = .true.
                decl%kind_value = kind_val
            else
                decl%type_name = trim(rt)
            end if
        else
            decl%type_name = trim(rt)
        end if

        fill_decl_from_return_type = len_trim(decl%type_name) > 0
    end function fill_decl_from_return_type

    subroutine infer_result_type(var_name, decl)
        use ast_nodes_data, only: declaration_node
        character(len=*), intent(in) :: var_name
        type(declaration_node), intent(inout) :: decl
        character(len=32) :: inferred_type

        call infer_parameter_type(var_name, inferred_type, decl%has_kind, &
                                  decl%kind_value)
        decl%type_name = trim(inferred_type)
    end subroutine infer_result_type

    subroutine ensure_function_return_type(func_def, decl)
        use ast_nodes_data, only: declaration_node
        type(function_def_node), intent(inout) :: func_def
        type(declaration_node), intent(in) :: decl

        if (.not. allocated(func_def%return_type) .or. &
            len_trim(func_def%return_type) == 0) then
            if (len_trim(decl%type_name) == 0) return
            if (decl%has_kind .and. decl%kind_value > 0 .and. &
                decl%type_name /= "character") then
                call assign_return_type_with_kind(func_def, decl%type_name, &
                                                  decl%kind_value)
            else
                func_def%return_type = trim(decl%type_name)
            end if
        end if
    end subroutine ensure_function_return_type

    subroutine insert_result_declaration_into_body(func_def, new_decl_index)
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: new_decl_index
        integer, allocatable :: new_body_indices(:)
        integer :: original_size

        if (.not. allocated(func_def%body_indices)) then
            allocate (func_def%body_indices(1))
            func_def%body_indices(1) = new_decl_index
            return
        end if

        original_size = size(func_def%body_indices)
        allocate (new_body_indices(original_size + 1))
        if (original_size == 0) then
            new_body_indices(1) = new_decl_index
        else
            new_body_indices(1) = func_def%body_indices(1)
            new_body_indices(2) = new_decl_index
            if (original_size > 1) then
                new_body_indices(3:) = func_def%body_indices(2:)
            end if
        end if
        func_def%body_indices = new_body_indices
    end subroutine insert_result_declaration_into_body

    logical function is_character_length_decl(type_name) result(is_match)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(type_name)
        if (len_trim(lowered) == 0) then
            is_match = .false.
            return
        end if
        if (.not. is_character_type_string(type_name)) then
            is_match = .false.
            return
        end if
        is_match = (index(lowered, 'len=') > 0) .and. &
                   (index(lowered, 'len=*') == 0) .and. &
                   (index(lowered, 'len=:') == 0)
    end function is_character_length_decl

    ! Standardize function parameters by updating existing declarations or adding new ones
    subroutine standardize_function_parameters(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(param_metadata_t) :: metadata
        logical :: standardizer_type_standardization_enabled
        logical :: requires_intent_in_flag
        integer :: n_params

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)
        call init_param_metadata(metadata, n_params)

        requires_intent_in_flag = needs_intent_in(func_def)
        if (requires_intent_in_flag) metadata%intent = "in"

        call populate_param_metadata(arena, func_def, metadata)
        call analyze_parameter_usage(arena, func_def, metadata)
        call finalize_param_types(metadata)

        if (allocated(func_def%body_indices)) then
            call synchronize_parameter_declarations( &
                arena, func_def%body_indices, metadata, "in", &
                standardizer_type_standardization_enabled)
        end if

        call add_missing_parameter_declarations_ext( &
            arena, func_def, func_index, metadata, &
            standardizer_type_standardization_enabled)

        if (requires_intent_in_flag) then
            call rebuild_parameter_declarations( &
                arena, func_def, func_index, metadata, &
                standardizer_type_standardization_enabled)
        end if

        call set_function_param_intents(func_def, metadata)
    end subroutine standardize_function_parameters

    subroutine init_param_metadata(metadata, n_params)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: n_params

        if (n_params <= 0) return
        if (allocated(metadata%names)) deallocate (metadata%names)
        if (allocated(metadata%found)) deallocate (metadata%found)
        if (allocated(metadata%optional)) deallocate (metadata%optional)
        if (allocated(metadata%intent)) deallocate (metadata%intent)
        if (allocated(metadata%type_name)) deallocate (metadata%type_name)
        if (allocated(metadata%has_kind)) deallocate (metadata%has_kind)
        if (allocated(metadata%kind_value)) deallocate (metadata%kind_value)
        if (allocated(metadata%is_array)) deallocate (metadata%is_array)
        if (allocated(metadata%is_allocatable)) deallocate (metadata%is_allocatable)
        if (allocated(metadata%type_inferred)) deallocate (metadata%type_inferred)
        if (allocated(metadata%rank)) deallocate (metadata%rank)

        allocate (character(len=64) :: metadata%names(n_params))
        allocate (metadata%found(n_params))
        allocate (metadata%optional(n_params))
        allocate (character(len=8) :: metadata%intent(n_params))
        allocate (character(len=64) :: metadata%type_name(n_params))
        allocate (metadata%has_kind(n_params))
        allocate (metadata%kind_value(n_params))
        allocate (metadata%is_array(n_params))
        allocate (metadata%is_allocatable(n_params))
        allocate (metadata%type_inferred(n_params))
        allocate (metadata%rank(n_params))

        metadata%names = ""
        metadata%found = 0
        metadata%optional = .false.
        metadata%intent = ""
        metadata%type_name = ""
        metadata%has_kind = .false.
        metadata%kind_value = 0
        metadata%is_array = .false.
        metadata%is_allocatable = .false.
        metadata%type_inferred = .true.
        metadata%rank = 0
    end subroutine init_param_metadata

    logical function needs_intent_in(func_def)
        type(function_def_node), intent(in) :: func_def
        integer :: i

        needs_intent_in = .false.
        if (.not. allocated(func_def%prefix_keywords)) return
        do i = 1, size(func_def%prefix_keywords)
            select case (trim(func_def%prefix_keywords(i)))
            case ("pure", "elemental")
                needs_intent_in = .true.
                return
            end select
        end do
    end function needs_intent_in

    subroutine populate_param_metadata(arena, func_def, metadata)
        use ast_nodes_core, only: identifier_node
        use ast_nodes_data, only: declaration_node, parameter_declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        type(param_metadata_t), intent(inout) :: metadata
        integer :: i
        integer :: idx

        do i = 1, size(func_def%param_indices)
            idx = func_def%param_indices(i)
            if (.not. node_exists(arena, idx)) then
                call assign_default_param_name(metadata, i, i)
                cycle
            end if

            select type (param => arena%entries(idx)%node)
            type is (identifier_node)
                call update_metadata_from_identifier(metadata, i, param)
            type is (parameter_declaration_node)
                call update_metadata_from_parameter_decl(metadata, i, param)
            type is (declaration_node)
                call update_metadata_from_declaration(metadata, i, param)
            class default
                call assign_default_param_name(metadata, i, i)
            end select

            call ensure_metadata_name(metadata, i, i)
        end do
    end subroutine populate_param_metadata

    subroutine update_metadata_from_identifier(metadata, slot, param)
        use ast_nodes_core, only: identifier_node
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        type(identifier_node), intent(in) :: param
        character(len=:), allocatable :: inferred_text

        metadata%names(slot) = param%name
        inferred_text = param%inferred_type%to_string()
        call apply_function_type(metadata, slot, .false., "", .false., 0, &
                                 param%inferred_type%kind > 0, inferred_text, &
                                 .false., .false.)
    end subroutine update_metadata_from_identifier

    subroutine update_metadata_from_parameter_decl(metadata, slot, param)
        use ast_nodes_data, only: parameter_declaration_node
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        type(parameter_declaration_node), intent(in) :: param
        character(len=:), allocatable :: inferred_text
        character(len=:), allocatable :: explicit_type
        logical :: has_explicit_type

        metadata%names(slot) = param%name
        metadata%optional(slot) = param%is_optional
        inferred_text = param%inferred_type%to_string()
        has_explicit_type = allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0
        if (has_explicit_type) then
            explicit_type = trim(param%type_name)
        else
            explicit_type = ""
        end if
        call apply_function_type(metadata, slot, has_explicit_type, explicit_type, &
                                 param%has_kind, param%kind_value, &
                                 param%inferred_type%kind > 0, inferred_text, &
                                 param%is_array, .false.)
        call copy_parameter_intent(metadata, slot, param%intent_type)
    end subroutine update_metadata_from_parameter_decl

    subroutine update_metadata_from_declaration(metadata, slot, param)
        use ast_nodes_data, only: declaration_node
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        type(declaration_node), intent(in) :: param
        character(len=:), allocatable :: inferred_text
        character(len=:), allocatable :: explicit_type
        logical :: has_explicit_type

        metadata%names(slot) = param%var_name
        inferred_text = param%inferred_type%to_string()
        has_explicit_type = allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0
        if (has_explicit_type) then
            explicit_type = trim(param%type_name)
        else
            explicit_type = ""
        end if
        call apply_function_type(metadata, slot, has_explicit_type, explicit_type, &
                                 param%has_kind, param%kind_value, &
                                 param%inferred_type%kind > 0, inferred_text, &
                                 param%is_array, param%is_allocatable)
        if (param%has_intent .and. allocated(param%intent)) then
            metadata%intent(slot) = trim(param%intent)
        end if
    end subroutine update_metadata_from_declaration

    subroutine copy_parameter_intent(metadata, slot, intent_kind)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        integer, intent(in) :: intent_kind

        select case (intent_kind)
        case (INTENT_IN)
            metadata%intent(slot) = "in"
        case (INTENT_OUT)
            metadata%intent(slot) = "out"
        case (INTENT_INOUT)
            metadata%intent(slot) = "inout"
        end select
    end subroutine copy_parameter_intent

    subroutine assign_default_param_name(metadata, slot, param_number)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        integer, intent(in) :: param_number

        metadata%names(slot) = default_param_name(param_number)
    end subroutine assign_default_param_name

    subroutine ensure_metadata_name(metadata, slot, param_number)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: slot
        integer, intent(in) :: param_number

        if (len_trim(metadata%names(slot)) == 0) then
            metadata%names(slot) = default_param_name(param_number)
        end if
    end subroutine ensure_metadata_name

    character(len=64) function default_param_name(param_number) result(name)
        integer, intent(in) :: param_number

        write (name, '(a,i0)') "param", param_number
    end function default_param_name

    logical function node_exists(arena, index) result(has_node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index

        has_node = .false.
        if (index <= 0 .or. index > arena%size) return
        has_node = allocated(arena%entries(index)%node)
    end function node_exists

    subroutine analyze_parameter_usage(arena, func_def, metadata)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        type(param_metadata_t), intent(inout) :: metadata
        integer :: body_idx

        if (.not. allocated(func_def%body_indices)) return
        do body_idx = 1, size(func_def%body_indices)
            call scan_node(arena, func_def%body_indices(body_idx), metadata)
        end do
    end subroutine analyze_parameter_usage

    recursive subroutine scan_node(arena, node_index, metadata)
        use ast_nodes_core, only: assignment_node, binary_op_node
        use ast_nodes_core, only: call_or_subscript_node, component_access_node
        use ast_nodes_bounds, only: array_slice_node, array_bounds_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(param_metadata_t), intent(inout) :: metadata

        if (.not. node_exists(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call scan_assignment_children(arena, node, metadata)
        type is (binary_op_node)
            call scan_binary_children(arena, node, metadata)
        type is (call_or_subscript_node)
            call handle_call_or_subscript(arena, node, metadata)
            call scan_optional_child(arena, node%base_expr_index, metadata)
            call scan_argument_list(arena, node, metadata)
        type is (component_access_node)
            call scan_optional_child(arena, node%base_expr_index, metadata)
        type is (array_slice_node)
            call scan_array_slice_children(arena, node, metadata)
        type is (array_bounds_node)
            call scan_array_bounds_children(arena, node, metadata)
        class default
            call scan_generic_children(arena, node_index, metadata)
        end select
    end subroutine scan_node

    subroutine scan_assignment_children(arena, node, metadata)
        use ast_nodes_core, only: assignment_node
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%target_index, metadata)
        call scan_optional_child(arena, node%value_index, metadata)
    end subroutine scan_assignment_children

    subroutine scan_binary_children(arena, node, metadata)
        use ast_nodes_core, only: binary_op_node
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%left_index, metadata)
        call scan_optional_child(arena, node%right_index, metadata)
    end subroutine scan_binary_children

    subroutine scan_argument_list(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        integer :: j

        if (.not. allocated(node%arg_indices)) return
        do j = 1, size(node%arg_indices)
            call scan_optional_child(arena, node%arg_indices(j), metadata)
        end do
    end subroutine scan_argument_list

    subroutine scan_array_slice_children(arena, node, metadata)
        use ast_nodes_bounds, only: array_slice_node
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        integer :: j

        call scan_optional_child(arena, node%array_index, metadata)
        do j = 1, node%num_dimensions
            call scan_optional_child(arena, node%bounds_indices(j), metadata)
        end do
    end subroutine scan_array_slice_children

    subroutine scan_array_bounds_children(arena, node, metadata)
        use ast_nodes_bounds, only: array_bounds_node
        type(ast_arena_t), intent(in) :: arena
        type(array_bounds_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%lower_bound_index, metadata)
        call scan_optional_child(arena, node%upper_bound_index, metadata)
        call scan_optional_child(arena, node%stride_index, metadata)
    end subroutine scan_array_bounds_children

    subroutine scan_generic_children(arena, node_index, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(param_metadata_t), intent(inout) :: metadata
        integer, allocatable :: child_indices(:)
        integer :: j

        child_indices = get_child_list(arena, node_index)
        if (.not. allocated(child_indices)) return
        do j = 1, size(child_indices)
            call scan_optional_child(arena, child_indices(j), metadata)
        end do
    end subroutine scan_generic_children

    subroutine scan_optional_child(arena, child_index, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: child_index
        type(param_metadata_t), intent(inout) :: metadata

        if (.not. node_exists(arena, child_index)) return
        call scan_node(arena, child_index, metadata)
    end subroutine scan_optional_child

    subroutine handle_call_or_subscript(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        character(len=:), allocatable :: target_name
        integer :: idx
        integer :: rank_size

        target_name = ""
        if (allocated(node%name)) target_name = trim(node%name)
        if (len_trim(target_name) == 0) then
            if (node%base_expr_index > 0) then
                target_name = resolve_name_from_index(arena, node%base_expr_index)
            end if
        end if
        if (len_trim(target_name) == 0) return
        idx = metadata_find_param(metadata, target_name)
        if (idx <= 0) return

        metadata%is_array(idx) = .true.
        if (allocated(node%arg_indices)) then
            rank_size = count(node%arg_indices > 0)
        else
            rank_size = 0
        end if
        if (rank_size <= 0) rank_size = 1
        if (rank_size > metadata%rank(idx)) metadata%rank(idx) = rank_size
    end subroutine handle_call_or_subscript

    recursive function resolve_name_from_index(arena, idx) result(name)
        use ast_nodes_core, only: call_or_subscript_node, component_access_node
        use ast_nodes_core, only: identifier_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: name

        name = ""
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (base => arena%entries(idx)%node)
        type is (identifier_node)
            if (allocated(base%name)) name = trim(base%name)
        type is (call_or_subscript_node)
            if (allocated(base%name)) then
                name = trim(base%name)
            else if (base%base_expr_index > 0) then
                name = resolve_name_from_index(arena, base%base_expr_index)
            end if
        type is (component_access_node)
            name = resolve_name_from_index(arena, base%base_expr_index)
        class default
            name = ""
        end select
    end function resolve_name_from_index

    function get_child_list(arena, node_index) result(indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: indices(:)
        integer :: count_children

        allocate (indices(0))
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        count_children = arena%entries(node_index)%child_count
        if (count_children <= 0) return
        if (allocated(indices)) deallocate (indices)
        allocate (indices(count_children))
        indices = arena%entries(node_index)%child_indices(1:count_children)
    end function get_child_list

    subroutine finalize_param_types(metadata)
        type(param_metadata_t), intent(inout) :: metadata
        integer :: i
        character(len=32) :: inferred_type
        logical :: has_kind_local
        integer :: kind_value_local

        do i = 1, size(metadata%names)
            if (is_type_variable_str(metadata%type_name(i))) then
                call infer_parameter_type(metadata%names(i), inferred_type, &
                                          has_kind_local, kind_value_local)
                metadata%type_name(i) = trim(inferred_type)
                metadata%has_kind(i) = has_kind_local
                metadata%kind_value(i) = kind_value_local
                metadata%type_inferred(i) = .true.
            end if
            if (metadata%is_array(i)) then
                if (metadata%rank(i) <= 0) metadata%rank(i) = 1
                if (len_trim(metadata%type_name(i)) == 0) &
                    metadata%type_name(i) = "real"
            end if
        end do
    end subroutine finalize_param_types

    subroutine set_function_param_intents(func_def, metadata)
        type(function_def_node), intent(inout) :: func_def
        type(param_metadata_t), intent(in) :: metadata
        integer :: n_params

        n_params = size(metadata%names)
        if (allocated(func_def%param_intents)) deallocate (func_def%param_intents)
        if (n_params <= 0) return
        allocate (character(len=8) :: func_def%param_intents(n_params))
        func_def%param_intents = metadata%intent
    end subroutine set_function_param_intents

    subroutine apply_function_type(metadata, idx, type_present, type_text, &
                                   has_kind_flag, kind_value, inferred_present, &
                                   inferred_text, is_array_flag, is_alloc_flag)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: idx
        logical, intent(in) :: type_present
        character(len=*), intent(in) :: type_text
        logical, intent(in) :: has_kind_flag
        integer, intent(in) :: kind_value
        logical, intent(in) :: inferred_present
        character(len=*), intent(in) :: inferred_text
        logical, intent(in) :: is_array_flag
        logical, intent(in) :: is_alloc_flag

        if (type_present .and. len_trim(type_text) > 0 .and. &
            .not. is_type_variable_str(type_text)) then
            metadata%type_name(idx) = trim(type_text)
            metadata%type_inferred(idx) = .false.
        else if (inferred_present .and. len_trim(inferred_text) > 0 .and. &
                 .not. is_type_variable_str(inferred_text)) then
            metadata%type_name(idx) = trim(inferred_text)
            metadata%type_inferred(idx) = .false.
        end if

        metadata%has_kind(idx) = has_kind_flag
        if (has_kind_flag) then
            metadata%kind_value(idx) = kind_value
        else
            metadata%kind_value(idx) = 0
        end if

        metadata%is_array(idx) = is_array_flag
        metadata%is_allocatable(idx) = is_alloc_flag
    end subroutine apply_function_type

    ! Add missing parameter declarations
    subroutine add_missing_parameter_declarations_ext(arena, func_def, func_index, &
                                                      metadata, type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(param_metadata_t), intent(inout) :: metadata
        logical, intent(in) :: type_std_enabled
        integer :: missing_count
        integer :: i
        integer :: new_count
        integer, allocatable :: existing(:)
        integer, allocatable :: new_indices(:)
        type(declaration_node) :: decl
        logical :: inferred_local

        missing_count = count(metadata%found == 0)
        if (missing_count == 0) return

        if (allocated(func_def%body_indices)) then
            allocate (existing(size(func_def%body_indices)))
            existing = func_def%body_indices
        else
            allocate (existing(0))
        end if

        allocate (new_indices(missing_count))
        new_count = 0

        do i = 1, size(metadata%names)
            if (metadata%found(i) /= 0) cycle
            call reset_declaration_node(decl)
            inferred_local = metadata%type_inferred(i)
            call fill_parameter_declaration(decl, metadata, i, type_std_enabled, &
                                            inferred_local)
            decl%intent = choose_param_intent(metadata%intent(i))
            decl%has_intent = .true.
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", func_index)
            new_count = new_count + 1
            new_indices(new_count) = arena%size
            metadata%found(i) = arena%size
        end do

        call combine_existing_and_new_body(func_def, existing, new_indices, new_count)
    end subroutine add_missing_parameter_declarations_ext

    subroutine rebuild_parameter_declarations(arena, func_def, func_index, metadata, &
                                              type_std_enabled)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(in) :: type_std_enabled
        integer, allocatable :: existing(:)
        integer, allocatable :: new_indices(:)

        if (.not. allocated(func_def%body_indices)) return
        if (.not. allocated(metadata%names)) return
        if (size(metadata%names) == 0) return

        call collect_non_param_body_indices(arena, func_def, metadata, existing)
        call build_replacement_parameter_declarations(arena, func_def, func_index, &
                                                      metadata, type_std_enabled, &
                                                      new_indices)
        call combine_existing_and_new_body(func_def, existing, new_indices, &
                                           size(new_indices))
    end subroutine rebuild_parameter_declarations

    subroutine collect_non_param_body_indices(arena, func_def, metadata, collected)
        use ast_nodes_data, only: declaration_node, parameter_declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        type(param_metadata_t), intent(in) :: metadata
        integer, allocatable, intent(out) :: collected(:)
        integer :: i
        integer :: idx

        allocate (collected(0))
        if (.not. allocated(func_def%body_indices)) return
        do i = 1, size(func_def%body_indices)
            idx = func_def%body_indices(i)
            if (.not. node_exists(arena, idx)) cycle
            if (should_skip_existing_param(arena, idx, metadata)) cycle
            collected = [collected, idx]
        end do
    end subroutine collect_non_param_body_indices

    logical function should_skip_existing_param(arena, idx, metadata) result(skip)
        use ast_nodes_data, only: declaration_node, parameter_declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(param_metadata_t), intent(in) :: metadata
        integer :: name_idx

        skip = .false.
        if (.not. node_exists(arena, idx)) return

        select type (stmt => arena%entries(idx)%node)
        type is (declaration_node)
            skip = is_metadata_parameter(metadata, stmt%var_name)
            if (.not. skip) then
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do name_idx = 1, size(stmt%var_names)
                        if (is_metadata_parameter(metadata, &
                                                  stmt%var_names(name_idx))) then
                            skip = .true.
                            exit
                        end if
                    end do
                end if
            end if
        type is (parameter_declaration_node)
            skip = .true.
        class default
            skip = .false.
        end select
    end function should_skip_existing_param

    subroutine build_replacement_parameter_declarations(arena, func_def, func_index, &
                                                        metadata, type_std_enabled, &
                                                        new_indices)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(param_metadata_t), intent(in) :: metadata
        logical, intent(in) :: type_std_enabled
        integer, allocatable, intent(out) :: new_indices(:)
        type(declaration_node) :: decl
        logical :: inferred_local
        integer :: i
        integer :: n_params

        n_params = size(metadata%names)
        if (n_params <= 0) then
            allocate (new_indices(0))
            return
        end if

        allocate (new_indices(n_params))
        do i = 1, n_params
            call reset_declaration_node(decl)
            inferred_local = metadata%type_inferred(i)
            call fill_parameter_declaration(decl, metadata, i, type_std_enabled, &
                                            inferred_local)
            decl%intent = "in"
            decl%has_intent = .true.
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", func_index)
            new_indices(i) = arena%size
        end do
    end subroutine build_replacement_parameter_declarations

    subroutine fill_parameter_declaration(decl, metadata, idx, type_std_enabled, &
                                          inferred_local)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: decl
        type(param_metadata_t), intent(in) :: metadata
        integer, intent(in) :: idx
        logical, intent(in) :: type_std_enabled
        logical, intent(inout) :: inferred_local
        character(len=32) :: inferred_type

        if (len_trim(metadata%type_name(idx)) > 0 .and. &
            .not. is_type_variable_str(metadata%type_name(idx))) then
            decl%type_name = trim(metadata%type_name(idx))
            decl%has_kind = metadata%has_kind(idx)
            decl%kind_value = metadata%kind_value(idx)
            inferred_local = metadata%type_inferred(idx)
        else
            call infer_parameter_type(metadata%names(idx), inferred_type, &
                                      decl%has_kind, decl%kind_value)
            decl%type_name = trim(inferred_type)
            inferred_local = .true.
        end if

        if (decl%type_name == "real" .and. type_std_enabled .and. inferred_local) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        decl%is_array = metadata%is_array(idx)
        decl%is_allocatable = metadata%is_allocatable(idx)
        decl%var_name = metadata%names(idx)
        decl%is_parameter = .true.
        if (decl%is_array) call ensure_deferred_shape(decl, metadata%rank(idx))
    end subroutine fill_parameter_declaration

    character(len=8) function choose_param_intent(intent_value) result(result_intent)
        character(len=*), intent(in) :: intent_value

        if (len_trim(intent_value) > 0) then
            result_intent = trim(intent_value)
        else
            result_intent = "in"
        end if
    end function choose_param_intent

    subroutine combine_existing_and_new_body(func_def, existing, &
                                             new_indices, new_count)
        type(function_def_node), intent(inout) :: func_def
        integer, allocatable, intent(in) :: existing(:)
        integer, intent(in) :: new_indices(:)
        integer, intent(in) :: new_count
        integer :: existing_count
        integer, allocatable :: combined(:)

        existing_count = size(existing)
        if (new_count <= 0) then
            if (existing_count > 0) then
                func_def%body_indices = existing
            else
                if (allocated(func_def%body_indices)) deallocate &
                    (func_def%body_indices)
            end if
            return
        end if

        if (existing_count == 0) then
            allocate (combined(new_count))
            combined(1:new_count) = new_indices(1:new_count)
        else
            allocate (combined(existing_count + new_count))
            combined(1) = existing(1)
            if (new_count > 0) combined(2:new_count + 1) = new_indices(1:new_count)
            if (existing_count > 1) combined(new_count + 2:) = existing(2:)
        end if
        func_def%body_indices = combined
    end subroutine combine_existing_and_new_body

    logical function is_metadata_parameter(metadata, name) result(is_match)
        type(param_metadata_t), intent(in) :: metadata
        character(len=*), intent(in) :: name
        integer :: k

        is_match = .false.
        if (len_trim(name) == 0) return
        if (.not. allocated(metadata%names)) return
        do k = 1, size(metadata%names)
            if (trim(metadata%names(k)) == trim(name)) then
                is_match = .true.
                return
            end if
        end do
    end function is_metadata_parameter

    ! Standardize a subroutine definition
    subroutine standardize_subroutine_def(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j

        ! Add implicit none at the beginning of subroutine body
        if (allocated(sub_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=sub_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(sub_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(sub_def%body_indices)
                new_body_indices(i + 1) = sub_def%body_indices(i)
            end do
            sub_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_subroutine_parameters(arena, sub_def, sub_index)

        ! Update the arena entry
        arena%entries(sub_index)%node = sub_def
    end subroutine standardize_subroutine_def

    ! Infer parameter type from name patterns
    subroutine infer_parameter_type(param_name, type_name, has_kind, kind_value)
        character(len=*), intent(in) :: param_name
        character(len=*), intent(out) :: type_name
        logical, intent(out) :: has_kind
        integer, intent(out) :: kind_value
        character :: first_char

        has_kind = .false.
        kind_value = 0

        if (len_trim(param_name) > 0) then
            first_char = param_name(1:1)
            select case (first_char)
            case ('i', 'j', 'k', 'l', 'm', 'n')
                type_name = "integer"
            case ('x', 'y', 'z', 'r', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', &
                  'o', 'p', &
                  'q', 's', 't', 'u', 'v', 'w')
                type_name = "real"
            case default
                type_name = "real"  ! Default to real
            end select
        else
            type_name = "real"  ! Default fallback
        end if

    end subroutine infer_parameter_type

! Wrap a standalone function in a program
    subroutine wrap_function_in_program(arena, func_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: func_index

        call wrap_subprogram_common(arena, func_index, .true.)
    end subroutine wrap_function_in_program

! Wrap a standalone subroutine in a program
    subroutine wrap_subroutine_in_program(arena, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: sub_index

        call wrap_subprogram_common(arena, sub_index, .false.)
    end subroutine wrap_subroutine_in_program

    subroutine wrap_subprogram_common(arena, member_index, is_function)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: member_index
        logical, intent(in) :: is_function
        type(program_node) :: prog
        type(contains_node) :: contains_stmt
        integer :: implicit_none_index
        integer :: contains_index
        integer :: prog_index

        implicit_none_index = push_implicit_statement(arena, .true., &
                                                      line=1, column=1, parent_index=0)
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        call standardize_wrapper_member(arena, member_index, is_function)
        call initialize_wrapper_program(prog, implicit_none_index, contains_index, &
                                        member_index)

        call arena%push(prog, "program", 0)
        prog_index = arena%size
        call update_wrapper_parents(arena, implicit_none_index, contains_index, &
                                    member_index, prog_index)
        member_index = prog_index
    end subroutine wrap_subprogram_common

    subroutine standardize_wrapper_member(arena, member_index, is_function)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: member_index
        logical, intent(in) :: is_function

        if (.not. node_exists(arena, member_index)) return

        if (is_function) then
            select type (member => arena%entries(member_index)%node)
            type is (function_def_node)
                call standardize_function_def(arena, member, member_index)
            end select
        else
            select type (member => arena%entries(member_index)%node)
            type is (subroutine_def_node)
                call standardize_subroutine_def(arena, member, member_index)
            end select
        end if
    end subroutine standardize_wrapper_member

    subroutine initialize_wrapper_program(prog, implicit_none_index, contains_index, &
                                          member_index)
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: implicit_none_index
        integer, intent(in) :: contains_index
        integer, intent(in) :: member_index
        integer, allocatable :: body_indices(:)

        prog%name = "main"
        prog%line = 1
        prog%column = 1
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = member_index
        prog%body_indices = body_indices
    end subroutine initialize_wrapper_program

    subroutine update_wrapper_parents(arena, implicit_index, contains_index, &
                                      member_index, program_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: implicit_index
        integer, intent(in) :: contains_index
        integer, intent(in) :: member_index
        integer, intent(in) :: program_index

        if (node_exists(arena, implicit_index)) then
            arena%entries(implicit_index)%parent_index = program_index
        end if
        if (node_exists(arena, contains_index)) then
            arena%entries(contains_index)%parent_index = program_index
        end if
        if (node_exists(arena, member_index)) then
            arena%entries(member_index)%parent_index = program_index
        end if
    end subroutine update_wrapper_parents

! Standardize subroutine parameters by updating existing declarations or adding new ones
    subroutine standardize_subroutine_parameters(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        type(param_metadata_t) :: metadata
        logical :: type_std_enabled
        integer :: n_params

        if (.not. allocated(sub_def%param_indices)) return
        n_params = size(sub_def%param_indices)
        if (n_params == 0) return

        call get_standardizer_type_standardization(type_std_enabled)
        call init_param_metadata(metadata, n_params)

        call populate_subroutine_param_metadata(arena, sub_def, metadata)
        if (allocated(sub_def%body_indices)) then
            call synchronize_parameter_declarations( &
                arena, sub_def%body_indices, metadata, "", type_std_enabled)
        end if

        call add_missing_subroutine_parameter_declarations_ext( &
            arena, sub_def, sub_index, metadata, type_std_enabled)

        call set_subroutine_param_intents(sub_def, metadata)
    end subroutine standardize_subroutine_parameters

    subroutine populate_subroutine_param_metadata(arena, sub_def, metadata)
        use ast_nodes_core, only: identifier_node
        use ast_nodes_data, only: declaration_node, parameter_declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub_def
        type(param_metadata_t), intent(inout) :: metadata
        integer :: i
        integer :: idx

        do i = 1, size(sub_def%param_indices)
            metadata%names(i) = ""
            idx = sub_def%param_indices(i)
            if (idx <= 0 .or. idx > arena%size) then
                write (metadata%names(i), '(a,i0)') "param", i
                cycle
            end if
            if (.not. allocated(arena%entries(idx)%node)) then
                write (metadata%names(i), '(a,i0)') "param", i
                cycle
            end if
            select type (param => arena%entries(idx)%node)
            type is (identifier_node)
                metadata%names(i) = param%name
            type is (parameter_declaration_node)
                metadata%names(i) = param%name
                metadata%optional(i) = param%is_optional
                select case (param%intent_type)
                case (INTENT_IN)
                    metadata%intent(i) = "in"
                case (INTENT_OUT)
                    metadata%intent(i) = "out"
                case (INTENT_INOUT)
                    metadata%intent(i) = "inout"
                end select
            type is (declaration_node)
                metadata%names(i) = param%var_name
                if (param%has_intent .and. allocated(param%intent)) then
                    metadata%intent(i) = trim(param%intent)
                end if
            class default
                write (metadata%names(i), '(a,i0)') "param", i
            end select
            if (len_trim(metadata%names(i)) == 0) then
                write (metadata%names(i), '(a,i0)') "param", i
            end if
        end do
    end subroutine populate_subroutine_param_metadata

    subroutine add_missing_subroutine_parameter_declarations_ext(arena, sub_def, &
                                                                 sub_index, metadata, &
                                                                 type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        type(param_metadata_t), intent(inout) :: metadata
        logical, intent(in) :: type_std_enabled
        integer :: missing_count
        integer :: i
        integer, allocatable :: existing(:)
        integer, allocatable :: new_indices(:)
        type(declaration_node) :: decl
        logical :: inferred_local
        integer :: new_count

        missing_count = count(metadata%found == 0)
        if (missing_count == 0) return

        if (allocated(sub_def%body_indices)) then
            allocate (existing(size(sub_def%body_indices)))
            existing = sub_def%body_indices
        else
            allocate (existing(0))
        end if

        allocate (new_indices(missing_count))
        new_count = 0

        do i = 1, size(metadata%names)
            if (metadata%found(i) /= 0) cycle
            call reset_declaration_node(decl)
            inferred_local = metadata%type_inferred(i)
            call fill_parameter_declaration(decl, metadata, i, type_std_enabled, &
                                            inferred_local)
            decl%intent = choose_subroutine_intent(metadata%intent(i))
            decl%has_intent = .true.
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", sub_index)
            new_count = new_count + 1
            new_indices(new_count) = arena%size
            metadata%found(i) = arena%size
        end do

        call combine_existing_and_new_body_subroutine(sub_def, existing, &
                                                      new_indices, new_count)
    end subroutine add_missing_subroutine_parameter_declarations_ext

    subroutine set_subroutine_param_intents(sub_def, metadata)
        type(subroutine_def_node), intent(inout) :: sub_def
        type(param_metadata_t), intent(in) :: metadata
        integer :: n_params

        n_params = size(metadata%names)
        if (allocated(sub_def%param_intents)) deallocate (sub_def%param_intents)
        if (n_params <= 0) return
        allocate (character(len=8) :: sub_def%param_intents(n_params))
        sub_def%param_intents = metadata%intent
    end subroutine set_subroutine_param_intents

    character(len=8) function choose_subroutine_intent(intent_value) &
        result(result_intent)
        character(len=*), intent(in) :: intent_value

        if (len_trim(intent_value) > 0) then
            result_intent = trim(intent_value)
        else
            result_intent = "inout"
        end if
    end function choose_subroutine_intent

    subroutine combine_existing_and_new_body_subroutine(sub_def, existing, &
                                                        new_indices, new_count)
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, allocatable, intent(in) :: existing(:)
        integer, intent(in) :: new_indices(:)
        integer, intent(in) :: new_count
        integer :: existing_count
        integer, allocatable :: combined(:)

        existing_count = size(existing)
        if (new_count <= 0) then
            if (existing_count > 0) then
                sub_def%body_indices = existing
            else
                if (allocated(sub_def%body_indices)) deallocate (sub_def%body_indices)
            end if
            return
        end if

        if (existing_count == 0) then
            allocate (combined(new_count))
            combined(1:new_count) = new_indices(1:new_count)
        else
            allocate (combined(existing_count + new_count))
            combined(1) = existing(1)
            if (new_count > 0) combined(2:new_count + 1) = new_indices(1:new_count)
            if (existing_count > 1) combined(new_count + 2:) = existing(2:)
        end if
        sub_def%body_indices = combined
    end subroutine combine_existing_and_new_body_subroutine

end module standardizer_subprograms
