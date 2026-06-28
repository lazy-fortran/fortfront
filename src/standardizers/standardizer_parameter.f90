module standardizer_parameter
    use ast_arena_modern, only: ast_arena_t
    use string_utils_mod, only: to_lower
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use standardizer_declarations_state, only: &
        get_shared_type_standardization => get_standardizer_type_standardization
    implicit none
    private

    ! Input mode: Distinguish standard Fortran (.f90) vs lazy Fortran (.lf)
    ! Used to determine whether to add default intents and perform inference
    integer, save :: global_input_mode = INPUT_MODE_LAZY
    type :: param_metadata_t
        character(len=:), allocatable :: names(:)
        integer, allocatable :: found(:)
        logical, allocatable :: optional(:)
        character(len=:), allocatable :: intent(:)
        character(len=:), allocatable :: type_name(:)
        logical, allocatable :: is_unsigned(:)
        logical, allocatable :: has_kind(:)
        integer, allocatable :: kind_value(:)
        logical, allocatable :: is_array(:)
        logical, allocatable :: is_allocatable(:)
        logical, allocatable :: type_inferred(:)
        integer, allocatable :: rank(:)
        logical, allocatable :: is_procedure(:)
    end type param_metadata_t

    public :: param_metadata_t
    public :: get_standardizer_type_standardization
    public :: get_standardizer_input_mode, set_standardizer_input_mode
    public :: synchronize_parameter_declarations
    public :: init_param_metadata
    public :: metadata_find_param
    public :: fill_parameter_declaration
    public :: infer_parameter_type
    public :: is_type_variable_str
    public :: is_procedure_type
    public :: node_exists
    public :: reset_declaration_node

contains

    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        call get_shared_type_standardization(enabled)
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
    pure logical function is_procedure_type(type_name) result(is_proc)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        if (len_trim(type_name) == 0) then
            is_proc = .false.
            return
        end if

        lowered = to_lower(trim(type_name))
        if (len(lowered) < 9) then
            is_proc = .false.
        else
            is_proc = lowered(1:9) == "procedure"
        end if
    end function is_procedure_type

    subroutine synchronize_parameter_declarations(arena, body_indices, metadata, &
            default_intent, &
            standardize_types)
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

    subroutine sync_process_body_index(arena, body_index, metadata, &
            default_intent, &
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
            call sync_handle_declaration(stmt, body_index, metadata, &
                default_intent, &
                stmt_intent, standardize_types)
            arena%entries(body_index)%node = stmt
        end select
    end subroutine sync_process_body_index

    subroutine sync_handle_declaration(stmt, decl_index, metadata, &
            default_intent, &
            stmt_intent, standardize_types)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: decl_index
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        character(len=*), intent(inout) :: stmt_intent
        logical, intent(in) :: standardize_types
        integer :: param_idx
        logical :: suppress_intent

        if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
            call sync_handle_multi_declaration(stmt, decl_index, metadata, &
                default_intent, stmt_intent, &
                standardize_types)
            return
        end if

        param_idx = metadata_find_param(metadata, stmt%var_name)
        if (param_idx <= 0) return

        call sync_process_single_declaration(stmt, decl_index, param_idx, &
            metadata, &
            default_intent, stmt_intent, &
            suppress_intent)
        call sync_infer_type_if_variable(stmt, metadata%names(param_idx))
        call apply_type_standardization_to_stmt(stmt, standardize_types)
        call sync_finalize_intent(stmt, stmt_intent, default_intent, &
            suppress_intent)
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
        logical :: has_reference_intent
        logical :: conflict_detected
        character(len=16) :: reference_intent
        character(len=16) :: current_intent
        logical :: contains_procedure
        logical :: suppress_intent

        matched = .false.
        has_reference_intent = .false.
        conflict_detected = .false.
        reference_intent = ""
        contains_procedure = .false.

        do name_idx = 1, size(stmt%var_names)
            param_idx = metadata_find_param(metadata, stmt%var_names(name_idx))
            if (param_idx <= 0) cycle

            current_intent = trim(metadata%intent(param_idx))
            if (len_trim(current_intent) > 0) then
                if (.not. has_reference_intent) then
                    has_reference_intent = .true.
                    reference_intent = current_intent
                else if (trim(current_intent) /= trim(reference_intent)) then
                    conflict_detected = .true.
                end if
            end if

            call sync_process_single_declaration(stmt, decl_index, param_idx, &
                metadata, &
                default_intent, stmt_intent, &
                suppress_intent)
            contains_procedure = contains_procedure .or. suppress_intent
            matched = .true.
        end do

        if (.not. matched) return
        call apply_type_standardization_to_stmt(stmt, standardize_types)

        if (contains_procedure) then
            call sync_finalize_intent(stmt, stmt_intent, default_intent, .true.)
        else if (conflict_detected) then
            call sync_finalize_intent(stmt, stmt_intent, default_intent, .true.)
        else
            call sync_finalize_intent(stmt, stmt_intent, default_intent)
        end if
    end subroutine sync_handle_multi_declaration

    subroutine sync_process_single_declaration(stmt, decl_index, param_idx, &
            metadata, &
            default_intent, stmt_intent, &
            suppress_intent)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        integer, intent(in) :: decl_index
        integer, intent(in) :: param_idx
        type(param_metadata_t), intent(inout) :: metadata
        character(len=*), intent(in) :: default_intent
        character(len=*), intent(inout) :: stmt_intent
        logical, intent(out) :: suppress_intent

        suppress_intent = metadata%is_procedure(param_idx)
        if (suppress_intent) then
            metadata%intent(param_idx) = ""
            stmt_intent = ""
            if (allocated(stmt%intent)) deallocate (stmt%intent)
            stmt%has_intent = .false.
        else
            call sync_update_intent(metadata%intent(param_idx), stmt_intent, &
                default_intent)
        end if
        if (metadata%optional(param_idx)) stmt%is_optional = .true.
        metadata%found(param_idx) = decl_index

        if (allocated(stmt%type_name)) then
            if (.not. is_type_variable_str(stmt%type_name)) then
                metadata%type_name(param_idx) = trim(stmt%type_name)
                metadata%type_inferred(param_idx) = .false.
            end if
        end if
        if (.not. metadata%is_procedure(param_idx)) then
            if (allocated(stmt%type_name)) then
                if (is_procedure_type(stmt%type_name)) then
                    metadata%is_procedure(param_idx) = .true.
                    suppress_intent = .true.
                    metadata%intent(param_idx) = ""
                    stmt_intent = ""
                    if (allocated(stmt%intent)) deallocate (stmt%intent)
                    stmt%has_intent = .false.
                end if
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

        if (metadata%is_procedure(param_idx)) then
            metadata%is_array(param_idx) = .false.
            metadata%rank(param_idx) = 0
            stmt%is_array = .false.
            if (allocated(stmt%dimension_indices)) then
                deallocate (stmt%dimension_indices)
            end if
            return
        end if

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

    subroutine sync_finalize_intent(stmt, stmt_intent, default_intent, &
            skip_intent)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: stmt
        character(len=*), intent(inout) :: stmt_intent
        character(len=*), intent(in) :: default_intent
        logical, intent(in), optional :: skip_intent
        character(len=16) :: final_intent
        logical :: suppress

        suppress = .false.
        if (present(skip_intent)) suppress = skip_intent
        if (suppress) then
            stmt_intent = ""
            if (allocated(stmt%intent)) deallocate (stmt%intent)
            stmt%has_intent = .false.
            return
        end if

        if (is_procedure_type(stmt%type_name)) then
            stmt_intent = ""
            if (allocated(stmt%intent)) deallocate (stmt%intent)
            stmt%has_intent = .false.
            return
        end if

        final_intent = trim(stmt_intent)
        if (len_trim(final_intent) == 0) final_intent = trim(default_intent)
        if (len_trim(final_intent) == 0) then
            if (allocated(stmt%intent)) deallocate (stmt%intent)
            stmt%has_intent = .false.
            return
        end if

        stmt%intent = final_intent
        stmt%has_intent = .true.
    end subroutine sync_finalize_intent

    integer function metadata_find_param(metadata, name) result(index)
        type(param_metadata_t), intent(in) :: metadata
        character(len=*), intent(in) :: name
        integer :: k
        character(len=64) :: normalized_target, normalized_candidate

        index = 0
        if (.not. allocated(metadata%names)) return

        normalized_target = to_lower(trim(name))

        do k = 1, size(metadata%names)
            normalized_candidate = to_lower(trim(metadata%names(k)))
            if (trim(normalized_candidate) == trim(normalized_target)) then
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

    subroutine reset_declaration_node(decl)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(out) :: decl

        decl%type_name = ""
        decl%has_kind = .false.
        decl%kind_value = 0
        decl%is_array = .false.
        decl%is_allocatable = .false.
        decl%is_multi_declaration = .false.
        decl%is_unsigned = .false.
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

    subroutine init_param_metadata(metadata, n_params)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: n_params

        if (n_params <= 0) return
        if (allocated(metadata%names)) deallocate (metadata%names)
        if (allocated(metadata%found)) deallocate (metadata%found)
        if (allocated(metadata%optional)) deallocate (metadata%optional)
        if (allocated(metadata%intent)) deallocate (metadata%intent)
        if (allocated(metadata%type_name)) deallocate (metadata%type_name)
        if (allocated(metadata%is_unsigned)) deallocate (metadata%is_unsigned)
        if (allocated(metadata%has_kind)) deallocate (metadata%has_kind)
        if (allocated(metadata%kind_value)) deallocate (metadata%kind_value)
        if (allocated(metadata%is_array)) deallocate (metadata%is_array)
        if (allocated(metadata%is_allocatable)) deallocate &
            (metadata%is_allocatable)
        if (allocated(metadata%type_inferred)) deallocate (metadata%type_inferred)
        if (allocated(metadata%rank)) deallocate (metadata%rank)
        if (allocated(metadata%is_procedure)) deallocate (metadata%is_procedure)

        allocate (character(len=64) :: metadata%names(n_params))
        allocate (metadata%found(n_params))
        allocate (metadata%optional(n_params))
        allocate (character(len=8) :: metadata%intent(n_params))
        allocate (character(len=64) :: metadata%type_name(n_params))
        allocate (metadata%is_unsigned(n_params))
        allocate (metadata%has_kind(n_params))
        allocate (metadata%kind_value(n_params))
        allocate (metadata%is_array(n_params))
        allocate (metadata%is_allocatable(n_params))
        allocate (metadata%type_inferred(n_params))
        allocate (metadata%rank(n_params))
        allocate (metadata%is_procedure(n_params))

        metadata%names = ""
        metadata%found = 0
        metadata%optional = .false.
        metadata%intent = ""
        metadata%type_name = ""
        metadata%is_unsigned = .false.
        metadata%has_kind = .false.
        metadata%kind_value = 0
        metadata%is_array = .false.
        metadata%is_allocatable = .false.
        metadata%type_inferred = .true.
        metadata%rank = 0
        metadata%is_procedure = .false.
    end subroutine init_param_metadata

    logical function node_exists(arena, index) result(has_node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index

        has_node = arena%has_node_at(index)
    end function node_exists

    subroutine fill_parameter_declaration(decl, metadata, idx, type_std_enabled, &
            inferred_local)
        use ast_nodes_data, only: declaration_node
        type(declaration_node), intent(inout) :: decl
        type(param_metadata_t), intent(in) :: metadata
        integer, intent(in) :: idx
        logical, intent(in) :: type_std_enabled
        logical, intent(inout) :: inferred_local
        character(len=32) :: inferred_type
        logical :: is_proc_param

        is_proc_param = metadata%is_procedure(idx)

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

        if (decl%type_name == "unsigned_integer") then
            decl%type_name = "integer"
            decl%is_unsigned = .true.
        else if (decl%type_name == "integer") then
            decl%is_unsigned = metadata%is_unsigned(idx)
        else
            decl%is_unsigned = .false.
        end if

        if (decl%type_name == "real" .and. type_std_enabled .and. &
            inferred_local) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        if (is_proc_param) then
            decl%is_array = .false.
            decl%is_allocatable = .false.
        else
            decl%is_array = metadata%is_array(idx)
            decl%is_allocatable = metadata%is_allocatable(idx)
        end if
        decl%var_name = metadata%names(idx)
        decl%is_parameter = .true.
        if (.not. is_proc_param) then
            if (decl%is_array) call ensure_deferred_shape(decl, metadata%rank(idx))
        end if
    end subroutine fill_parameter_declaration

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
                type_name = "real" ! Default to real
            end select
        else
            type_name = "real" ! Default fallback
        end if

    end subroutine infer_parameter_type
    pure integer function get_standardizer_input_mode()
        get_standardizer_input_mode = global_input_mode
    end function get_standardizer_input_mode

    subroutine set_standardizer_input_mode(mode)
        integer, intent(in) :: mode
        global_input_mode = mode
    end subroutine set_standardizer_input_mode

end module standardizer_parameter
