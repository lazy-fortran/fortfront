module standardizer_function_parameters
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_data, only: INTENT_IN, INTENT_INOUT, INTENT_OUT
    use ast_nodes_procedure, only: function_def_node
    use type_system_unified, only: TINT
    use standardizer_function_param_scanner, only: analyze_parameter_usage
    use standardizer_function_parameter_builders, only: &
        add_missing_parameter_declarations_ext, &
        rebuild_parameter_declarations
    use standardizer_parameter, only: get_standardizer_type_standardization
    use standardizer_parameter, only: infer_parameter_type
    use standardizer_parameter, only: init_param_metadata
    use standardizer_parameter, only: is_type_variable_str, is_procedure_type
    use standardizer_parameter, only: node_exists
    use standardizer_parameter, only: param_metadata_t
    use standardizer_parameter, only: synchronize_parameter_declarations
    use standardizer_parameter, only: get_standardizer_input_mode
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    implicit none
    private
    public :: standardize_function_parameters
contains

    subroutine standardize_function_parameters(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(param_metadata_t) :: metadata
        logical :: standardizer_type_standardization_enabled
        logical :: requires_intent_in_flag
        logical :: is_lazy_fortran
        integer :: n_params
        integer :: current_input_mode
        character(len=:), allocatable :: default_intent

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)
        call init_param_metadata(metadata, n_params)

        ! Check input mode: lazy Fortran vs standard Fortran
        current_input_mode = get_standardizer_input_mode()
        is_lazy_fortran = (current_input_mode == INPUT_MODE_LAZY)

        requires_intent_in_flag = needs_intent_in(func_def)
        if (requires_intent_in_flag) metadata%intent = "in"

        call populate_param_metadata(arena, func_def, metadata)
        call analyze_parameter_usage(arena, func_def, metadata)
        call finalize_param_types(metadata)

        ! Determine default intent based on input mode and function type
        ! ISO/IEC 1539-1:2018 Section 15.5.2.3:
        ! - Pure/elemental functions REQUIRE intent(in) for all dummy arguments
        ! - Standard Fortran: Normal functions do NOT require intent (preserve as-is)
        ! - Lazy Fortran: Add intent(in) for type inference purposes
        if (requires_intent_in_flag) then
            default_intent = "in"
        else if (is_lazy_fortran) then
            default_intent = "in"  ! Lazy Fortran: add intent for inference
        else
            default_intent = ""    ! Standard Fortran: no default intent
        end if

        if (allocated(func_def%body_indices)) then
            call synchronize_parameter_declarations( &
                arena, func_def%body_indices, metadata, default_intent, &
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
        metadata%is_unsigned(slot) = param%inferred_type%kind == TINT .and. &
                                     param%inferred_type%is_unsigned
        if (metadata%is_unsigned(slot)) then
            inferred_text = "integer"
        else
            inferred_text = param%inferred_type%to_string()
        end if
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
        has_explicit_type = allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0
        if (has_explicit_type) then
            explicit_type = trim(param%type_name)
            metadata%is_unsigned(slot) = .false.
        else
            explicit_type = ""
            metadata%is_unsigned(slot) = param%inferred_type%kind == TINT .and. &
                                         param%inferred_type%is_unsigned
        end if
        if (metadata%is_unsigned(slot)) then
            inferred_text = "integer"
        else
            inferred_text = param%inferred_type%to_string()
        end if
        call apply_function_type(metadata, slot, has_explicit_type, &
                                 explicit_type, &
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
        has_explicit_type = allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0
        if (has_explicit_type) then
            explicit_type = trim(param%type_name)
            metadata%is_unsigned(slot) = param%is_unsigned
        else
            explicit_type = ""
            metadata%is_unsigned(slot) = param%inferred_type%kind == TINT .and. &
                                         param%inferred_type%is_unsigned
        end if
        if (metadata%is_unsigned(slot)) then
            inferred_text = "integer"
        else
            inferred_text = param%inferred_type%to_string()
        end if
        call apply_function_type(metadata, slot, has_explicit_type, &
                                 explicit_type, &
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
        logical :: is_proc_type

        if (type_present .and. len_trim(type_text) > 0 .and. &
            .not. is_type_variable_str(type_text)) then
            metadata%type_name(idx) = trim(type_text)
            metadata%type_inferred(idx) = .false.
        else if (inferred_present .and. len_trim(inferred_text) > 0 .and. &
                 .not. is_type_variable_str(inferred_text)) then
            metadata%type_name(idx) = trim(inferred_text)
            metadata%type_inferred(idx) = .false.
        end if

        is_proc_type = is_procedure_type(metadata%type_name(idx))
        metadata%is_procedure(idx) = is_proc_type
        if (is_proc_type) metadata%intent(idx) = ""

        if (is_proc_type) then
            metadata%has_kind(idx) = .false.
            metadata%kind_value(idx) = 0
        else
            metadata%has_kind(idx) = has_kind_flag
            if (has_kind_flag) then
                metadata%kind_value(idx) = kind_value
            else
                metadata%kind_value(idx) = 0
            end if
        end if

        if (is_proc_type) then
            metadata%is_array(idx) = .false.
            metadata%is_allocatable(idx) = .false.
            metadata%rank(idx) = 0
        else
            metadata%is_array(idx) = is_array_flag
            metadata%is_allocatable(idx) = is_alloc_flag
        end if
    end subroutine apply_function_type

end module standardizer_function_parameters
