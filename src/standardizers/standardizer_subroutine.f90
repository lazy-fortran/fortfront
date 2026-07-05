module standardizer_subroutine
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use ast_nodes_procedure, only: subroutine_def_node
    use ast_nodes_data, only: INTENT_IN, INTENT_INOUT, INTENT_OUT
    use standardizer_parameter, only: fill_parameter_declaration
    use standardizer_parameter, only: get_standardizer_type_standardization
    use standardizer_parameter, only: init_param_metadata
    use standardizer_parameter, only: param_metadata_t
    use standardizer_parameter, only: reset_declaration_node
    use standardizer_parameter, only: synchronize_parameter_declarations
    use standardizer_parameter, only: get_standardizer_input_mode
    use standardizer_declarations_insertion, only: &
        insert_procedure_variable_declarations
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    use standardizer_function_param_scanner, only: &
        analyze_subroutine_parameter_usage
    use standardizer_subroutine_intent, only: infer_subroutine_parameter_intents
    implicit none
    private
    public :: standardize_subroutine_def
    public :: standardize_subroutine_parameters
contains

    subroutine standardize_subroutine_def(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i
        ! Local snapshot: see standardize_function_def for the rationale.
        ! push_implicit_statement may cause move_alloc on arena%entries, leaving
        ! any select-type alias of arena%entries(sub_index)%node dangling.
        type(subroutine_def_node) :: local_def

        local_def = sub_def

        ! Add implicit none at the beginning of subroutine body.
        ! push_implicit_statement may reallocate arena%entries; local_def is
        ! already a safe copy so nothing here can corrupt it.
        if (allocated(local_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                line=1, column=1, &
                parent_index=sub_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(local_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(local_def%body_indices)
                new_body_indices(i + 1) = local_def%body_indices(i)
            end do
            local_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_subroutine_parameters(arena, local_def, sub_index)

        ! Synthesize declarations for implicitly typed locals (lazy/infer mode),
        ! mirroring the implicit main program path.
        if (get_standardizer_input_mode() /= INPUT_MODE_STANDARD) then
            call insert_procedure_variable_declarations(arena, &
                local_def%body_indices, sub_index)
        end if

        ! Write back to arena and propagate to caller.
        arena%entries(sub_index)%node = local_def
        sub_def = local_def
    end subroutine standardize_subroutine_def

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
            call analyze_subroutine_parameter_usage(arena, sub_def, metadata)
            call infer_subroutine_parameter_intents(arena, sub_def%body_indices, &
                metadata)
            call synchronize_parameter_declarations( &
                arena, sub_def%body_indices, metadata, "", type_std_enabled)
        else
            call analyze_subroutine_parameter_usage(arena, sub_def, metadata)
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
            sub_index, &
            metadata, &
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
        character(len=8) :: intent_value
        logical :: skip_intent
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
            call try_copy_dimension_from_peer(arena, metadata, i, decl)
            skip_intent = metadata%is_procedure(i)
            if (skip_intent) then
                if (allocated(decl%intent)) deallocate (decl%intent)
                decl%has_intent = .false.
            else
                intent_value = choose_subroutine_intent(metadata%intent(i))
                if (len_trim(intent_value) > 0) then
                    decl%intent = intent_value
                    decl%has_intent = .true.
                else
                    decl%has_intent = .false.
                end if
            end if
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", sub_index)
            new_count = new_count + 1
            new_indices(new_count) = arena%size
            metadata%found(i) = arena%size
        end do

        call combine_existing_and_new_body_subroutine(sub_def, existing, &
            new_indices, new_count)

    contains

        subroutine try_copy_dimension_from_peer(arena_l, metadata_l, target_idx, &
                target_decl)
            use ast_nodes_data, only: declaration_node
            type(ast_arena_t), intent(in) :: arena_l
            type(param_metadata_t), intent(in) :: metadata_l
            integer, intent(in) :: target_idx
            type(declaration_node), intent(inout) :: target_decl
            integer :: peer_idx
            integer :: decl_index

            if (.not. target_decl%is_array) return
            if (.not. allocated(target_decl%dimension_indices)) return
            if (.not. all(target_decl%dimension_indices == 0)) return

            do peer_idx = 1, size(metadata_l%names)
                if (peer_idx == target_idx) cycle
                if (metadata_l%found(peer_idx) <= 0) cycle
                if (.not. metadata_l%is_array(peer_idx)) cycle
                if (metadata_l%rank(peer_idx) /= metadata_l%rank(target_idx)) cycle

                decl_index = metadata_l%found(peer_idx)
                if (decl_index <= 0 .or. decl_index > arena_l%size) cycle
                if (.not. allocated(arena_l%entries(decl_index)%node)) cycle

                select type (peer_decl => arena_l%entries(decl_index)%node)
                    type is (declaration_node)
                    if (.not. peer_decl%is_array) cycle
                    if (.not. allocated(peer_decl%dimension_indices)) cycle
                    if (all(peer_decl%dimension_indices == 0)) cycle
                    call copy_dimension_indices(target_decl, &
                        peer_decl%dimension_indices)
                    return
                class default
                end select
            end do
        end subroutine try_copy_dimension_from_peer

        subroutine copy_dimension_indices(target_decl, source_dims)
            use ast_nodes_data, only: declaration_node
            type(declaration_node), intent(inout) :: target_decl
            integer, intent(in) :: source_dims(:)

            if (allocated(target_decl%dimension_indices)) then
                deallocate (target_decl%dimension_indices)
            end if
            allocate (target_decl%dimension_indices(size(source_dims)))
            target_decl%dimension_indices = source_dims
        end subroutine copy_dimension_indices
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
                if (allocated(sub_def%body_indices)) deallocate &
                    (sub_def%body_indices)
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

end module standardizer_subroutine
