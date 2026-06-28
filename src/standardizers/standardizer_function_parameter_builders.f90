module standardizer_function_parameter_builders
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_procedure, only: function_def_node
    use standardizer_parameter, only: fill_parameter_declaration
    use standardizer_parameter, only: node_exists
    use standardizer_parameter, only: param_metadata_t
    use standardizer_parameter, only: reset_declaration_node
    implicit none
    private
    public :: add_missing_parameter_declarations_ext
    public :: rebuild_parameter_declarations
contains

    subroutine add_missing_parameter_declarations_ext(arena, func_def, &
            func_index, &
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
        character(len=8) :: intent_value
        logical :: skip_intent

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
            skip_intent = metadata%is_procedure(i)
            if (skip_intent) then
                if (allocated(decl%intent)) deallocate (decl%intent)
                decl%has_intent = .false.
            else
                intent_value = choose_param_intent(metadata%intent(i))
                if (len_trim(intent_value) > 0) then
                    decl%intent = intent_value
                    decl%has_intent = .true.
                else
                    decl%has_intent = .false.
                end if
            end if
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", func_index)
            new_count = new_count + 1
            new_indices(new_count) = arena%size
            metadata%found(i) = arena%size
        end do

        call combine_existing_and_new_body(func_def, existing, &
            new_indices, new_count)
    end subroutine add_missing_parameter_declarations_ext
    subroutine rebuild_parameter_declarations(arena, func_def, func_index, &
            metadata, &
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
        call build_replacement_parameter_declarations(arena, func_def, &
            func_index, &
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
    subroutine build_replacement_parameter_declarations(arena, func_def, &
            func_index, &
            metadata, &
            type_std_enabled, &
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
        character(len=8) :: intent_value
        logical :: skip_intent

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
            skip_intent = metadata%is_procedure(i)
            if (skip_intent) then
                if (allocated(decl%intent)) deallocate (decl%intent)
                decl%has_intent = .false.
            else
                intent_value = choose_param_intent(metadata%intent(i))
                if (len_trim(intent_value) == 0) intent_value = "in"
                decl%intent = intent_value
                decl%has_intent = .true.
            end if
            decl%is_optional = metadata%optional(i)
            call arena%push(decl, "declaration", func_index)
            new_indices(i) = arena%size
        end do
    end subroutine build_replacement_parameter_declarations
    character(len=8) function choose_param_intent(intent_value) &
            result(result_intent)
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

end module standardizer_function_parameter_builders
