module standardizer_function_result_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node
    use lexer_core, only: to_lower
    use semantic_validation_utils, only: rename_identifier_in_arena
    use standardizer_interface_utils, only: function_in_interface_block
    use standardizer_parameter, only: infer_parameter_type, is_type_variable_str, &
                                      reset_declaration_node
    use standardizer_declarations_array, only: set_array_properties_from_type
    use type_string_utils, only: is_character_type_string, mono_type_to_string
    implicit none
    private
    public :: determine_preferred_result_name
    public :: apply_result_variable
    public :: sync_result_declaration
contains

    subroutine determine_preferred_result_name(arena, func_def, func_index, &
                                               preferred_name)
        use ast_nodes_core, only: assignment_node, identifier_node
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        integer, intent(in) :: func_index
        character(len=*), intent(out) :: preferred_name
        character(len=64) :: fallback_name
        character(len=64) :: function_name
        character(len=64) :: target_name
        integer :: body_index
        integer :: name_pos
        integer :: target_index
        integer :: i
        logical :: in_interface_block

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
                    if (len_trim(fallback_name) == 0) fallback_name = &
                        trim(target_name)
                end select
            end select
        end do

        if (len_trim(preferred_name) == 0) preferred_name = trim(fallback_name)
        if (len_trim(preferred_name) == 0 .and. len_trim(function_name) > 0) then
            in_interface_block = function_in_interface_block(arena, func_index)

            if (.not. in_interface_block) then
                do i = 1, size(func_def%body_indices)
                    body_index = func_def%body_indices(i)
                    if (body_index <= 0 .or. body_index > arena%size) cycle
                    if (.not. allocated(arena%entries(body_index)%node)) cycle
                    select type (stmt => arena%entries(body_index)%node)
                    type is (declaration_node)
                        if (allocated(stmt%var_name)) then
                            if (trim(stmt%var_name) == trim(function_name)) then
                                preferred_name = trim(function_name)
                                return
                            end if
                        end if
                        if (stmt%is_multi_declaration .and. &
                            allocated(stmt%var_names)) then
                            do name_pos = 1, size(stmt%var_names)
                                if (trim(stmt%var_names(name_pos)) == &
                                    trim(function_name)) then
                                    preferred_name = trim(function_name)
                                    return
                                end if
                            end do
                        end if
                    end select
                end do
            end if
        end if

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
                    call rename_identifier_in_arena(arena, "result", &
                                                    trimmed_name, &
                                                    func_def%body_indices, &
                                                    func_index)
                end if
            end if
        end if

        if (len_trim(function_name) > 0) then
            if (allocated(func_def%result_variable)) then
                if (len_trim(func_def%result_variable) > 0) then
                    if (trim(func_def%result_variable) /= &
                        trim(function_name)) then
                        if (allocated(func_def%body_indices)) then
                            call rename_identifier_in_arena( &
                                arena, trim(function_name), &
                                trim(func_def%result_variable), &
                                func_def%body_indices, func_index)
                        end if
                    end if
                end if
            end if
        end if

    end subroutine apply_result_variable
    subroutine sync_result_declaration(arena, func_def, func_index, &
                                       type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical, intent(in) :: type_std_enabled
        logical :: has_decl
        logical :: decl_in_multi
        integer :: decl_index
        integer :: result_name_position
        type(declaration_node) :: existing_decl

        if (function_in_interface_block(arena, func_index)) return

        call find_result_declaration(arena, func_def, has_decl, decl_index, &
                                     existing_decl, decl_in_multi, &
                                     result_name_position)

        if (has_function_name_result(func_def)) then
            if (has_decl) then
                if (decl_in_multi) then
                    call detach_result_from_declaration(arena, func_def, &
                                                        func_index, decl_index, &
                                                        existing_decl, &
                                                        result_name_position)
                end if
                call remove_intent_from_declaration(arena, decl_index)
            end if
            call ensure_function_name_return_type(func_def, type_std_enabled)
            arena%entries(func_index)%node = func_def
            return
        end if

        if (has_decl) then
            if (decl_in_multi) then
                call detach_result_from_declaration(arena, func_def, func_index, &
                                                    decl_index, existing_decl, &
                                                    result_name_position)
            end if
            call update_return_type_from_existing(func_def, arena, func_index, &
                                                  existing_decl)
            call remove_intent_from_declaration(arena, decl_index)
            return
        end if

        call create_result_declaration(arena, func_def, func_index, &
                                       type_std_enabled)

    end subroutine sync_result_declaration
    subroutine find_result_declaration(arena, func_def, has_decl, decl_index, &
                                       existing_decl, in_multi, name_position)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        logical, intent(out) :: has_decl
        integer, intent(out) :: decl_index
        type(declaration_node), intent(out) :: existing_decl
        logical, intent(out) :: in_multi
        integer, intent(out) :: name_position
        integer :: i
        integer :: name_pos

        has_decl = .false.
        decl_index = 0
        in_multi = .false.
        name_position = 0
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
                    in_multi = stmt%is_multi_declaration .and. &
                               allocated(stmt%var_names) .and. &
                               size(stmt%var_names) > 1
                    if (in_multi) name_position = 1
                    return
                end if
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do name_pos = 1, size(stmt%var_names)
                        if (trim(stmt%var_names(name_pos)) == &
                            trim(func_def%result_variable)) then
                            has_decl = .true.
                            existing_decl = stmt
                            in_multi = (size(stmt%var_names) > 1)
                            name_position = name_pos
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
                if (existing_decl%has_kind .and. existing_decl%kind_value &
                    > 0 .and. &
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

        if (decl%type_name == "real" .and. type_std_enabled .and. &
            result_inferred) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        call ensure_function_return_type(func_def, decl)

    end subroutine ensure_function_name_return_type
    subroutine create_result_declaration(arena, func_def, func_index, &
                                         type_std_enabled)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        logical, intent(in) :: type_std_enabled
        type(declaration_node) :: decl
        logical :: result_inferred
        integer :: i, body_idx

        ! Safety check: if a declaration for this result variable already exists
        ! in the body with a valid type, do not create a new one
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                body_idx = func_def%body_indices(i)
                if (body_idx <= 0 .or. body_idx > arena%size) cycle
                if (.not. allocated(arena%entries(body_idx)%node)) cycle
                select type (stmt => arena%entries(body_idx)%node)
                type is (declaration_node)
                    if (allocated(stmt%var_name)) then
                        if (trim(stmt%var_name) == &
                            trim(func_def%result_variable)) then
                            if (len_trim(stmt%type_name) > 0 .and. &
                                trim(stmt%type_name) /= "real" .and. &
                                trim(stmt%type_name) /= "function") then
                                ! Declaration already exists with non-default type
                                return
                            end if
                        end if
                    end if
                end select
            end do
        end if

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

        if (decl%type_name == "real" .and. type_std_enabled .and. &
            result_inferred) then
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

        if (func_def%inferred_type%kind > 0) then
            decl%inferred_type = func_def%inferred_type
            call decl%inferred_type%sync_from_arena()
            call set_array_properties_from_type(arena, decl%var_name, func_index, &
                                                decl)
            if (decl%inferred_type%alloc_info%is_allocatable) then
                decl%is_allocatable = .true.
            end if
            ! Only use inferred type for type_name if not already set by return_type
            if (decl%inferred_type%kind > 0 .and. len_trim(decl%type_name) == 0) then
                decl%type_name = mono_type_to_string(decl%inferred_type, &
                                                     include_shape=.false., &
                                                    standardize_real=type_std_enabled, &
                                                     fallback=decl%type_name)
            end if
        end if

        call arena%push(decl, "declaration", func_index)
        call insert_result_declaration_into_body(func_def, arena%size)
        arena%entries(func_index)%node = func_def

    end subroutine create_result_declaration
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

    subroutine detach_result_from_declaration(arena, func_def, func_index, &
                                              decl_index, existing_decl, &
                                              name_position)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer, intent(inout) :: decl_index
        type(declaration_node), intent(inout) :: existing_decl
        integer, intent(in) :: name_position
        type(declaration_node) :: result_decl
        integer :: new_decl_index

        call remove_result_name_from_declaration(arena, decl_index, &
                                                 name_position)

        result_decl = existing_decl
        result_decl%is_multi_declaration = .false.
        result_decl%var_name = trim(func_def%result_variable)
        if (allocated(result_decl%var_names)) deallocate (result_decl%var_names)
        result_decl%intent = ""
        result_decl%has_intent = .false.
        result_decl%is_optional = .false.

        call arena%push(result_decl, "declaration", func_index)
        new_decl_index = arena%size
        call insert_body_index_after(func_def, decl_index, new_decl_index)
        decl_index = new_decl_index
        existing_decl = result_decl
    end subroutine detach_result_from_declaration

    subroutine remove_result_name_from_declaration(arena, decl_index, &
                                                   name_position)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        integer, intent(in) :: name_position
        integer :: i
        integer :: new_count
        integer :: length_hint
        character(len=:), allocatable :: temp_names(:)

        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return

        select type (decl => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. decl%is_multi_declaration) return
            if (.not. allocated(decl%var_names)) return
            if (name_position < 1 .or. name_position > size(decl%var_names)) return

            new_count = size(decl%var_names) - 1
            if (new_count <= 0) then
                decl%is_multi_declaration = .false.
                decl%var_name = ""
                deallocate (decl%var_names)
                arena%entries(decl_index)%node = decl
                return
            end if

            length_hint = len(decl%var_names(1))
            allocate (character(len=length_hint) :: temp_names(new_count))
            new_count = 0
            do i = 1, size(decl%var_names)
                if (i == name_position) cycle
                new_count = new_count + 1
                temp_names(new_count) = decl%var_names(i)
            end do
            call move_alloc(temp_names, decl%var_names)
            decl%is_multi_declaration = new_count > 1
            if (decl%is_multi_declaration) then
                decl%var_name = decl%var_names(1)
            else
                decl%var_name = decl%var_names(1)
                deallocate (decl%var_names)
            end if
            arena%entries(decl_index)%node = decl
        end select
    end subroutine remove_result_name_from_declaration

    subroutine insert_body_index_after(func_def, anchor_index, new_index)
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: anchor_index
        integer, intent(in) :: new_index
        integer, allocatable :: new_body(:)
        integer :: i, n
        logical :: inserted

        if (.not. allocated(func_def%body_indices)) then
            allocate (func_def%body_indices(1))
            func_def%body_indices(1) = new_index
            return
        end if

        n = size(func_def%body_indices)
        allocate (new_body(n + 1))
        inserted = .false.
        do i = 1, n
            new_body(i) = func_def%body_indices(i)
            if (.not. inserted .and. func_def%body_indices(i) == anchor_index) then
                new_body(i + 1) = new_index
                if (i < n) new_body(i + 2:n + 1) = func_def%body_indices(i + 1:n)
                inserted = .true.
                exit
            end if
        end do

        if (.not. inserted) then
            new_body(1:n) = func_def%body_indices
            new_body(n + 1) = new_index
        end if

        call move_alloc(new_body, func_def%body_indices)
    end subroutine insert_body_index_after

    subroutine remove_intent_from_declaration(arena, decl_index)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index

        if (decl_index <= 0 .or. decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return

        select type (decl => arena%entries(decl_index)%node)
        type is (declaration_node)
            if (.not. decl%has_intent) return
            decl%intent = ""
            decl%has_intent = .false.
            decl%is_optional = .false.
            arena%entries(decl_index)%node = decl
        end select
    end subroutine remove_intent_from_declaration
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

end module standardizer_function_result_utils
