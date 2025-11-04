module standardizer_function_result_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node
    use lexer_core, only: to_lower
    use semantic_validation_utils, only: rename_identifier_in_arena
    use standardizer_parameter, only: infer_parameter_type, is_type_variable_str, &
                                      reset_declaration_node
    use type_string_utils, only: is_character_type_string
    implicit none
    private
    public :: determine_preferred_result_name
    public :: apply_result_variable
    public :: sync_result_declaration
contains

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
                    if (len_trim(fallback_name) == 0) fallback_name = &
                        trim(target_name)
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
