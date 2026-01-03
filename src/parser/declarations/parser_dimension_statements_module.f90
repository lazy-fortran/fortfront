module parser_dimension_statements_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        & TK_WHITESPACE, TK_COMMENT, TK_NEWLINE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, module_node
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use parser_declaration_attributes_module, only: parse_array_dimensions
    use parser_type_hooks_module, only: register_type_annotation, &
        & update_type_annotation_entry
    implicit none
    private

    integer, parameter :: CONTAINER_PROGRAM = 1
    integer, parameter :: CONTAINER_FUNCTION = 2
    integer, parameter :: CONTAINER_SUBROUTINE = 3
    integer, parameter :: CONTAINER_MODULE = 4

    public :: parse_dimension_statement

contains

    logical function parse_dimension_statement(parser, arena) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: var_name
        character(len=:), allocatable :: lowered_keyword
        integer, allocatable :: dimension_indices(:)
        logical :: applied

        success = .false.
        if (parser%is_at_end()) return

        call skip_trivia(parser)

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_keyword = to_lower(token%text)
                if (trim(lowered_keyword) == "dimension") then
                    token = parser%consume()
                    call skip_trivia(parser)
                end if
            end if
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
                call skip_trivia(parser)
            end if
        end if

        do
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                call skip_trivia(parser)
                cycle
            end if

            if (.not. token_is_identifier(token)) exit
            token = parser%consume()
            var_name = adjustl(trim(token%text))

            call skip_trivia(parser)
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= "(") exit
            token = parser%consume()

            call parse_array_dimensions(parser, arena, dimension_indices)
            applied = apply_dimension_to_variable(arena, var_name, dimension_indices)
            if (allocated(dimension_indices)) deallocate (dimension_indices)
            success = success .or. applied

            call skip_trivia(parser)
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (.not. (token%kind == TK_OPERATOR .and. token%text == ",")) exit
            token = parser%consume()
            call skip_trivia(parser)
        end do
    end function parse_dimension_statement

    subroutine skip_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_trivia

    logical function token_is_identifier(token) result(is_ident)
        type(token_t), intent(in) :: token
        is_ident = (token%kind == TK_IDENTIFIER)
    end function token_is_identifier

    logical function apply_dimension_to_variable(arena, name, dimension_indices) &
        result(applied)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension_indices(:)
        character(len=:), allocatable :: target
        integer :: idx

        applied = .false.
        target = adjustl(trim(name))

        do idx = arena%size, 1, -1
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    if (apply_dimension_multi(arena, idx, decl, target, &
                                              dimension_indices)) then
                        applied = .true.
                        return
                    end if
                end if
                if (trim(decl%var_name) == target) then
                    call apply_dimension_single(arena, idx, decl, target, &
                                                dimension_indices)
                    applied = .true.
                    return
                end if
            end select
        end do
    end function apply_dimension_to_variable

    subroutine apply_dimension_single(arena, decl_index, decl, target, &
                                      dimension_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        type(declaration_node), intent(inout) :: decl
        character(len=*), intent(in) :: target
        integer, intent(in) :: dimension_indices(:)

        call set_declaration_dimensions(decl, dimension_indices)
        decl%var_name = target
        decl%is_multi_declaration = .false.
        if (allocated(decl%var_names)) deallocate (decl%var_names)
        arena%entries(decl_index)%node = decl
        call update_type_annotation_entry(decl_index, [target], &
                                          dimension_indices=dimension_indices)
    end subroutine apply_dimension_single

    logical function apply_dimension_multi(arena, decl_index, decl, target, &
                                           dimension_indices) result(updated)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: decl_index
        type(declaration_node), intent(inout) :: decl
        character(len=*), intent(in) :: target
        integer, intent(in) :: dimension_indices(:)
        type(declaration_node) :: original_decl
        character(len=:), allocatable :: other_names(:)
        integer :: container_idx, container_pos, container_kind
        type(declaration_node) :: remaining_decl
        integer :: new_index

        updated = .false.
        original_decl = decl

        if (.not. gather_remaining_variables(original_decl, target, &
                                             other_names)) return
        if (.not. find_container_for_declaration(arena, decl_index, container_idx, &
                                                 container_pos, container_kind)) then
            call release_name_array(other_names)
            return
        end if

        call convert_decl_to_single(decl, target, dimension_indices)
        arena%entries(decl_index)%node = decl
        call update_type_annotation_entry(decl_index, [target], &
                                          dimension_indices=dimension_indices)

        if (size(other_names) > 0) then
            call build_remaining_declaration(original_decl, other_names, &
                                             remaining_decl)
            call arena%push(remaining_decl, "declaration", container_idx)
            new_index = arena%size
            call insert_declaration_after(arena, container_idx, container_kind, &
                                          container_pos, new_index)
            call register_remaining_annotation(new_index, remaining_decl, &
                                               original_decl)
        end if

        call release_name_array(other_names)
        updated = .true.
    end function apply_dimension_multi

    logical function gather_remaining_variables(decl, target, names) result(found)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: target
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: i, count, max_len
        logical :: matched

        if (.not. decl%is_multi_declaration) then
            allocate (character(len=1) :: names(0))
            found = .false.
            return
        end if

        if (.not. allocated(decl%var_names)) then
            allocate (character(len=1) :: names(0))
            found = .false.
            return
        end if

        count = 0
        max_len = 1
        matched = .false.
        do i = 1, size(decl%var_names)
            if (trim(decl%var_names(i)) == target) then
                matched = .true.
            else
                count = count + 1
                max_len = max(max_len, len_trim(decl%var_names(i)))
            end if
        end do

        if (.not. matched) then
            allocate (character(len=1) :: names(0))
            found = .false.
            return
        end if

        if (count == 0) then
            allocate (character(len=1) :: names(0))
            found = .true.
            return
        end if

        allocate (character(len=max_len) :: names(count))
        count = 0
        do i = 1, size(decl%var_names)
            if (trim(decl%var_names(i)) == target) cycle
            count = count + 1
            names(count) = adjustl(trim(decl%var_names(i)))
        end do
        found = .true.
    end function gather_remaining_variables

    subroutine release_name_array(names)
        character(len=:), allocatable, intent(inout) :: names(:)
        if (allocated(names)) then
            block
                character(len=:), allocatable :: temp(:)
                call move_alloc(names, temp)
            end block
        end if
    end subroutine release_name_array

    subroutine convert_decl_to_single(decl, target, dimension_indices)
        type(declaration_node), intent(inout) :: decl
        character(len=*), intent(in) :: target
        integer, intent(in) :: dimension_indices(:)

        call set_declaration_dimensions(decl, dimension_indices)
        decl%var_name = target
        decl%is_multi_declaration = .false.
        if (allocated(decl%var_names)) deallocate (decl%var_names)
    end subroutine convert_decl_to_single

    subroutine build_remaining_declaration(original, names, remaining)
        type(declaration_node), intent(in) :: original
        character(len=*), intent(in) :: names(:)
        type(declaration_node), intent(out) :: remaining

        remaining = original
        remaining%var_name = ""
        if (size(names) > 0) remaining%var_name = names(1)

        if (allocated(remaining%var_names)) deallocate (remaining%var_names)

        if (size(names) > 1) then
            remaining%is_multi_declaration = .true.
            call allocate_name_array(remaining%var_names, names)
        else
            remaining%is_multi_declaration = .false.
        end if

        if (.not. remaining%is_multi_declaration) then
            if (allocated(remaining%var_names)) deallocate (remaining%var_names)
        end if
    end subroutine build_remaining_declaration

    subroutine allocate_name_array(dest, source)
        character(len=:), allocatable, intent(out) :: dest(:)
        character(len=*), intent(in) :: source(:)
        integer :: i, max_len

        max_len = 1
        do i = 1, size(source)
            max_len = max(max_len, len_trim(source(i)))
        end do
        allocate (character(len=max_len) :: dest(size(source)))
        do i = 1, size(source)
            dest(i) = adjustl(trim(source(i)))
        end do
    end subroutine allocate_name_array

    subroutine register_remaining_annotation(index, decl, template)
        integer, intent(in) :: index
        type(declaration_node), intent(in) :: decl
        type(declaration_node), intent(in) :: template

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            call register_type_annotation(index, template%type_name, decl%var_names, &
                                          has_kind=template%has_kind, &
                                          kind_value=template%kind_value, &
                                          is_unsigned=template%is_unsigned, &
                                          is_parameter=template%is_parameter, &
                                          is_allocatable=template%is_allocatable, &
                                          is_pointer=template%is_pointer)
        else
            call register_type_annotation(index, template%type_name, &
                                          [decl%var_name], &
                                          has_kind=template%has_kind, &
                                          kind_value=template%kind_value, &
                                          is_unsigned=template%is_unsigned, &
                                          is_parameter=template%is_parameter, &
                                          is_allocatable=template%is_allocatable, &
                                          is_pointer=template%is_pointer)
        end if
    end subroutine register_remaining_annotation

    subroutine set_declaration_dimensions(decl, dimension_indices)
        type(declaration_node), intent(inout) :: decl
        integer, intent(in) :: dimension_indices(:)
        integer :: n

        decl%is_array = .true.
        if (allocated(decl%dimension_indices)) then
            deallocate (decl%dimension_indices)
        end if
        if (size(dimension_indices) > 0) then
            allocate (decl%dimension_indices(size(dimension_indices)))
            decl%dimension_indices = dimension_indices
        else
            allocate (decl%dimension_indices(0))
        end if

        ! Only set allocatable for deferred dimensions if not a parameter constant
        ! (parameters cannot be allocatable - fixes issue #1810)
        decl%is_allocatable = .false.
        if (.not. decl%is_parameter) then
            do n = 1, size(dimension_indices)
                if (dimension_indices(n) == 0) then
                    decl%is_allocatable = .true.
                    exit
                end if
            end do
        end if
    end subroutine set_declaration_dimensions

    logical function find_container_for_declaration(arena, decl_index, container_idx, &
                                                    position, container_kind) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        integer, intent(out) :: container_idx
        integer, intent(out) :: position
        integer, intent(out) :: container_kind
        integer :: i

        found = .false.
        container_idx = 0
        position = 0
        container_kind = 0

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    if (find_index(node%body_indices, decl_index, position)) then
                        container_idx = i
                        container_kind = CONTAINER_PROGRAM
                        found = .true.
                        return
                    end if
                end if
            type is (function_def_node)
                if (allocated(node%body_indices)) then
                    if (find_index(node%body_indices, decl_index, position)) then
                        container_idx = i
                        container_kind = CONTAINER_FUNCTION
                        found = .true.
                        return
                    end if
                end if
            type is (subroutine_def_node)
                if (allocated(node%body_indices)) then
                    if (find_index(node%body_indices, decl_index, position)) then
                        container_idx = i
                        container_kind = CONTAINER_SUBROUTINE
                        found = .true.
                        return
                    end if
                end if
            type is (module_node)
                if (allocated(node%declaration_indices)) then
                    if (find_index(node%declaration_indices, decl_index, &
                                   position)) then
                        container_idx = i
                        container_kind = CONTAINER_MODULE
                        found = .true.
                        return
                    end if
                end if
            end select
        end do
    end function find_container_for_declaration

    logical function find_index(values, target, position) result(found)
        integer, intent(in) :: values(:)
        integer, intent(in) :: target
        integer, intent(out) :: position
        integer :: i

        found = .false.
        position = 0
        do i = 1, size(values)
            if (values(i) == target) then
                position = i
                found = .true.
                return
            end if
        end do
    end function find_index

    subroutine insert_declaration_after(arena, container_idx, container_kind, &
                                        position, new_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: container_idx, container_kind, position, new_index

        select case (container_kind)
        case (CONTAINER_PROGRAM)
            select type (node => arena%entries(container_idx)%node)
            type is (program_node)
                call insert_after(node%body_indices, position, new_index)
            end select
        case (CONTAINER_FUNCTION)
            select type (node => arena%entries(container_idx)%node)
            type is (function_def_node)
                call insert_after(node%body_indices, position, new_index)
            end select
        case (CONTAINER_SUBROUTINE)
            select type (node => arena%entries(container_idx)%node)
            type is (subroutine_def_node)
                call insert_after(node%body_indices, position, new_index)
            end select
        case (CONTAINER_MODULE)
            select type (node => arena%entries(container_idx)%node)
            type is (module_node)
                call insert_after(node%declaration_indices, position, new_index)
            end select
        end select
    end subroutine insert_declaration_after

    subroutine insert_after(values, position, new_value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: position, new_value
        integer, allocatable :: temp(:)
        integer :: n

        if (.not. allocated(values)) then
            allocate (values(1))
            values(1) = new_value
            return
        end if

        n = size(values)
        allocate (temp(n + 1))
        if (position >= n) then
            temp(1:n) = values
            temp(n + 1) = new_value
        else if (position <= 0) then
            temp(1) = new_value
            temp(2:) = values
        else
            temp(1:position) = values(1:position)
            temp(position + 1) = new_value
            temp(position + 2:) = values(position + 1:)
        end if
        call move_alloc(temp, values)
    end subroutine insert_after

end module parser_dimension_statements_module
