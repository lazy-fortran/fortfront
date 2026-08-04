module parser_declarations_multi_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_declarations_core_module, only: skip_declaration_separator
    use parser_declarations_construction_module, only: handle_complex_initializer, &
        add_single_declaration, &
        emit_multi_declaration
    use parser_declarations_type_spec_support_module, only: type_specifier_t
    use parser_declarations_type_spec_module, only: parse_type_specifier
    use parser_declaration_attributes_module, only: parse_declaration_attributes, &
        parse_array_dimensions
    use parser_expressions_module, only: parse_comparison
    use declaration_attribute_utils, only: declaration_attribute_info_t
    implicit none
    private

    public :: parse_multi_declaration

contains

    function parse_multi_declaration(parser, arena) result(decl_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(type_specifier_t) :: type_spec
        type(declaration_attribute_info_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer, allocatable :: per_var_dims(:, :)
        logical, allocatable :: has_dims(:)
        integer, allocatable :: init_indices(:)
        integer :: var_count
        logical :: has_any_initializer
        integer :: idx_i, idx_val

        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            allocate (decl_indices(0))
            return
        end if

        call parse_declaration_attributes(parser, arena, attr_info)
        call skip_declaration_separator(parser)

        call initialize_multi_state(var_names, per_var_dims, has_dims, &
            init_indices)
        call collect_multi_variable_data(parser, arena, type_spec, var_names, &
            per_var_dims, has_dims, init_indices, &
            var_count, has_any_initializer)
        call finalize_multi_declaration( &
            arena, type_spec, attr_info, var_names, per_var_dims, has_dims, &
            init_indices, var_count, has_any_initializer, decl_indices)

        ! Set line numbers on created declaration nodes
        if (allocated(decl_indices)) then
            do idx_i = 1, size(decl_indices)
                idx_val = decl_indices(idx_i)
                if (idx_val > 0 .and. idx_val <= arena%size) then
                    if (arena%has_node_at(idx_val)) then
                        if (allocated(arena%entries(idx_val)%node)) then
                            arena%entries(idx_val)%node%line = type_spec%line
                            arena%entries(idx_val)%node%column = type_spec%column
                        end if
                    end if
                end if
            end do
        end if
    end function parse_multi_declaration

    subroutine initialize_multi_state(var_names, per_var_dims, has_dims, &
            init_indices)
        character(len=64), allocatable, intent(out) :: var_names(:)
        integer, allocatable, intent(out) :: per_var_dims(:, :)
        logical, allocatable, intent(out) :: has_dims(:)
        integer, allocatable, intent(out) :: init_indices(:)
        integer, parameter :: initial_capacity = 4
        integer, parameter :: max_dim_slots = 10

        allocate (var_names(initial_capacity))
        allocate (per_var_dims(initial_capacity, max_dim_slots))
        allocate (has_dims(initial_capacity))
        allocate (init_indices(initial_capacity))
        var_names = ""
        per_var_dims = 0
        has_dims = .false.
        init_indices = 0
    end subroutine initialize_multi_state

    subroutine collect_multi_variable_data(parser, arena, type_spec, var_names, &
            per_var_dims, has_dims, &
            init_indices, var_count, &
            has_any_initializer)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(out) :: var_count
        logical, intent(out) :: has_any_initializer
        integer :: capacity
        type(token_t) :: token

        var_count = 0
        has_any_initializer = .false.
        capacity = size(var_names)

        do while (.not. parser%is_at_end())
            token = parser%consume()
            ! Fortran reserves no words, so a declared entity may be spelled
            ! with one: `real(dp) :: distance, parameter, source(2)` is legal
            ! and appears in fortfem. Requiring an identifier here stopped the
            ! list at that name and left the rest of the line unconsumed, which
            ! surfaced much later as an unexpected trailing token.
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
                exit
            end if

            var_count = var_count + 1
            if (var_count > capacity) then
                call expand_multi_state(var_names, per_var_dims, has_dims, &
                    init_indices, capacity)
            end if

            var_names(var_count) = token%text
            has_dims(var_count) = .false.
            init_indices(var_count) = 0
            per_var_dims(var_count, :) = 0

            call parse_multi_variable_dimensions(parser, arena, per_var_dims, &
                has_dims, var_count)
            call parse_multi_variable_initializer(parser, arena, type_spec, &
                init_indices(var_count), &
                has_any_initializer)
            if (.not. continue_multi_variable(parser)) then
                exit
            end if
        end do
    end subroutine collect_multi_variable_data

    subroutine expand_multi_state(var_names, per_var_dims, has_dims, &
            init_indices, capacity)
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(inout) :: capacity
        character(len=64), allocatable :: new_names(:)
        integer, allocatable :: new_dims(:, :)
        logical, allocatable :: new_has(:)
        integer, allocatable :: new_init(:)
        integer :: new_capacity
        integer :: dim_slots

        new_capacity = capacity * 2
        dim_slots = size(per_var_dims, 2)

        allocate (new_names(new_capacity))
        allocate (new_dims(new_capacity, dim_slots))
        allocate (new_has(new_capacity))
        allocate (new_init(new_capacity))

        new_names = ""
        new_dims = 0
        new_has = .false.
        new_init = 0

        new_names(1:capacity) = var_names(1:capacity)
        new_dims(1:capacity, :) = per_var_dims(1:capacity, :)
        new_has(1:capacity) = has_dims(1:capacity)
        new_init(1:capacity) = init_indices(1:capacity)

        call move_alloc(new_names, var_names)
        call move_alloc(new_dims, per_var_dims)
        call move_alloc(new_has, has_dims)
        call move_alloc(new_init, init_indices)

        capacity = new_capacity
    end subroutine expand_multi_state

    subroutine parse_multi_variable_dimensions(parser, arena, per_var_dims, &
            has_dims, index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, intent(in) :: index
        type(token_t) :: token
        integer, allocatable :: local_dims(:)
        integer :: slot_count
        integer :: j

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if

        token = parser%consume()
        call parse_array_dimensions(parser, arena, local_dims)
        if (.not. allocated(local_dims)) then
            return
        end if

        if (size(local_dims) == 0) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(local_dims, temp)
            end block
            return
        end if

        has_dims(index) = .true.
        slot_count = min(size(local_dims), size(per_var_dims, 2))
        do j = 1, slot_count
            per_var_dims(index, j) = local_dims(j)
        end do
        do j = slot_count + 1, size(per_var_dims, 2)
            per_var_dims(index, j) = 0
        end do

        block
            integer, allocatable :: temp(:)
            call move_alloc(local_dims, temp)
        end block
    end subroutine parse_multi_variable_dimensions

    subroutine parse_multi_variable_initializer(parser, arena, type_spec, &
            init_index, has_any_initializer)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        integer, intent(inout) :: init_index
        logical, intent(inout) :: has_any_initializer
        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text == "=" .or. token%text == "=>") then
            token = parser%consume()
            if (type_spec%base_keyword == "complex") then
                init_index = handle_complex_initializer( &
                    parser, arena, type_spec%base_keyword)
            else
                init_index = parse_comparison(parser, arena)
            end if
            if (init_index > 0) then
                has_any_initializer = .true.
            end if
        end if
    end subroutine parse_multi_variable_initializer

    logical function continue_multi_variable(parser) result(should_continue)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        should_continue = .false.
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text == ",") then
            token = parser%consume()
            should_continue = .true.
        end if
    end function continue_multi_variable

    subroutine finalize_multi_declaration(arena, type_spec, attr_info, &
            var_names, per_var_dims, has_dims, &
            init_indices, var_count, &
            has_any_initializer, decl_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(in) :: var_count
        logical, intent(in) :: has_any_initializer
        integer, allocatable, intent(out) :: decl_indices(:)
        integer :: decl_index

        if (var_count <= 0) then
            allocate (decl_indices(0))
            return
        end if

        if (requires_individual_declarations(has_dims, has_any_initializer, &
            var_count)) then
            call emit_individual_declarations( &
                arena, type_spec, attr_info, var_names, per_var_dims, has_dims, &
                init_indices, var_count, decl_indices)
        else
            decl_index = emit_multi_declaration( &
                arena, type_spec, attr_info, var_names(1:var_count))
            if (decl_index > 0) then
                allocate (decl_indices(1))
                decl_indices(1) = decl_index
            else
                allocate (decl_indices(0))
            end if
        end if
    end subroutine finalize_multi_declaration

    subroutine emit_individual_declarations(arena, type_spec, attr_info, &
            var_names, per_var_dims, has_dims, &
            init_indices, var_count, decl_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(in) :: var_count
        integer, allocatable, intent(out) :: decl_indices(:)
        integer :: i
        integer, allocatable :: var_dims(:)

        allocate (decl_indices(var_count))

        do i = 1, var_count
            if (has_dims(i)) then
                call extract_variable_dimensions(per_var_dims, i, var_dims)
                decl_indices(i) = add_single_declaration( &
                    arena, type_spec, attr_info, var_names(i), &
                    init_indices(i), .true., var_dims)
                if (allocated(var_dims)) then
                    block
                        integer, allocatable :: temp(:)
                        call move_alloc(var_dims, temp)
                    end block
                end if
            else
                decl_indices(i) = add_single_declaration( &
                    arena, type_spec, attr_info, var_names(i), &
                    init_indices(i), .false.)
            end if
        end do
    end subroutine emit_individual_declarations

    logical function requires_individual_declarations( &
            has_dims, has_any_initializer, var_count) result(needs_split)
        logical, intent(in) :: has_dims(:)
        logical, intent(in) :: has_any_initializer
        integer, intent(in) :: var_count
        integer :: i

        needs_split = has_any_initializer
        if (needs_split) then
            return
        end if

        do i = 1, var_count
            if (has_dims(i)) then
                needs_split = .true.
                return
            end if
        end do
    end function requires_individual_declarations

    subroutine extract_variable_dimensions(per_var_dims, index, var_dims)
        integer, allocatable, intent(in) :: per_var_dims(:, :)
        integer, intent(in) :: index
        integer, allocatable, intent(out) :: var_dims(:)
        integer :: dim_count
        integer :: j

        dim_count = 0
        do j = 1, size(per_var_dims, 2)
            if (per_var_dims(index, j) > 0) then
                dim_count = dim_count + 1
            else
                exit
            end if
        end do

        if (dim_count == 0) then
            allocate (var_dims(0))
            return
        end if

        allocate (var_dims(dim_count))
        var_dims = per_var_dims(index, 1:dim_count)
    end subroutine extract_variable_dimensions

end module parser_declarations_multi_module
