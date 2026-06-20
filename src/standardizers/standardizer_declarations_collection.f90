module standardizer_declarations_collection
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, pointer_assignment_node, &
                              binary_op_node, &
                              call_or_subscript_node, identifier_node, literal_node
    use ast_nodes_data, only: declaration_node, derived_type_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_control, only: if_node, select_case_node, case_block_node, &
                                 case_default_node
    use ast_nodes_io, only: io_implied_do_node, print_statement_node, &
                            read_statement_node
    use ast_nodes_misc, only: allocate_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: goto_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_STRING
    use standardizer_declarations_state, only: get_standardizer_type_standardization
    use standardizer_declarations_inference, only: &
        handle_string_concatenation, &
        infer_type_from_binary_operation, &
        get_string_length_from_node, &
        infer_type_from_intrinsic_call, &
        build_character_type_from_length, &
        merge_character_type_lengths, &
        is_integer_expression
    use standardizer_declarations_variables, only: &
        add_variable, mark_variable_declared, &
        collect_identifier_var_with_type, collect_identifier_var
    use standardizer_types, only: get_array_var_type, get_expression_type, &
                                  get_fortran_type_string, is_array_expression, &
                                  string_result_t
    use string_utils_mod, only: int_to_string
    use lexer_core, only: to_lower
    use type_string_utils, only: is_character_type_string, mono_type_to_string
    use type_system_unified, only: mono_type_t
    implicit none
    private

    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: handle_string_concatenation
    public :: infer_type_from_binary_operation
    public :: get_string_length_from_node

contains

    subroutine collect_statement_vars(arena, stmt_index, var_names, &
                                      var_types, var_declared, var_count, &
                                      function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        type stack_entry
            integer :: idx = 0
        end type stack_entry

        type(stack_entry), allocatable :: stack(:)
        integer :: capacity, top
        integer :: current_index
        integer :: j

        capacity = 128
        allocate (stack(capacity))
        top = 0

        call push(stmt_index)

        do while (top > 0)
            current_index = pop()
            if (.not. arena%has_node_at(current_index)) cycle

            call process_node_for_vars(arena, current_index, var_names, &
                                       var_types, var_declared, var_count, &
                                       function_names, func_count)
        end do

    contains

        subroutine process_node_for_vars(arena_l, node_idx, vnames, vtypes, &
                                         vdecl, vcount, fnames, fcount)
            type(ast_arena_t), intent(in) :: arena_l
            integer, intent(in) :: node_idx
            character(len=64), intent(inout) :: vnames(:), vtypes(:)
            logical, intent(inout) :: vdecl(:)
            integer, intent(inout) :: vcount
            character(len=64), intent(in) :: fnames(:)
            integer, intent(in) :: fcount

            select type (stmt => arena_l%entries(node_idx)%node)
            type is (declaration_node)
                call process_declaration_node(arena_l, stmt, vnames, vtypes, &
                                              vdecl, vcount, fnames, fcount)
            type is (assignment_node)
                call collect_assignment_vars(arena_l, node_idx, vnames, &
                                             vtypes, vdecl, vcount, &
                                             fnames, fcount)
            type is (pointer_assignment_node)
                call collect_pointer_assignment_vars(arena_l, node_idx, &
                                                     vnames, vtypes, &
                                                     vdecl, vcount, &
                                                     fnames, fcount)
            type is (do_loop_node)
                call process_do_loop_node(stmt, vnames, vtypes, &
                                          vdecl, vcount, fnames, fcount)
            type is (do_while_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (io_implied_do_node)
                call process_io_implied_do_node(stmt, vnames, vtypes, &
                                                vdecl, vcount, fnames, fcount)
            type is (if_node)
                call process_if_node_bodies(stmt)
            type is (select_case_node)
                call process_select_case_node_stmt(stmt)
            type is (case_block_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (case_default_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (print_statement_node)
                if (allocated(stmt%expression_indices)) then
                    call push_many(stmt%expression_indices)
                end if
            type is (read_statement_node)
                if (allocated(stmt%var_indices)) then
                    call push_many(stmt%var_indices)
                end if
            type is (allocate_statement_node)
                call collect_allocate_vars(arena_l, node_idx, vnames, &
                                           vtypes, vdecl, vcount)
            type is (goto_node)
                ! Handle computed goto: push selector expression to collect variables
                if (stmt%selector_index > 0) call push(stmt%selector_index)
            type is (function_def_node)
                ! Do not traverse into contained function bodies
            type is (subroutine_def_node)
                ! Do not traverse into contained subroutine bodies
            type is (identifier_node)
                call collect_identifier_var(stmt, vnames, vtypes, &
                                            vdecl, vcount, fnames, fcount)
            class default
            end select
        end subroutine process_node_for_vars

        subroutine process_declaration_node(arena_l, decl, vnames, vtypes, &
                                            vdecl, vcount, fnames, fcount)
            type(ast_arena_t), intent(in) :: arena_l
            type(declaration_node), intent(in) :: decl
            character(len=64), intent(inout) :: vnames(:), vtypes(:)
            logical, intent(inout) :: vdecl(:)
            integer, intent(inout) :: vcount
            character(len=64), intent(in) :: fnames(:)
            integer, intent(in) :: fcount
            integer :: j

            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                do j = 1, size(decl%var_names)
                    call handle_declaration_variable(arena_l, decl, &
                                                     trim(decl%var_names(j)), &
                                                     vnames, vtypes, vdecl, &
                                                     vcount, fnames, fcount)
                end do
            else
                call handle_declaration_variable(arena_l, decl, &
                                                 trim(decl%var_name), &
                                                 vnames, vtypes, vdecl, &
                                                 vcount, fnames, fcount)
            end if
        end subroutine process_declaration_node

        subroutine process_do_loop_node(loop, vnames, vtypes, vdecl, &
                                        vcount, fnames, fcount)
            type(do_loop_node), intent(in) :: loop
            character(len=64), intent(inout) :: vnames(:), vtypes(:)
            logical, intent(inout) :: vdecl(:)
            integer, intent(inout) :: vcount
            character(len=64), intent(in) :: fnames(:)
            integer, intent(in) :: fcount

            call add_variable(loop%var_name, "integer", vnames, vtypes, &
                              vdecl, vcount, fnames, fcount)
            if (allocated(loop%body_indices)) call push_many(loop%body_indices)
        end subroutine process_do_loop_node

        subroutine process_io_implied_do_node(loop, vnames, vtypes, vdecl, &
                                              vcount, fnames, fcount)
            type(io_implied_do_node), intent(in) :: loop
            character(len=64), intent(inout) :: vnames(:), vtypes(:)
            logical, intent(inout) :: vdecl(:)
            integer, intent(inout) :: vcount
            character(len=64), intent(in) :: fnames(:)
            integer, intent(in) :: fcount
            integer :: obj_idx

            call add_variable(loop%var_name, "integer", vnames, vtypes, &
                              vdecl, vcount, fnames, fcount)
            if (allocated(loop%object_indices)) then
                do obj_idx = 1, size(loop%object_indices)
                    if (loop%object_indices(obj_idx) > 0) then
                        call push(loop%object_indices(obj_idx))
                    end if
                end do
            else if (loop%expr_index > 0) then
                call push(loop%expr_index)
            end if
        end subroutine process_io_implied_do_node

        subroutine process_if_node_bodies(if_stmt)
            type(if_node), intent(in) :: if_stmt

            if (allocated(if_stmt%else_body_indices)) call &
                push_many(if_stmt%else_body_indices)
            if (allocated(if_stmt%then_body_indices)) call &
                push_many(if_stmt%then_body_indices)
        end subroutine process_if_node_bodies

        subroutine process_select_case_node_stmt(select_stmt)
            type(select_case_node), intent(in) :: select_stmt

            if (select_stmt%selector_index > 0) call push(select_stmt%selector_index)
            if (allocated(select_stmt%case_indices)) call &
                push_many(select_stmt%case_indices)
            if (select_stmt%default_index > 0) call push(select_stmt%default_index)
        end subroutine process_select_case_node_stmt

        subroutine push(idx)
            integer, intent(in) :: idx
            type(stack_entry), allocatable :: tmp(:)
            if (idx <= 0) return
            if (top >= capacity) then
                allocate (tmp(capacity * 2))
                if (capacity > 0) tmp(1:capacity) = stack(1:capacity)
                call move_alloc(tmp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = idx
        end subroutine push

        subroutine push_many(indices)
            integer, intent(in) :: indices(:)
            integer :: k
            do k = size(indices), 1, -1
                call push(indices(k))
            end do
        end subroutine push_many

        integer function pop()
            if (top <= 0) then
                pop = 0
            else
                pop = stack(top)%idx
                top = top - 1
            end if
        end function pop

    end subroutine collect_statement_vars

    subroutine handle_declaration_variable(arena, decl, var_name, var_names, &
                                           var_types, var_declared, var_count, &
                                           function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: var_name
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        character(len=:), allocatable :: type_str
        integer :: idx, k
        character(len=64) :: normalized_name, normalized_existing

        if (len_trim(var_name) == 0) return

        normalized_name = to_lower(trim(var_name))
        type_str = build_declaration_type_string(arena, decl)
        call add_variable(var_name, type_str, var_names, var_types, &
                          var_declared, var_count, function_names, func_count)
        call mark_variable_declared(var_name, var_names, var_declared, var_count)

        idx = 0
        do k = 1, var_count
            normalized_existing = to_lower(trim(var_names(k)))
            if (trim(normalized_existing) == trim(normalized_name)) then
                idx = k
                exit
            end if
        end do
        if (idx > 0 .and. len_trim(type_str) > 0) then
            var_types(idx) = type_str
        end if
    end subroutine handle_declaration_variable

    function build_declaration_type_string(arena, decl) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl
        character(len=:), allocatable :: type_str
        character(len=32) :: buffer

        type_str = trim(decl%type_name)
        if (decl%has_kind) then
            buffer = int_to_string(decl%kind_value)
            if (len_trim(buffer) > 0) then
                type_str = trim(type_str) // "(" // trim(buffer) // ")"
            end if
        end if

        if (decl%is_array .and. allocated(decl%dimension_indices)) then
            type_str = append_dimension_spec(arena, type_str, &
                                             decl%dimension_indices)
        end if

        type_str = append_attributes_to_type(type_str, decl)
    end function build_declaration_type_string

    function append_attributes_to_type(type_str, decl) result(result_str)
        character(len=*), intent(in) :: type_str
        type(declaration_node), intent(in) :: decl
        character(len=:), allocatable :: result_str
        character(len=:), allocatable :: lowered

        result_str = trim(type_str)

        if (decl%is_allocatable) then
            lowered = to_lower(result_str)
            if (index(lowered, 'allocatable') == 0) then
                result_str = trim(result_str) // ", allocatable"
            end if
        end if

        if (decl%is_pointer) then
            lowered = to_lower(result_str)
            if (index(lowered, 'pointer') == 0) then
                result_str = trim(result_str) // ", pointer"
            end if
        end if

        if (decl%is_target) then
            lowered = to_lower(result_str)
            if (index(lowered, 'target') == 0) then
                result_str = trim(result_str) // ", target"
            end if
        end if

        if (decl%is_parameter) then
            lowered = to_lower(result_str)
            if (index(lowered, 'parameter') == 0) then
                result_str = trim(result_str) // ", parameter"
            end if
        end if

        if (decl%has_intent .and. allocated(decl%intent)) then
            lowered = to_lower(result_str)
            if (index(lowered, 'intent(') == 0) then
                result_str = trim(result_str) // ", intent(" // &
                             trim(decl%intent) // ")"
            end if
        end if
    end function append_attributes_to_type

    function append_dimension_spec(arena, type_str, dimension_indices) &
        result(result_str)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_str
        integer, intent(in) :: dimension_indices(:)
        character(len=:), allocatable :: result_str
        character(len=32) :: buffer
        integer :: dim_idx, i

        result_str = trim(type_str) // ", dimension("
        do i = 1, size(dimension_indices)
            if (i > 1) result_str = result_str // ","
            dim_idx = dimension_indices(i)
            if (dim_idx == 0) then
                result_str = result_str // ":"
            else if (dim_idx > 0 .and. dim_idx <= arena%size) then
                if (allocated(arena%entries(dim_idx)%node)) then
                    select type (dim_node => arena%entries(dim_idx)%node)
                    type is (literal_node)
                        result_str = result_str // trim(dim_node%value)
                    class default
                        result_str = result_str // ":"
                    end select
                else
                    result_str = result_str // ":"
                end if
            else if (dim_idx > arena%size) then
                buffer = int_to_string(dim_idx)
                result_str = result_str // trim(buffer)
            else
                result_str = result_str // ":"
            end if
        end do
        result_str = result_str // ")"
    end function append_dimension_spec

    subroutine collect_allocate_vars(arena, alloc_index, var_names, &
                                     var_types, var_declared, var_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: alloc_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        integer :: i, var_index, rank
        character(len=:), allocatable :: var_name
        character(len=:), allocatable :: base_type
        character(len=:), allocatable :: dimension_spec
        character(len=64) :: var_type

        if (.not. arena%has_node_at(alloc_index)) return

        select type (alloc_stmt => arena%entries(alloc_index)%node)
        type is (allocate_statement_node)
            if (.not. allocated(alloc_stmt%var_indices)) return

            do i = 1, size(alloc_stmt%var_indices)
                var_index = alloc_stmt%var_indices(i)
                if (.not. arena%has_node_at(var_index)) cycle

                select type (node => arena%entries(var_index)%node)
                type is (identifier_node)
                    var_name = trim(node%name)
                    rank = 0
                    if (allocated(alloc_stmt%shape_indices)) then
                        rank = size(alloc_stmt%shape_indices)
                    end if
                type is (call_or_subscript_node)
                    var_name = trim(node%name)
                    rank = 0
                    if (allocated(node%arg_indices)) then
                        rank = size(node%arg_indices)
                    else if (allocated(alloc_stmt%shape_indices)) then
                        rank = size(alloc_stmt%shape_indices)
                    end if
                class default
                    ! Unknown node type in allocate - skip it
                    cycle
                end select

                if (len_trim(var_name) > 0) then
                    ! Determine base type
                    if (allocated(alloc_stmt%type_spec)) then
                        base_type = trim(alloc_stmt%type_spec)
                    else
                        base_type = "integer"
                    end if

                    ! Build type string with allocatable attribute
                    if (rank > 0) then
                        dimension_spec = ":"
                        block
                            integer :: j
                            do j = 2, rank
                                dimension_spec = trim(dimension_spec) // ",:"
                            end do
                        end block
                        var_type = trim(base_type) // ", dimension(" // &
                                   trim(dimension_spec) // "), allocatable"
                    else
                        var_type = trim(base_type) // ", allocatable"
                    end if

                    call add_or_update_alloc_var(var_name, var_type, var_names, &
                                                 var_types, var_declared, &
                                                 var_count)
                end if
            end do
        end select
    end subroutine collect_allocate_vars

    subroutine add_or_update_alloc_var(name, var_type, var_names, var_types, &
                                       var_declared, var_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64) :: normalized_name
        character(len=64) :: normalized_existing
        integer :: idx, j

        if (len_trim(name) == 0) return

        normalized_name = to_lower(trim(name))
        idx = 0

        do j = 1, var_count
            normalized_existing = to_lower(trim(var_names(j)))
            if (trim(normalized_existing) == trim(normalized_name)) then
                idx = j
                exit
            end if
        end do

        if (idx == 0) then
            block
                character(len=64) :: dummy_func_names(1)
                integer :: dummy_func_count
                dummy_func_names = ""
                dummy_func_count = 0
                call add_variable(name, var_type, var_names, var_types, &
                                  var_declared, var_count, &
                                  dummy_func_names, dummy_func_count)
            end block
            idx = var_count
        else
            var_declared(idx) = .true.
        end if

        if (idx > 0) then
            var_types(idx) = var_type
        end if
    end subroutine add_or_update_alloc_var

    subroutine collect_assignment_vars(arena, assign_index, var_names, &
                                       var_types, var_declared, var_count, &
                                       function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        if (.not. arena%has_node_at(assign_index)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            ! Skip keyword arguments - they don't create variable declarations
            if (assign%is_keyword_argument) return

            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
                        call handle_identifier_assignment_target(arena, assign, &
                                                                 target, &
                                                                 var_names, &
                                                                 var_types, &
                                                                 var_declared, &
                                                                 var_count, &
                                                                 function_names, &
                                                                 func_count)
                    type is (call_or_subscript_node)
                        call handle_array_assignment_target(assign, target, &
                                                            var_names, &
                                                            var_types, &
                                                            var_declared, &
                                                            var_count, &
                                                            function_names, &
                                                            func_count)
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

    subroutine handle_identifier_assignment_target(arena, assign, target, &
                                                   var_names, var_types, &
                                                   var_declared, var_count, &
                                                   function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: assign
        type(identifier_node), intent(in) :: target
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        character(len=64) :: var_type
        integer :: existing_idx

        existing_idx = find_existing_variable_index(target%name, var_names, &
                                                    var_count)
        var_type = infer_assignment_type(arena, assign%value_index)

        if (existing_idx > 0) then
            call update_existing_variable_type(existing_idx, var_type, &
                                               var_types)
        else
            call collect_identifier_var_with_type(target, var_type, var_names, &
                                                  var_types, var_declared, &
                                                  var_count, function_names, &
                                                  func_count)
        end if
    end subroutine handle_identifier_assignment_target

    function find_existing_variable_index(var_name, var_names, var_count) &
        result(idx)
        character(len=*), intent(in) :: var_name
        character(len=64), intent(in) :: var_names(:)
        integer, intent(in) :: var_count
        integer :: idx
        character(len=64) :: normalized_target, normalized_existing
        integer :: i

        idx = 0
        normalized_target = to_lower(trim(var_name))
        do i = 1, var_count
            normalized_existing = to_lower(trim(var_names(i)))
            if (trim(normalized_existing) == trim(normalized_target)) then
                idx = i
                exit
            end if
        end do
    end function find_existing_variable_index

    function infer_assignment_type(arena, value_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: value_index
        character(len=64) :: var_type
        type(mono_type_t), pointer :: value_type
        integer :: literal_length

        var_type = ""

        if (value_index <= 0 .or. value_index > arena%size) then
            var_type = "real"
            return
        end if

        if (.not. allocated(arena%entries(value_index)%node)) then
            var_type = "real"
            return
        end if

        ! Derived-type structure constructor: RHS call name matches a defined
        ! derived type, so the target is type(<name>) (Issue #2827).
        var_type = derived_constructor_var_type(arena, value_index)
        if (len_trim(var_type) > 0) return

        if (is_array_expression(arena, value_index)) then
            var_type = get_array_var_type(arena, value_index)
            return
        end if

        value_type => get_expression_type(arena, value_index)
        if (associated(value_type)) then
            block
                type(string_result_t) :: type_result
                type_result = get_fortran_type_string(value_type)
                if (type_result%is_success()) then
                    var_type = type_result%get_value()
                    return
                end if
            end block
        end if

        call infer_type_from_intrinsic_call(arena, value_index, var_type)
        if (len_trim(var_type) > 0) return

        if (is_integer_expression(arena, value_index)) then
            var_type = "integer"
            return
        end if

        var_type = handle_string_concatenation(arena, value_index)
        if (len_trim(var_type) > 0) return

        var_type = infer_type_from_binary_operation(arena, value_index)
        if (len_trim(var_type) > 0) return

        literal_length = get_string_length_from_node(arena, value_index)
        if (literal_length >= 0) then
            var_type = build_character_type_from_length(literal_length)
            return
        end if

        var_type = "real"
    end function infer_assignment_type

    function derived_constructor_var_type(arena, value_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: value_index
        character(len=64) :: var_type
        character(len=:), allocatable :: call_name

        var_type = ""
        if (value_index <= 0 .or. value_index > arena%size) return
        if (.not. allocated(arena%entries(value_index)%node)) return

        select type (v => arena%entries(value_index)%node)
        type is (call_or_subscript_node)
            if (.not. allocated(v%name)) return
            call_name = v%name
        class default
            return
        end select

        if (derived_type_is_defined(arena, call_name)) &
            var_type = "type(" // trim(call_name) // ")"
    end function derived_constructor_var_type

    logical function derived_type_is_defined(arena, name) result(is_defined)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: i

        is_defined = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (derived_type_node)
                if (.not. allocated(n%name)) cycle
                if (to_lower(trim(n%name)) == to_lower(trim(name))) then
                    is_defined = .true.
                    return
                end if
            end select
        end do
    end function derived_type_is_defined

    subroutine update_existing_variable_type(existing_idx, var_type, var_types)
        integer, intent(in) :: existing_idx
        character(len=*), intent(in) :: var_type
        character(len=64), intent(inout) :: var_types(:)

        if (len_trim(var_type) == 0) return

        ! Do not overwrite allocatable declarations (fixes #2069)
        if (index(to_lower(var_types(existing_idx)), 'allocatable') > 0) then
            ! Keep existing allocatable type
        else if (is_character_type_string(var_types(existing_idx)) &
                 .and. is_character_type_string(var_type)) then
            var_types(existing_idx) = merge_character_type_lengths( &
                                      var_types(existing_idx), var_type)
        else
            var_types(existing_idx) = trim(var_type)
        end if

        if (index(var_types(existing_idx), 'character(') == 1 &
            .and. index(var_types(existing_idx), 'len=:') > 0 &
            .and. index(var_types(existing_idx), 'allocatable') == 0) then
            var_types(existing_idx) = trim(var_types(existing_idx)) &
                                      // ", allocatable"
        end if
    end subroutine update_existing_variable_type

    subroutine handle_array_assignment_target(assign, target, var_names, &
                                              var_types, var_declared, &
                                              var_count, function_names, &
                                              func_count)
        type(assignment_node), intent(in) :: assign
        type(call_or_subscript_node), intent(in) :: target
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        character(len=64) :: base_name
        character(len=96) :: decl_type
        integer :: rank, idx

        if (.not. target%is_array_access .or. .not. allocated(target%name)) &
            return

        base_name = to_lower(trim(target%name))
        decl_type = ''

        if (assign%type_was_inferred .and. &
            allocated(assign%inferred_type_name)) then
            decl_type = trim(assign%inferred_type_name)
        end if

        if (len_trim(decl_type) == 0) then
            rank = 0
            if (allocated(target%arg_indices)) rank = size(target%arg_indices)
            if (rank <= 0) rank = 1
            decl_type = 'real, dimension('
            do idx = 1, rank
                if (idx > 1) decl_type = trim(decl_type) // ','
                decl_type = trim(decl_type) // ':'
            end do
            decl_type = trim(decl_type) // ')'
        end if

        call add_variable(base_name, decl_type, var_names, var_types, &
                          var_declared, var_count, function_names, func_count)
    end subroutine handle_array_assignment_target

    subroutine collect_pointer_assignment_vars(arena, ptr_assign_index, var_names, &
                                               var_types, var_declared, var_count, &
                                               function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: ptr_assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        character(len=64) :: var_type
        integer :: existing_idx

        if (.not. arena%has_node_at(ptr_assign_index)) return

        select type (ptr_assign => arena%entries(ptr_assign_index)%node)
        type is (pointer_assignment_node)
            if (ptr_assign%pointer_index > 0 .and. &
                ptr_assign%pointer_index <= arena%size) then
                if (allocated(arena%entries(ptr_assign%pointer_index)%node)) then
                    select type (ptr_node => &
                                 arena%entries(ptr_assign%pointer_index)%node)
                    type is (identifier_node)
                        existing_idx = find_existing_variable_index(ptr_node%name, &
                                                                    var_names, &
                                                                    var_count)
                        var_type = infer_pointer_assignment_type(arena, &
                                                                 ptr_assign)

                        if (existing_idx > 0) then
                            var_types(existing_idx) = var_type
                        else
                            call add_variable(trim(ptr_node%name), var_type, &
                                              var_names, var_types, &
                                              var_declared, var_count, &
                                              function_names, func_count)
                        end if
                    end select
                end if
            end if
        end select
    end subroutine collect_pointer_assignment_vars

    function infer_pointer_assignment_type(arena, ptr_assign) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        type(pointer_assignment_node), intent(in) :: ptr_assign
        character(len=64) :: var_type
        type(mono_type_t), pointer :: pointer_type, target_type
        logical :: target_is_null

        target_is_null = is_null_pointer_assignment_target(arena, &
                                                           ptr_assign%target_index)

        pointer_type => get_expression_type(arena, ptr_assign%pointer_index)
        var_type = extract_type_from_mono_type(pointer_type)

        if (len_trim(var_type) == 0 .and. target_is_null) then
            var_type = "integer"
        end if

        if (len_trim(var_type) == 0) then
            target_type => get_expression_type(arena, ptr_assign%target_index)
            var_type = extract_type_from_mono_type(target_type)
        end if

        if (len_trim(var_type) == 0) then
            var_type = "integer"
        end if

        var_type = ensure_pointer_attribute(var_type)
    end function infer_pointer_assignment_type

    function extract_type_from_mono_type(mono_type) result(type_str)
        type(mono_type_t), pointer, intent(in) :: mono_type
        character(len=64) :: type_str
        type(string_result_t) :: type_result
        character(len=:), allocatable :: inferred_string
        logical :: string_success, standardize_flag

        type_str = ""
        if (.not. associated(mono_type)) return

        type_result = get_fortran_type_string(mono_type)
        if (type_result%is_success()) then
            type_str = type_result%get_value()
            return
        end if

        call get_standardizer_type_standardization(standardize_flag)
        inferred_string = mono_type_to_string(mono_type, &
                                              include_shape=.false., &
                                              prefer_len_zero_char=.true., &
                                              standardize_real=standardize_flag, &
                                              success=string_success)
        if (string_success) then
            type_str = inferred_string
        end if
    end function extract_type_from_mono_type

    function ensure_pointer_attribute(type_str) result(result_str)
        character(len=*), intent(in) :: type_str
        character(len=64) :: result_str
        character(len=:), allocatable :: lowered_type

        lowered_type = to_lower(trim(type_str))
        if (index(lowered_type, 'pointer') == 0) then
            result_str = trim(type_str) // ", pointer"
        else
            result_str = trim(type_str)
        end if
    end function ensure_pointer_attribute

    logical function is_null_pointer_assignment_target(arena, expr_index) &
        result(is_null_target)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        is_null_target = .false.
        if (.not. arena%has_node_at(expr_index)) return

        select type (target_node => arena%entries(expr_index)%node)
        type is (call_or_subscript_node)
            if (allocated(target_node%name)) then
                if (to_lower(trim(target_node%name)) == 'null') then
                    is_null_target = .true.
                    return
                end if
            end if
        type is (identifier_node)
            if (allocated(target_node%name)) then
                if (to_lower(trim(target_node%name)) == 'null') then
                    is_null_target = .true.
                    return
                end if
            end if
        end select
    end function is_null_pointer_assignment_target

end module standardizer_declarations_collection
