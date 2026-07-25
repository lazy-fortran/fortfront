
        subroutine prepare_function_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(function_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(infer_frame_t) :: post_frame
            type(mono_type_t), allocatable :: param_types(:)
            character(len=64), allocatable :: param_names(:)
            type(mono_type_t) :: return_type
            character(len=:), allocatable :: elemental_result_name
            integer :: i

            call analyze_function_parameters(arena, expr, param_types, param_names, &
                                             this%scopes, this%next_var_id)
            return_type = determine_function_return_type(arena, expr, param_names, &
                                                         param_types, this%next_var_id)
            call create_function_scope(arena, expr, node_index, return_type, &
                                       this%scopes)

            ! Enforce PURE/ELEMENTAL body restrictions (F2008 C1283)
            call validate_pure_procedure(arena, expr%body_indices, &
                                         expr%prefix_keywords, this%errors, &
                                         param_indices=expr%param_indices, &
                                         is_function=.true.)
            ! Enforce ELEMENTAL scalar-dummy restriction (F2008 C1290)
            if (allocated(expr%result_variable)) then
                elemental_result_name = expr%result_variable
            else
                elemental_result_name = expr%name
            end if
            call validate_elemental_procedure(arena, expr%param_indices, &
                                              expr%body_indices, &
                                              expr%prefix_keywords, this%errors, &
                                              proc_name=expr%name, &
                                              bind_c_clause=expr%bind_c_clause, &
                                              result_name=elemental_result_name, &
                                              require_declared_dummy_intent= &
                                              this%input_mode == INPUT_MODE_STANDARD, &
                                              procedure_line=expr%line, &
                                              procedure_column=expr%column)

            ! Enforce BIND(C) dummy-argument interoperability (F2003 15.3.2)
            call validate_bind_c_procedure(arena, expr%param_indices, &
                                           expr%body_indices, expr%bind_c_clause, &
                                           expr%name, this%errors)

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            post_frame%has_cached_type = .true.
            post_frame%cached_type = return_type
            call adopt_param_types(post_frame, param_types)
            call push_frame_local(post_frame)

            if (allocated(param_names)) deallocate (param_names)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_function_definition

        subroutine prepare_subroutine_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(subroutine_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(infer_frame_t) :: post_frame
            type(mono_type_t), allocatable :: param_types(:)
            character(len=64), allocatable :: param_names(:)
            integer :: i

            call analyze_subroutine_parameters(arena, expr, param_types, param_names, &
                                               this%scopes, this%next_var_id)
            call create_subroutine_scope(arena, expr, node_index, this%scopes)

            ! Enforce PURE/ELEMENTAL body restrictions (F2008 C1283)
            call validate_pure_procedure(arena, expr%body_indices, &
                                         expr%prefix_keywords, this%errors, &
                                         param_indices=expr%param_indices, &
                                         is_function=.false.)
            ! Enforce ELEMENTAL scalar-dummy restriction (F2008 C1290)
            call validate_elemental_procedure(arena, expr%param_indices, &
                                              expr%body_indices, &
                                              expr%prefix_keywords, this%errors, &
                                              proc_name=expr%name, &
                                              bind_c_clause=expr%bind_c_clause, &
                                              require_declared_dummy_intent= &
                                              this%input_mode == INPUT_MODE_STANDARD, &
                                              procedure_line=expr%line, &
                                              procedure_column=expr%column)

            ! Enforce BIND(C) dummy-argument interoperability (F2003 15.3.2)
            call validate_bind_c_procedure(arena, expr%param_indices, &
                                           expr%body_indices, expr%bind_c_clause, &
                                           expr%name, this%errors)

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            call adopt_param_types(post_frame, param_types)
            call push_frame_local(post_frame)

            if (allocated(param_names)) deallocate (param_names)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_subroutine_definition

        subroutine prepare_forall_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(forall_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            type(poly_type_t) :: int_scheme
            type(mono_type_t) :: control_type
            integer :: i

            call process_forall_node_body(expr, int_scheme, control_type)
            call this%scopes%enter_block()
            if (allocated(expr%index_names)) then
                do i = 1, size(expr%index_names)
                    call this%scopes%define(expr%index_names(i), int_scheme)
                end do
            end if

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            post_frame%has_cached_type = .true.
            post_frame%cached_type = control_type
            call push_frame_local(post_frame)

            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_forall_node

        subroutine schedule_select_case_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(select_case_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%case_indices)) then
                do i = size(expr%case_indices), 1, -1
                    call push_child(expr%case_indices(i))
                end do
            end if
            call push_child(expr%selector_index)
        end subroutine schedule_select_case_node

        subroutine prepare_associate_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(associate_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            call push_frame_local(post_frame)
            call this%scopes%enter_block()

            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if

            if (allocated(expr%associations)) then
                do i = size(expr%associations), 1, -1
                    if (expr%associations(i)%expr_index <= 0) cycle
                    call init_post_frame(current, post_frame)
                    post_frame%node_index = current%node_index
                    post_frame%state = STATE_ASSOC_DEFINE
                    post_frame%aux_index = i
                    call push_frame_local(post_frame)
                    call push_child(expr%associations(i)%expr_index)
                end do
            end if
        end subroutine prepare_associate_node

        subroutine schedule_nullify_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(nullify_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%pointer_indices)) then
                do i = size(expr%pointer_indices), 1, -1
                    if (expr%pointer_indices(i) > 0) then
                        call push_child(expr%pointer_indices(i))
                    end if
                end do
            end if
        end subroutine schedule_nullify_node

        subroutine schedule_read_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(read_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%var_indices)) then
                do i = size(expr%var_indices), 1, -1
                    call push_child(expr%var_indices(i))
                end do
            end if
        end subroutine schedule_read_statement_node

        subroutine schedule_allocate_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(allocate_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%var_indices)) then
                do i = size(expr%var_indices), 1, -1
                    call push_child(expr%var_indices(i))
                end do
            end if
        end subroutine schedule_allocate_statement_node

        subroutine schedule_print_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(print_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%expression_indices)) then
                do i = size(expr%expression_indices), 1, -1
                    call push_child(expr%expression_indices(i))
                end do
            end if
        end subroutine schedule_print_statement_node

        subroutine schedule_data_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(data_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)

            if (allocated(expr%value_indices)) then
                do i = size(expr%value_indices), 1, -1
                    call push_child(expr%value_indices(i))
                end do
            end if

            if (allocated(expr%object_indices)) then
                do i = size(expr%object_indices), 1, -1
                    call push_child(expr%object_indices(i))
                end do
            end if
        end subroutine schedule_data_statement_node

        subroutine schedule_single_child_frame(current, child_index)
            type(infer_frame_t), intent(in) :: current
            integer, intent(in) :: child_index
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(child_index)
        end subroutine schedule_single_child_frame

        subroutine schedule_default_post(current)
            type(infer_frame_t), intent(in) :: current
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
        end subroutine schedule_default_post

        subroutine finalize_data_statement_node(node_index, expr)
            integer, intent(in) :: node_index
            type(data_statement_node), intent(in) :: expr

            call infer_data_statement_types(expr)
            call finalize_node(node_index, control_flow_type())
        end subroutine finalize_data_statement_node

        subroutine infer_data_statement_types(expr)
            type(data_statement_node), intent(in) :: expr

            if (.not. allocated(expr%object_indices)) return
            if (.not. allocated(expr%value_indices)) return
            if (size(expr%object_indices) == 0) return
            if (size(expr%value_indices) == 0) return

            if (size(expr%object_indices) == 1) then
                call infer_single_data_object(expr%object_indices(1), &
                                              expr%value_indices)
            else
                call infer_multi_data_objects(expr%object_indices, &
                                              expr%value_indices)
            end if
        end subroutine infer_data_statement_types

        subroutine infer_single_data_object(object_index, value_indices)
            use ast_nodes_core, only: identifier_node
            use error_handling, only: create_error_result, &
                                      ERROR_SEMANTIC, result_t
            integer, intent(in) :: object_index
            integer, intent(in) :: value_indices(:)
            type(assignment_node) :: synthetic_assignment
            type(mono_type_t) :: expr_typ
            type(mono_type_t) :: updated_expr_typ
            type(mono_type_t) :: declared_type
            logical :: has_declared_type
            character(len=:), allocatable :: target_name
            character(len=32) :: value_count_str

            if (object_index <= 0) return
            if (size(value_indices) == 0) return

            synthetic_assignment%target_index = object_index
            synthetic_assignment%operator = "="

            if (size(value_indices) == 1) then
                synthetic_assignment%value_index = value_indices(1)
                expr_typ = get_node_type(value_indices(1))
            else
                has_declared_type = &
                    get_data_target_declared_type(object_index, &
                                                  declared_type)
                if (has_declared_type) then
                    if (declared_type%kind /= TARRAY) then
                        target_name = "unknown"
                        if (object_index > 0 .and. object_index <= &
                            arena%size) then
                            if (allocated(arena%entries(object_index)%node)) then
                                select type (target_node => &
                                             arena%entries(object_index)%node)
                                type is (identifier_node)
                                    if (allocated(target_node%name)) then
                                        if (len_trim(target_node%name) > 0) then
                                            target_name = trim(target_node%name)
                                        end if
                                    end if
                                class default
                                    continue
                                end select
                            end if
                        end if

                        write (value_count_str, '(I0)') size(value_indices)
                        block
                            type(result_t) :: error_result
                            character(len=:), allocatable :: message

                            message = "DATA statement has "// &
                                      trim(value_count_str)
                            message = message// &
                                      " values for scalar object "// &
                                      trim(target_name)
                            error_result = create_error_result( &
                                           trim(message), ERROR_SEMANTIC, &
                                           component="semantic_analyzer", &
                                           context="data_statement", &
                                           suggestion="Declare as array", &
                                           line=arena%entries(object_index)%node% &
                                           line, &
                                           column=arena%entries(object_index)%node% &
                                           column, &
                                           end_line=arena%entries(object_index)% &
                                           node%line, &
                                           end_column=arena%entries(object_index)% &
                                           node%column + 1)
                            call this%errors%add_result(error_result)
                        end block
                        return
                    end if
                    expr_typ = declared_type
                else
                    expr_typ = build_array_type_from_values(value_indices)
                end if

                synthetic_assignment%value_index = 0
            end if

            updated_expr_typ = expr_typ
            call process_assignment_inference( &
                arena, synthetic_assignment, 0, object_index, expr_typ, &
                updated_expr_typ, this%scopes, this%errors, this%input_mode, &
                this%next_var_id, this%parser_type_hints)
        end subroutine infer_single_data_object

        subroutine infer_multi_data_objects(object_indices, value_indices)
            use error_handling, only: create_error_result, &
                                      ERROR_SEMANTIC, result_t
            integer, intent(in) :: object_indices(:)
            integer, intent(in) :: value_indices(:)
            type(assignment_node) :: synthetic_assignment
            type(mono_type_t) :: expr_typ
            type(mono_type_t) :: updated_expr_typ
            type(mono_type_t) :: declared_type
            logical :: has_declared_type
            logical :: all_scalar_identifiers
            integer :: cursor
            integer :: i
            integer :: num_objects
            integer :: value_count
            character(len=32) :: obj_count_str
            character(len=32) :: value_count_str

            value_count = size(value_indices)
            if (value_count == 0) return

            all_scalar_identifiers = .true.
            num_objects = 0
            do i = 1, size(object_indices)
                if (object_indices(i) <= 0) cycle
                num_objects = num_objects + 1
                has_declared_type = &
                    get_data_target_declared_type(object_indices(i), &
                                                  declared_type)
                if (.not. has_declared_type) then
                    all_scalar_identifiers = .false.
                    exit
                end if
                if (declared_type%kind == TARRAY) then
                    all_scalar_identifiers = .false.
                    exit
                end if
            end do

            if (num_objects > 0 .and. all_scalar_identifiers .and. &
                value_count /= num_objects) then
                write (obj_count_str, '(I0)') num_objects
                write (value_count_str, '(I0)') value_count
                block
                    type(result_t) :: error_result

                    error_result = create_error_result( &
                                   "DATA statement has "//trim(value_count_str)// &
                                   " values for "//trim(obj_count_str)// &
                                   " scalar objects", &
                                   ERROR_SEMANTIC, "semantic_analyzer", &
                                   "data_statement", &
                                   "Match values to objects", &
                                   line=arena%entries(object_indices(1))%node%line, &
                                   column=arena%entries(object_indices(1))%node% &
                                   column, &
                                   end_line=arena%entries(object_indices(1))%node% &
                                   line, &
                                   end_column=arena%entries(object_indices(1))% &
                                   node%column + 1)
                    call this%errors%add_result(error_result)
                end block
                return
            end if

            cursor = 1
            synthetic_assignment%operator = "="

            do i = 1, size(object_indices)
                if (object_indices(i) <= 0) cycle
                if (cursor > value_count) exit

                synthetic_assignment%target_index = object_indices(i)
                synthetic_assignment%value_index = value_indices(cursor)
                expr_typ = get_node_type(value_indices(cursor))
                updated_expr_typ = expr_typ
                cursor = cursor + 1

                call process_assignment_inference( &
                    arena, synthetic_assignment, 0, &
                    synthetic_assignment%target_index, expr_typ, &
                    updated_expr_typ, this%scopes, this%errors, &
                    this%input_mode, this%next_var_id, &
                    this%parser_type_hints)
            end do
        end subroutine infer_multi_data_objects

        function build_array_type_from_values(value_indices) result(array_type)
            integer, intent(in) :: value_indices(:)
            type(mono_type_t) :: array_type
            type(mono_type_t) :: combined_type
            type(mono_type_t) :: value_type
            type(mono_type_t), allocatable :: args(:)
            integer :: i

            if (size(value_indices) == 0) then
                array_type = create_mono_type(TINT)
                return
            end if

            combined_type = get_node_type(value_indices(1))
            if (combined_type%kind == 0) combined_type = create_mono_type(TINT)

            do i = 2, size(value_indices)
                value_type = get_node_type(value_indices(i))
                if (value_type%kind == 0) value_type = create_mono_type(TINT)
                combined_type = get_common_type(combined_type, value_type)
            end do

            if (combined_type%kind == 0) combined_type = create_mono_type(TINT)

            allocate (args(1))
            args(1) = combined_type
            array_type = create_mono_type(TARRAY, args=args, &
                                          array_size=size(value_indices))
            deallocate (args)
        end function build_array_type_from_values

        logical function get_data_target_declared_type(object_index, decl_type) &
            result(found)
            use semantic_declaration_utils, only: fetch_declaration_type
            integer, intent(in) :: object_index
            type(mono_type_t), intent(out) :: decl_type
            character(len=:), allocatable :: var_name

            found = .false.
            if (.not. arena%has_node_at(object_index)) return

            ! Get the variable name from the DATA target
            select type (target_node => arena%entries(object_index)%node)
            type is (identifier_node)
                if (allocated(target_node%name)) then
                    var_name = trim(target_node%name)
                end if
            end select

            if (.not. allocated(var_name)) return
            if (len_trim(var_name) == 0) return

            ! Look up declared type from arena
            found = fetch_declaration_type(arena, var_name, decl_type)
        end function get_data_target_declared_type

        subroutine finalize_binary_operation(node_index, expr)
            use semantic_unsigned_integer_mix_diagnostics, only: &
                emit_unsigned_integer_mix_error, extract_integer_signedness, &
                is_integer_literal_expr
            integer, intent(in) :: node_index
            type(binary_op_node), intent(in) :: expr
            type(mono_type_t) :: left_t
            type(mono_type_t) :: right_t
            type(mono_type_t) :: node_type
            type(mono_type_t) :: common_t
            logical :: left_is_int
            logical :: right_is_int
            logical :: left_is_unsigned
            logical :: right_is_unsigned
            logical :: left_is_int_literal
            logical :: right_is_int_literal

            left_t = get_node_type(expr%left_index)
            right_t = get_node_type(expr%right_index)
            node_type = infer_binary_operation(arena, node_index, expr, &
                                               left_t, right_t)

            call extract_integer_signedness(left_t, left_is_int, left_is_unsigned)
            call extract_integer_signedness(right_t, right_is_int, right_is_unsigned)

            left_is_int_literal = is_integer_literal_expr(arena, expr%left_index)
            right_is_int_literal = is_integer_literal_expr(arena, expr%right_index)

            if (left_is_int .and. right_is_int) then
                if (left_is_unsigned .neqv. right_is_unsigned) then
                    if (.not. left_is_int_literal .and. &
                        .not. right_is_int_literal) then
                        call emit_unsigned_integer_mix_error(this%errors, &
                                                             expr%line, &
                                                             expr%column)
                    end if
                end if
            end if

            ! Unify operand types based on operator kind
            if (expr%operator == "//") then
                ! String concatenation: both operands must be CHARACTER
                call this%unify(left_t, create_mono_type(TCHAR))
                call this%unify(right_t, create_mono_type(TCHAR))
            else if (expr%operator == "==" .or. expr%operator == "/=" .or. &
                     expr%operator == "<" .or. expr%operator == "<=" .or. &
                     expr%operator == ">" .or. expr%operator == ">=") then
                ! Comparison operators: operands must have compatible types
                ! For string comparisons, if either operand is TCHAR, both must be
                if (left_t%kind == TCHAR .or. right_t%kind == TCHAR) then
                    call this%unify(left_t, create_mono_type(TCHAR))
                    call this%unify(right_t, create_mono_type(TCHAR))
                else
                    ! For numeric comparisons, use common type promotion
                    common_t = get_common_type(left_t, right_t)
                    call this%unify(left_t, common_t)
                    call this%unify(right_t, common_t)
                end if
            end if

            call finalize_node(node_index, node_type)
        end subroutine finalize_binary_operation

        subroutine finalize_call_or_subscript(node_index, expr)
            use semantic_unsigned_integer_mix_diagnostics, only: &
                emit_unsigned_integer_mix_error, extract_integer_signedness, &
                is_integer_literal_expr
            use intrinsic_registry, only: get_intrinsic_signature, &
                                          is_intrinsic_function
            use string_utils_mod, only: to_lower
            integer, intent(in) :: node_index
            type(call_or_subscript_node), intent(inout) :: expr
            type(mono_type_t) :: node_type

            node_type = infer_function_call_type(arena, expr, this%scopes, &
                                                 module_get_node_type_wrapper)
            call collect_call_signature(this%signatures, arena, expr, node_type, &
                                        node_index)
            call validate_unsigned_integer_intrinsic_argument_mixing(expr)
            if (this%operating_mode == OPERATING_MODE_STRICT) then
                call ensure_explicit_interface_cache()
                call validate_explicit_interface_for_function_reference( &
                    arena, this%scopes, this%explicit_interface_procedure_names, &
                    this%errors, expr, node_index)
                call validate_strict_argument_types_for_function_reference( &
                    arena, this%errors, expr, node_index)
            end if
            call finalize_node(node_index, node_type)
        end subroutine finalize_call_or_subscript

        subroutine validate_unsigned_integer_intrinsic_argument_mixing(expr)
            use intrinsic_registry, only: get_intrinsic_signature, &
                                          is_intrinsic_function
            use semantic_unsigned_integer_mix_diagnostics, only: &
                emit_unsigned_integer_mix_error, extract_integer_signedness, &
                is_integer_literal_expr
            use string_utils_mod, only: to_lower
            type(call_or_subscript_node), intent(in) :: expr
            character(len=:), allocatable :: lowered
            character(len=:), allocatable :: sig
            character(len=:), allocatable :: args_str
            character(len=:), allocatable :: expected_arg
            integer :: open_paren
            integer :: close_paren
            integer :: i
            integer :: start
            integer :: arg_pos
            integer :: arg_index
            type(mono_type_t) :: actual_type
            logical :: actual_is_int
            logical :: actual_is_unsigned
            logical :: expected_is_int
            logical :: expected_is_unsigned
            logical :: actual_is_int_literal

            if (.not. allocated(expr%name)) return
            if (.not. allocated(expr%arg_indices)) return
            if (size(expr%arg_indices) <= 0) return

            lowered = to_lower(trim(expr%name))
            if (.not. is_intrinsic_function(lowered)) return
            if (lowered == "uint" .or. lowered == "int") return

            sig = get_intrinsic_signature(lowered)
            if (len_trim(sig) == 0) return
            if (index(sig, "unsigned_integer") == 0) return

            open_paren = index(sig, "(")
            close_paren = len_trim(sig)
            if (open_paren <= 0) return
            if (close_paren <= open_paren) return
            if (sig(close_paren:close_paren) /= ")") return

            args_str = sig(open_paren + 1:close_paren - 1)
            if (len_trim(args_str) == 0) return

            start = 1
            arg_pos = 1
            do i = 1, len_trim(args_str) + 1
                if (i <= len_trim(args_str)) then
                    if (args_str(i:i) /= ",") cycle
                end if

                expected_arg = trim(adjustl(args_str(start:i - 1)))
                start = i + 1

                expected_is_int = .false.
                expected_is_unsigned = .false.
                if (expected_arg == "integer") then
                    expected_is_int = .true.
                    expected_is_unsigned = .false.
                else if (expected_arg == "unsigned_integer") then
                    expected_is_int = .true.
                    expected_is_unsigned = .true.
                end if

                if (expected_is_int) then
                    if (arg_pos <= size(expr%arg_indices)) then
                        arg_index = expr%arg_indices(arg_pos)
                        actual_is_int_literal = is_integer_literal_expr(arena, &
                                                                        arg_index)
                        actual_type = get_node_type(arg_index)
                        call extract_integer_signedness(actual_type, &
                                                        actual_is_int, &
                                                        actual_is_unsigned)
                        if (actual_is_int) then
                            if (actual_is_unsigned .neqv. expected_is_unsigned) then
                                if (.not. actual_is_int_literal) then
                                    call emit_unsigned_integer_mix_error( &
                                        this%errors, expr%line, expr%column)
                                end if
                            end if
                        end if
                    end if
                end if

                arg_pos = arg_pos + 1
            end do
        end subroutine validate_unsigned_integer_intrinsic_argument_mixing

        subroutine finalize_subroutine_call(node_index)
            integer, intent(in) :: node_index
            type(mono_type_t) :: node_type

            if (node_index > 0 .and. node_index <= arena%size) then
                if (allocated(arena%entries(node_index)%node)) then
                    select type (call_expr => arena%entries(node_index)%node)
                    type is (subroutine_call_node)
                        call collect_subroutine_signature(this%signatures, arena, &
                                                          call_expr, node_index)
                        call validate_call_target_has_no_type(arena, this%errors, &
                                                             call_expr, node_index)
                        if (this%operating_mode == OPERATING_MODE_STRICT) then
                            call ensure_explicit_interface_cache()
                            call validate_explicit_interface_for_subroutine_call( &
                                arena, this%scopes, &
                                this%explicit_interface_procedure_names, &
                                this%errors, call_expr, node_index)
                            call validate_strict_argument_types_for_subroutine_call( &
                                arena, this%errors, call_expr, node_index)
                        end if
                    end select
                end if
            end if

            node_type = create_mono_type(TVAR, var=create_type_var(0, "error"))
            call finalize_node(node_index, node_type)
        end subroutine finalize_subroutine_call

        subroutine ensure_explicit_interface_cache()
            if (this%explicit_interface_cache_valid) then
                if (this%explicit_interface_cache_arena_size == arena%size) then
                    return
                end if
            end if

            call build_explicit_interface_name_cache( &
                arena, this%explicit_interface_procedure_names)
            this%explicit_interface_cache_arena_size = arena%size
            this%explicit_interface_cache_valid = .true.
        end subroutine ensure_explicit_interface_cache

        subroutine finalize_do_loop_node(node_index)
            integer, intent(in) :: node_index
            type(mono_type_t), allocatable :: args(:)
            type(mono_type_t) :: node_type
            type(mono_type_t) :: element_type
            type(mono_type_t) :: child_type
            integer :: parent_index
            integer :: i
            logical :: is_implied_do

            element_type = create_mono_type(TINT)
            is_implied_do = .false.

            if (node_index > 0 .and. node_index <= arena%size) then
                parent_index = arena%entries(node_index)%parent_index
                if (parent_index > 0 .and. parent_index <= arena%size) then
                    if (allocated(arena%entries(parent_index)%node)) then
                        select type (parent_node => arena%entries(parent_index)%node)
                        type is (array_literal_node)
                            is_implied_do = .true.
                        end select
                    end if
                end if
            end if

            if (is_implied_do) then
                element_type%kind = 0
                element_type%size = 0
                if (allocated(arena%entries(node_index)%node)) then
                    select type (loop_node => arena%entries(node_index)%node)
                    type is (do_loop_node)
                        if (allocated(loop_node%body_indices)) then
                            do i = 1, size(loop_node%body_indices)
                                child_type = get_node_type(loop_node%body_indices(i))
                                if (child_type%kind == 0) cycle
                                if (element_type%kind == 0) then
                                    element_type = child_type
                                else
                                    element_type = &
                                        get_common_type(element_type, child_type)
                                end if
                            end do
                        end if
                    end select
                end if
                if (element_type%kind == 0) element_type = create_mono_type(TINT)
            end if

            allocate (args(1))
            args(1) = element_type
            node_type = create_mono_type(TARRAY, args=args)
            call finalize_node(node_index, node_type)
            if (allocated(args)) deallocate (args)
        end subroutine finalize_do_loop_node

        subroutine finalize_function_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(function_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(mono_type_t) :: node_type
            type(poly_type_t) :: func_scheme

            node_type = build_function_type(current)
            call finalize_node(node_index, node_type)
            if (allocated(expr%name)) then
                func_scheme = this%generalize(node_type)
                call this%scopes%define(trim(expr%name), func_scheme)
            end if
        end subroutine finalize_function_definition
