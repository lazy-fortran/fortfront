
        subroutine finalize_node(node_idx, raw_type)
            integer, intent(in) :: node_idx
            type(mono_type_t), intent(in) :: raw_type
            type(mono_type_t) :: final_type

            final_type = this%apply_subst_to_type(raw_type)
            if (final_type%kind == TVAR) then
                if (len_trim(final_type%var%name) == 0) then
                    final_type%var%name = "v" // int_to_str(final_type%var%id)
                end if
            end if
            call set_node_inferred_type(arena, node_idx, final_type)
        end subroutine finalize_node

        function get_node_type(idx) result(res_type)
            integer, intent(in) :: idx
            type(mono_type_t) :: res_type

            res_type = get_inferred_type_from_arena(this, arena, idx)
        end function get_node_type

        subroutine handle_declaration(node, node_index)
            type(declaration_node), intent(in) :: node
            integer, intent(in) :: node_index
            type(mono_type_t) :: decl_type
            type(poly_type_t) :: scheme
            integer :: j

            call process_declaration_variables(node, decl_type)
            scheme = this%generalize(decl_type)
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                do j = 1, size(node%var_names)
                    call this%scopes%define(node%var_names(j), scheme)
                end do
            else if (allocated(node%var_name)) then
                call this%scopes%define(node%var_name, scheme)
            end if
            call finalize_node(node_index, decl_type)
        end subroutine handle_declaration

        function build_function_type(frame) result(fun_type)
            type(infer_frame_t), intent(in) :: frame
            type(mono_type_t) :: fun_type
            type(mono_type_t) :: return_type
            type(mono_type_t), allocatable :: params(:)

            if (frame%has_cached_type) then
                return_type = frame%cached_type
            else
                return_type = create_mono_type(TREAL)
            end if

            if (allocated(frame%param_types)) then
                allocate (params(size(frame%param_types)))
                params = frame%param_types
            else
                allocate (params(0))
            end if

            if (size(params) == 0) then
                fun_type = create_fun_type(create_mono_type(TCHAR), return_type)
            else if (size(params) == 1) then
                fun_type = create_fun_type(params(1), return_type)
            else
                fun_type = create_fun_type(params(1), return_type)
            end if

            if (allocated(params)) deallocate (params)
        end function build_function_type
