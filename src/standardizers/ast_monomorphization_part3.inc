
    function determine_result_name(orig_func, mangled_name) result(result_name)
        type(function_def_node), pointer, intent(in) :: orig_func
        character(len=*), intent(in) :: mangled_name
        character(len=:), allocatable :: result_name

        if (allocated(orig_func%result_variable)) then
            if (len_trim(orig_func%result_variable) > 0) then
                result_name = trim(orig_func%result_variable)
                return
            end if
        end if

        if (allocated(orig_func%name)) then
            if (len_trim(orig_func%name) > 0) then
                result_name = trim(orig_func%name)
                return
            end if
        end if

        result_name = mangled_name
    end function determine_result_name

    function clone_function_body_with_updated_result(arena, orig_func, &
                                                     result_name, return_type) &
        result(body_indices_copy)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), pointer, intent(in) :: orig_func
        character(len=*), intent(in) :: result_name
        character(len=*), intent(in) :: return_type
        integer, allocatable :: body_indices_copy(:)
        character(len=:), allocatable :: lowered_return
        integer :: i
        type(declaration_node) :: decl_copy
        logical :: has_result_decl
        integer :: new_decl_idx
        integer, allocatable :: dim_indices(:)
        integer, allocatable :: temp_indices(:)
        character(len=:), allocatable :: decl_type

        if (len_trim(return_type) > 0) then
            lowered_return = to_lower(return_type)
        else
            lowered_return = ""
        end if

        has_result_decl = .false.
        if (allocated(orig_func%body_indices)) then
            allocate (body_indices_copy(size(orig_func%body_indices)))
            do i = 1, size(orig_func%body_indices)
                body_indices_copy(i) = orig_func%body_indices(i)
                if (body_indices_copy(i) < 1) cycle
                if (body_indices_copy(i) > arena%size) cycle
                if (.not. allocated(arena%entries(body_indices_copy(i))%node)) cycle
                select type (decl => arena%entries(body_indices_copy(i))%node)
                type is (declaration_node)
                    if (len_trim(result_name) == 0) cycle
                    if (.not. allocated(decl%var_name)) cycle
                    if (trim(decl%var_name) /= trim(result_name)) cycle
                    has_result_decl = .true.
                    decl_copy = decl
                    decl_copy%uid = generate_uid()
                    if (len_trim(return_type) > 0) then
                        decl_copy%type_name = trim(return_type)
                    end if
                    decl_copy%has_kind = .false.
                    if (len_trim(lowered_return) > 0) then
                        decl_copy%is_allocatable = &
                            index(lowered_return, "allocatable") > 0
                        decl_copy%is_array = &
                            index(lowered_return, "dimension(") > 0
                    else
                        decl_copy%is_allocatable = .false.
                        decl_copy%is_array = .false.
                    end if
                    if (decl_copy%is_array) then
                        call extract_dimension_from_type_string( &
                             decl_copy%type_name, decl_copy%dimension_indices)
                        decl_copy%type_name = strip_dimension_from_type( &
                             decl_copy%type_name)
                    else
                        if (allocated(decl_copy%dimension_indices)) then
                            deallocate (decl_copy%dimension_indices)
                        end if
                    end if
                    call arena%push(decl_copy)
                    body_indices_copy(i) = arena%size
                end select
            end do
        else
            allocate (body_indices_copy(0))
        end if

        if (.not. has_result_decl) then
            if (len_trim(result_name) > 0 .and. len_trim(return_type) > 0) then
                decl_type = trim(return_type)
                call extract_dimension_from_type_string(decl_type, dim_indices)
                decl_type = strip_dimension_from_type(decl_type)
                if (allocated(dim_indices)) then
                    new_decl_idx = push_declaration( &
                        arena, decl_type, [trim(result_name)], &
                        dimension_indices=dim_indices, &
                        is_allocatable=index(lowered_return, "allocatable") > 0)
                else
                    new_decl_idx = push_declaration( &
                        arena, decl_type, [trim(result_name)], &
                        is_allocatable=index(lowered_return, "allocatable") > 0)
                end if
                if (new_decl_idx > 0) then
                    allocate (temp_indices(size(body_indices_copy) + 1))
                    temp_indices(1) = new_decl_idx
                    if (size(body_indices_copy) > 0) then
                        temp_indices(2:) = body_indices_copy
                    end if
                    call move_alloc(temp_indices, body_indices_copy)
                end if
            end if
        end if
    end function clone_function_body_with_updated_result

    subroutine extract_dimension_from_type_string(type_name, dimension_indices)
        character(len=:), allocatable, intent(inout) :: type_name
        integer, allocatable, intent(out) :: dimension_indices(:)
        integer :: dim_pos, paren_start, paren_end, ndims, i, comma_count
        character(len=:), allocatable :: base_type, dim_spec
        integer :: has_colon

        ! Safety checks
        if (.not. allocated(type_name)) return
        if (len_trim(type_name) == 0) return

        ! Find ", dimension(" in the type string
        dim_pos = index(to_lower(type_name), ", dimension(")
        if (dim_pos == 0) then
            ! Try without comma (could be at start)
            dim_pos = index(to_lower(type_name), "dimension(")
            if (dim_pos /= 1) return  ! Not at start either, give up
            base_type = ""
            paren_start = 10  ! len("dimension(")
        else
            ! Extract base type (everything before ", dimension(")
            base_type = trim(type_name(1:dim_pos - 1))
            paren_start = dim_pos + 12  ! len(", dimension(")
        end if

        ! Find the matching closing parenthesis
        paren_end = index(type_name(paren_start:), ")")
        if (paren_end == 0) return  ! No closing paren found
        paren_end = paren_start + paren_end - 2  ! Adjust to global position

        ! Extract dimension specification (e.g., ":", "3", ":,:","3,5")
        if (paren_end >= paren_start) then
            dim_spec = type_name(paren_start:paren_end)
        else
            dim_spec = ""
        end if

        ! Count dimensions by counting colons or commas
        ! First check if we have any colons (assumed-shape)
        has_colon = 0
        do i = 1, len(dim_spec)
            if (dim_spec(i:i) == ':') has_colon = 1
        end do

        if (has_colon > 0) then
            ! Count colons for assumed-shape arrays: dimension(:) or dimension(:,:)
            ndims = 0
            do i = 1, len(dim_spec)
                if (dim_spec(i:i) == ':') ndims = ndims + 1
            end do
        else
            ! Count commas + 1 for explicit-shape arrays: dimension(3) or dimension(3,5)
            comma_count = 0
            do i = 1, len(dim_spec)
                if (dim_spec(i:i) == ',') comma_count = comma_count + 1
            end do
            ndims = comma_count + 1
        end if

        ! Allocate dimension_indices if we found dimensions
        if (ndims > 0) then
            allocate (dimension_indices(ndims))
            dimension_indices = 0  ! 0 means assumed-shape (:) for now
        end if

        ! Update type_name to just the base type
        if (len_trim(base_type) > 0) then
            type_name = base_type
        else if (dim_pos == 1) then
            ! dimension(...) was at the start, extract just "integer" or whatever
            ! This shouldn't happen in practice but handle it
            type_name = "real"  ! default fallback
        end if
    end subroutine extract_dimension_from_type_string

    function clone_parameter_with_kind(arena, param_idx, kind_value, type_override) &
        result(new_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: param_idx
        integer, intent(in) :: kind_value
        character(len=*), intent(in), optional :: type_override
        integer :: new_idx
        type(parameter_declaration_node), pointer :: orig_param
        type(parameter_declaration_node) :: new_param
        character(len=:), allocatable :: type_name
        logical :: override_provided
        logical :: is_array_kind

        call get_parameter_node(arena, param_idx, orig_param)
        if (.not. associated(orig_param)) then
            new_idx = 0
            return
        end if

        new_param = orig_param
        new_param%uid = generate_uid()

        override_provided = .false.
        if (present(type_override)) then
            if (len_trim(type_override) > 0) then
                new_param%type_name = trim(type_override)
                override_provided = .true.
            end if
        end if

        if (.not. override_provided) then
            type_name = get_kind_type_string(kind_value)
            if (len_trim(type_name) == 0) then
                if (allocated(orig_param%type_name)) then
                    if (len_trim(orig_param%type_name) > 0) then
                        type_name = trim(orig_param%type_name)
                    end if
                end if
            end if
            if (len_trim(type_name) == 0) type_name = "real"
            new_param%type_name = trim(type_name)
        end if

        is_array_kind = (kind_value == TARRAY)
        if (override_provided) then
            new_param%has_kind = .false.
            new_param%kind_value = 0
            ! Extract dimension info from type string if present
            ! This handles cases where semantic analysis provides type strings like:
            ! "real, dimension(:)" or "integer, dimension(:)"
            if (index(to_lower(new_param%type_name), "dimension(") > 0) then
                ! Extract dimension info before stripping (for assumed-shape output)
                call extract_dimension_from_type_string(new_param%type_name, &
                                                        new_param%dimension_indices)
                ! Strip dimension() from type_name to avoid duplication
                ! The dimension spec will come from dimension_indices instead
                new_param%type_name = strip_dimension_from_type(new_param%type_name)
                new_param%is_array = .true.
            end if
        else
            if (is_array_kind) then
                new_param%has_kind = .false.
                new_param%kind_value = orig_param%kind_value
            else
                new_param%kind_value = get_actual_kind_value(kind_value)
                new_param%has_kind = .true.
            end if
            if (allocated(orig_param%dimension_indices)) then
                new_param%dimension_indices = orig_param%dimension_indices
            end if
        end if

        call arena%push(new_param)
        new_idx = arena%size
    end function clone_parameter_with_kind

    function create_module_procedure_node(arena, func_name, variant_indices) &
        result(idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: variant_indices(:)
        integer :: idx
        type(module_procedure_node) :: mod_proc
        type(string_t), allocatable :: proc_names(:)
        integer :: i

        allocate (proc_names(size(variant_indices)))
        do i = 1, size(variant_indices)
            proc_names(i)%s = get_procedure_name_from_arena(arena, &
                                                            variant_indices(i))
        end do

        mod_proc = create_module_procedure(proc_names, has_module_prefix=.true., &
                                           has_double_colon=.false.)
        call arena%push(mod_proc)
        idx = arena%size
    end function create_module_procedure_node

    function create_interface_node(arena, name, mod_proc_idx) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: mod_proc_idx
        integer :: idx
        type(interface_block_node) :: iface
        integer :: proc_indices(1)

        proc_indices(1) = mod_proc_idx
        iface = create_interface_block( &
                name=name, &
                procedure_indices=proc_indices)

        call arena%push(iface)
        idx = arena%size
    end function create_interface_node

    function create_module_for_function(arena, func_name, interface_idx, &
                                        variant_indices) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: interface_idx
        integer, intent(in) :: variant_indices(:)
        integer :: idx
        type(module_node) :: mod
        integer :: decl_indices(1)

        decl_indices(1) = interface_idx

        mod = create_module( &
              name="auto_"//func_name, &
              declaration_indices=decl_indices, &
              procedure_indices=variant_indices, &
              has_contains=.true.)

        call arena%push(mod)
        idx = arena%size
    end function create_module_for_function

    function create_use_statement_node(arena, module_name) result(idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        integer :: idx
        type(use_statement_node) :: use_stmt

        use_stmt = create_use_statement(module_name)
        call arena%push(use_stmt)
        idx = arena%size
    end function create_use_statement_node

    function determine_return_type_string(arena, signature, orig_func) &
        result(type_str)
        type(ast_arena_t), intent(inout) :: arena
        type(type_signature_t), intent(in) :: signature
        type(function_def_node), pointer, intent(in) :: orig_func
        character(len=:), allocatable :: type_str
        type(call_or_subscript_node), pointer :: call_node
        type(mono_type_t) :: inferred_type
        logical :: has_value
        logical :: return_is_array_kind

        type_str = ""
        return_is_array_kind = .false.

        if (allocated(signature%return_type_string)) then
            if (len_trim(signature%return_type_string) > 0) then
                type_str = trim(signature%return_type_string)
                if (index(to_lower(type_str), 'dimension(') > 0 .or. &
                    index(type_str, '(:)') > 0) then
                    return_is_array_kind = .true.
                end if
            end if
        end if

        if (len_trim(type_str) == 0) then
            if (signature%call_site_node > 0) then
                call get_call_node(arena, signature%call_site_node, call_node)
                if (associated(call_node)) then
                    inferred_type = call_node%inferred_type
                    call inferred_type%sync_from_arena()
                    if (inferred_type%kind == TARRAY) return_is_array_kind = .true.
                    type_str = trim(mono_type_to_string(inferred_type, &
                                                        include_shape=.true., &
                                                        fallback=''))
                    if (len_trim(type_str) == 0) then
                        ! Only use get_kind_type_string for non-array types
                        ! For arrays, mono_type_to_string should have handled it
                        if (inferred_type%kind /= TARRAY) then
                            type_str = get_kind_type_string(inferred_type%kind)
                        end if
                    end if
                end if
            end if
        end if

        if (len_trim(type_str) == 0) then
            has_value = associated(orig_func)
            if (has_value) has_value = allocated(orig_func%return_type)
            if (has_value) then
                if (len_trim(orig_func%return_type) > 0) then
                    type_str = trim(orig_func%return_type)
                    if (index(to_lower(type_str), 'dimension(') > 0 .or. &
                        index(type_str, '(:)') > 0) then
                        return_is_array_kind = .true.
                    end if
                end if
            end if
        end if

        if (len_trim(type_str) == 0 .and. associated(orig_func)) then
            type_str = trim(mono_type_to_string(orig_func%inferred_type, &
                                                include_shape=.true., fallback=''))
            if (orig_func%inferred_type%kind == TARRAY) then
                return_is_array_kind = .true.
            end if
        end if

        if (len_trim(type_str) == 0) then
            ! Only use get_kind_type_string for non-array types
            ! For TARRAY, we need dimension information
            if (signature%return_kind /= TARRAY) then
                type_str = get_kind_type_string(signature%return_kind)
            end if
        end if

        if (signature%return_kind == TARRAY) then
            if (len_trim(type_str) == 0) then
                return_is_array_kind = .true.
            end if
        end if

                if (allocated(signature%param_kinds)) then
                    if (size(signature%param_kinds) > 0) then
                        if (len_trim(type_str) == 0) then
                            type_str = fallback_return_type_from_params( &
                                signature%param_kinds)
                else
                    ! For monomorphization, check if return type matches parameters
                    ! If not, use parameter-based return type (fixes #2142)
                    block
                        character(len=:), allocatable :: param_return_type
                        integer :: dominant_kind

                        param_return_type = fallback_return_type_from_params( &
                                            signature%param_kinds)

                        ! Get dominant kind from parameters
                        dominant_kind = 0
                        if (any(signature%param_kinds == 9)) then  ! TDOUBLE
                            dominant_kind = 9
                        else if (any(signature%param_kinds == 8)) then  ! TCOMPLEX
                            dominant_kind = 8
                        else if (any(signature%param_kinds == 3)) then  ! TREAL
                            dominant_kind = 3
                        else if (any(signature%param_kinds == 4)) then  ! TCHAR
                            dominant_kind = 4
                        else if (any(signature%param_kinds == 5)) then  ! TLOGICAL
                            dominant_kind = 5
                        else if (any(signature%param_kinds == 2)) then  ! TINT
                            dominant_kind = 2
                        end if

                        ! If return_kind doesn't match dominant parameter kind,
                        ! use parameter-based return type
                        if (dominant_kind > 0 .and. signature%return_kind /= &
                            dominant_kind) then
                            type_str = param_return_type
                        end if
                    end block
                end if

                ! If any parameter is an array, return type should also be array
                ! (for element-wise operations like x + 1)
                if (allocated(signature%param_type_strings)) then
                    block
                        integer :: i
                        logical :: has_array_param
                        logical :: all_params_array
                        logical :: all_kinds_array
                        logical :: orig_params_all_array
                        type(parameter_declaration_node), pointer :: orig_param
                        character(len=:), allocatable :: scalar_type_candidate
                        has_array_param = .false.
                        all_params_array = .true.
                        all_kinds_array = .true.
                        orig_params_all_array = .true.
                        scalar_type_candidate = ""
                        do i = 1, size(signature%param_type_strings)
                            if (index(signature%param_type_strings(i), &
                                      'dimension') > 0 .or. &
                                index(signature%param_type_strings(i), '(:)') > 0) then
                                has_array_param = .true.
                            else
                                all_params_array = .false.
                                if (len_trim(scalar_type_candidate) == 0) then
                                    scalar_type_candidate = trim( &
                                        signature%param_type_strings(i))
                                end if
                                exit
                            end if
                        end do
                        if (allocated(signature%param_kinds)) then
                            do i = 1, size(signature%param_kinds)
                                if (signature%param_kinds(i) /= TARRAY) then
                                    all_kinds_array = .false.
                                    exit
                                end if
                            end do
                        end if
                        if (associated(orig_func)) then
                            if (allocated(orig_func%param_indices)) then
                                do i = 1, size(orig_func%param_indices)
                                    call get_parameter_node( &
                                         arena, orig_func%param_indices(i), &
                                         orig_param)
                                    if (.not. associated(orig_param)) cycle
                                    if (.not. orig_param%is_array) then
                                        orig_params_all_array = .false.
                                        exit
                                    end if
                                end do
                            end if
                        end if
                        if (.not. all_params_array) then
                            if (len_trim(scalar_type_candidate) > 0) then
                                type_str = strip_dimension_from_type( &
                                           scalar_type_candidate)
                            else
                                type_str = strip_dimension_from_type(type_str)
                            end if
                            return_is_array_kind = .false.
                        end if

                        if (has_array_param .and. all_params_array .and. &
                            all_kinds_array .and. orig_params_all_array .and. &
                            return_is_array_kind) then
                            ! Return type should match element type with array dimension
                            if (index(type_str, 'dimension') == 0 .and. &
                                index(type_str, '(:)') == 0) then
                                ! Add assumed-shape allocatable array dimension
                                ! allocatable is required for codegen to emit &
                                ! result clause
                                type_str = trim(type_str) // ", dimension(:), " // &
                                           "allocatable"
                            end if
                        end if
                    end block
                end if
            end if
        end if

        if (len_trim(type_str) == 0) type_str = "real"

        call update_call_result_type(arena, signature%call_site_node, &
                                     signature%return_kind, type_str)
    end function determine_return_type_string

    subroutine update_call_result_type(arena, call_index, return_kind, type_str)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: call_index
        integer, intent(in) :: return_kind
        character(len=*), intent(in) :: type_str
        integer :: resolved_kind
        type(call_or_subscript_node), pointer :: call_node

        if (.not. arena%has_node_at(call_index)) return
        call_node => null()
        select type (node => arena%entries(call_index)%node)
        type is (call_or_subscript_node)
            call_node => node
        class default
            return
        end select
        if (.not. associated(call_node)) return

        resolved_kind = return_kind
        if (resolved_kind <= 0) then
            resolved_kind = get_kind_from_type_string(type_str)
        end if
        if (resolved_kind <= 0) return
        if (resolved_kind == TARRAY) return

        call_node%inferred_type = create_mono_type(resolved_kind)
    end subroutine update_call_result_type

    integer function get_kind_from_type_string(type_name) result(kind_value)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_name))

        select case (lowered)
        case ('integer')
            kind_value = TINT
        case ('real')
            kind_value = TREAL
        case ('logical')
            kind_value = TLOGICAL
        case ('complex')
            kind_value = TCOMPLEX
        case ('real(8)')
            kind_value = TDOUBLE
        case ('double precision')
            kind_value = TDOUBLE
        case default
            if (index(lowered, 'character') == 1) then
                kind_value = TCHAR
            else if (index(lowered, 'real(8)') == 1) then
                kind_value = TDOUBLE
            else if (index(lowered, 'real(') == 1) then
                kind_value = TREAL
            else if (index(lowered, 'integer') == 1) then
                kind_value = TINT
            else
                kind_value = 0
            end if
        end select
    end function get_kind_from_type_string

    function fallback_return_type_from_params(param_kinds) result(type_str)
        integer, intent(in) :: param_kinds(:)
        character(len=:), allocatable :: type_str
        integer :: i
        integer :: best_rank

        type_str = ""
        best_rank = -1

        do i = 1, size(param_kinds)
            select case (param_kinds(i))
            case (9)  ! TDOUBLE
                if (best_rank < 4) then
                    type_str = "real(8)"
                    best_rank = 4
                end if
            case (8)  ! TCOMPLEX
                if (best_rank < 3) then
                    type_str = "complex"
                    best_rank = 3
                end if
            case (3)  ! TREAL
                if (best_rank < 2) then
                    type_str = "real"
                    best_rank = 2
                end if
            case (4)  ! TCHAR
                if (best_rank < 2) then
                    type_str = "character"
                    best_rank = 2
                end if
            case (5)  ! TLOGICAL
                if (best_rank < 1) then
                    type_str = "logical"
                    best_rank = 1
                end if
            case (2)  ! TINT
                if (best_rank < 0) then
                    type_str = "integer"
                    best_rank = 0
                end if
            case default
                if (best_rank < 0) then
                    type_str = get_kind_type_string(param_kinds(i))
                    if (len_trim(type_str) > 0) best_rank = 0
                end if
            end select
            if (best_rank == 4) exit
        end do
    end function fallback_return_type_from_params

    function get_kind_type_string(kind_value) result(type_str)
        integer, intent(in) :: kind_value
        character(len=:), allocatable :: type_str

        ! kind_value is from type_constants: TINT=2, TREAL=3, TCHAR=4, TLOGICAL=5
        select case (kind_value)
        case (2)  ! TINT
            type_str = "integer"
        case (3)  ! TREAL
            type_str = "real"
        case (4)  ! TCHAR
            type_str = "character"
        case (5)  ! TLOGICAL
            type_str = "logical"
        case (8)  ! TCOMPLEX
            type_str = "complex"
        case (9)  ! TDOUBLE
            type_str = "real(8)"
        case default
            type_str = ""
        end select
    end function get_kind_type_string

    function get_actual_kind_value(encoded_kind) result(kind_val)
        integer, intent(in) :: encoded_kind
        integer :: kind_val

        ! For type constants (TINT=2, TREAL=3, etc.), default Fortran kinds
        select case (encoded_kind)
        case (2)  ! TINT
            kind_val = 4  ! integer(4)
        case (3)  ! TREAL
            kind_val = 8  ! real(8)
        case (4)  ! TCHAR
            kind_val = 1  ! character(len=1)
        case (5)  ! TLOGICAL
            kind_val = 4  ! logical(4)
        case default
            kind_val = 4  ! Default to integer(4)
        end select
    end function get_actual_kind_value

    function get_procedure_signatures(signatures, proc_name) result(sigs)
        type(signatures_map_t), intent(in) :: signatures
        character(len=*), intent(in) :: proc_name
        type(type_signature_t), allocatable :: sigs(:)
        integer :: num_sigs

        num_sigs = signatures%get_signatures(proc_name, sigs)

        if (num_sigs == 0 .and. .not. allocated(sigs)) then
            allocate (sigs(0))
        end if
    end function get_procedure_signatures

    subroutine get_program_node(arena, idx, node_ptr)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(program_node), pointer, intent(out) :: node_ptr

        nullify (node_ptr)
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (program_node)
            node_ptr => n
        end select
    end subroutine get_program_node

    subroutine get_function_node(arena, idx, node_ptr)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(function_def_node), pointer, intent(out) :: node_ptr

        nullify (node_ptr)
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (function_def_node)
            node_ptr => n
        end select
    end subroutine get_function_node

    subroutine get_subroutine_node(arena, idx, node_ptr)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(subroutine_def_node), pointer, intent(out) :: node_ptr

        nullify (node_ptr)
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (subroutine_def_node)
            node_ptr => n
        end select
    end subroutine get_subroutine_node

    subroutine get_call_node(arena, idx, node_ptr)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(call_or_subscript_node), pointer, intent(out) :: node_ptr

        nullify (node_ptr)
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (call_or_subscript_node)
            node_ptr => n
        end select
    end subroutine get_call_node

    subroutine get_parameter_node(arena, idx, node_ptr)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(parameter_declaration_node), pointer, intent(out) :: node_ptr

        nullify (node_ptr)
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (parameter_declaration_node)
            node_ptr => n
        end select
    end subroutine get_parameter_node

    function is_function_node(arena, idx) result(is_func)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        logical :: is_func

        is_func = .false.
        if (idx < 1 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (n => arena%entries(idx)%node)
        type is (function_def_node)
            is_func = .true.
        end select
    end function is_function_node

    function get_function_name(arena, idx) result(name)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: name
        type(function_def_node), pointer :: func

        call get_function_node(arena, idx, func)
        if (associated(func)) then
            name = func%name
        else
            name = ""
        end if
    end function get_function_name

    function get_procedure_name_from_arena(arena, idx) result(name)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: name

        if (idx < 1 .or. idx > arena%size) then
            name = ""
            return
        end if
        if (.not. allocated(arena%entries(idx)%node)) then
            name = ""
            return
        end if

        name = get_procedure_name(arena%entries(idx)%node)
    end function get_procedure_name_from_arena

    logical function debug_logging_enabled() result(active)
        character(len=64) :: val
        character(len=64) :: lowered
        integer :: stat

        if (.not. debug_initialized) then
            val = ''
            call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
            if (stat == 0) then
                lowered = to_lower(adjustl(trim(val)))
                select case (trim(lowered))
                case ('0', 'false', 'off', 'no', '')
                    debug_enabled = .false.
                case default
                    debug_enabled = .true.
                end select
            else
                debug_enabled = .false.
            end if
            debug_initialized = .true.
        end if

        active = debug_enabled
    end function debug_logging_enabled

    subroutine resize_integer_array(arr, new_size)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: new_size
        integer, allocatable :: temp(:)
        integer :: old_size

        old_size = size(arr)
        allocate (temp(new_size))
        temp(1:old_size) = arr(1:old_size)
        call move_alloc(temp, arr)
    end subroutine resize_integer_array

    subroutine resize_character_array(arr, new_size)
        character(len=*), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: new_size
        integer :: old_size, limit, i
        character(len=len(arr(1))), allocatable :: temp(:)

        old_size = size(arr)
        allocate (temp(new_size))
        limit = min(old_size, new_size)
        temp = ''
        do i = 1, limit
            temp(i) = arr(i)
        end do
        call move_alloc(temp, arr)
    end subroutine resize_character_array

    ! Strip dimension() attribute from type string
    ! e.g., "integer, dimension(5), intent(in)" -> "integer, intent(in)"
    !       "real, dimension(3,4)" -> "real"
    function strip_dimension_from_type(type_str) result(stripped)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: stripped
        integer :: dim_start, paren_end, paren_count, i
        character(len=MAX_DIMENSION_BUFFER_LEN) :: before_dim, after_dim

        stripped = type_str

        ! Find "dimension("
        dim_start = index(to_lower(type_str), "dimension(")
        if (dim_start == 0) return

        ! Find the matching closing parenthesis
        paren_count = 1
        paren_end = 0
        do i = dim_start + len("dimension("), len(type_str)
            if (type_str(i:i) == '(') paren_count = paren_count + 1
            if (type_str(i:i) == ')') then
                paren_count = paren_count - 1
                if (paren_count == 0) then
                    paren_end = i
                    exit
                end if
            end if
        end do

        if (paren_end == 0) return  ! Malformed, return as-is

        ! Extract parts before and after dimension()
        before_dim = ''
        after_dim = ''
        if (dim_start > 1) then
            before_dim = type_str(1:dim_start - 1)
        end if
        if (paren_end < len(type_str)) then
            after_dim = type_str(paren_end + 1:len(type_str))
        end if

        ! Remove leading/trailing commas and spaces
        before_dim = adjustl(trim(before_dim))
        after_dim = adjustl(trim(after_dim))

        ! Remove trailing comma from before_dim
        if (len_trim(before_dim) > 0) then
            if (before_dim(len_trim(before_dim):len_trim(before_dim)) == ',') then
                before_dim = before_dim(1:len_trim(before_dim) - 1)
            end if
        end if

        ! Remove leading comma from after_dim
        if (len_trim(after_dim) > 0) then
            if (after_dim(1:1) == ',') then
                after_dim = after_dim(2:len_trim(after_dim))
            end if
        end if

        ! Reconstruct the type string
        if (len_trim(before_dim) > 0 .and. len_trim(after_dim) > 0) then
            stripped = trim(before_dim) // ', ' // trim(after_dim)
        else if (len_trim(before_dim) > 0) then
            stripped = trim(before_dim)
        else if (len_trim(after_dim) > 0) then
            stripped = trim(after_dim)
        else
            stripped = ''
        end if
    end function strip_dimension_from_type
