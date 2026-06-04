
    subroutine process_container_entry(arena, signatures, child_idx, module_indices, &
                                       module_count, module_names, preserved_indices, &
                                       preserved_count, &
                                       program_indices, program_count, updated_idx)
        type(ast_arena_t), intent(inout) :: arena
        type(signatures_map_t), intent(in) :: signatures
        integer, intent(in) :: child_idx
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)
        integer, allocatable, intent(inout) :: preserved_indices(:)
        integer, intent(inout) :: preserved_count
        integer, allocatable, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer, intent(out) :: updated_idx
        logical :: handled
        integer :: prog_idx

        updated_idx = child_idx
        if (child_idx < 1 .or. child_idx > arena%size) return
        if (.not. allocated(arena%entries(child_idx)%node)) return

        select type (node => arena%entries(child_idx)%node)
        type is (function_def_node)
            call process_specializable_procedure(arena, signatures, child_idx, &
                                                 node%name, .true., handled, &
                                                 module_indices, &
                                                 module_count, &
                                                 module_names)
            if (.not. handled) then
                preserved_count = preserved_count + 1
                if (preserved_count > size(preserved_indices)) then
                    call resize_integer_array(preserved_indices, preserved_count * 2)
                end if
                preserved_indices(preserved_count) = child_idx
            end if
        type is (subroutine_def_node)
            call process_specializable_procedure(arena, signatures, child_idx, &
                                                 node%name, .false., handled, &
                                                 module_indices, &
                                                 module_count, &
                                                 module_names)
            if (.not. handled) then
                preserved_count = preserved_count + 1
                if (preserved_count > size(preserved_indices)) then
                    call resize_integer_array(preserved_indices, preserved_count * 2)
                end if
                preserved_indices(preserved_count) = child_idx
            end if
        type is (program_node)
            prog_idx = child_idx
            call transform_monomorphization(arena, prog_idx, signatures)
            updated_idx = prog_idx
            program_count = program_count + 1
            if (program_count > size(program_indices)) then
                call resize_integer_array(program_indices, program_count * 2)
            end if
            program_indices(program_count) = prog_idx
            preserved_count = preserved_count + 1
            if (preserved_count > size(preserved_indices)) then
                call resize_integer_array(preserved_indices, preserved_count * 2)
            end if
            preserved_indices(preserved_count) = prog_idx
        class default
            preserved_count = preserved_count + 1
            if (preserved_count > size(preserved_indices)) then
                call resize_integer_array(preserved_indices, preserved_count * 2)
            end if
            preserved_indices(preserved_count) = child_idx
        end select
    end subroutine process_container_entry

    subroutine promote_double_literal_assignments(arena, prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_idx
        type(program_node), pointer :: prog
        integer :: i, stmt_idx, target_idx, value_idx
        integer :: promote_capacity
        integer :: promote_count
        character(len=64), allocatable :: promote_names(:)
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: target_name
        logical :: already_present
        integer :: j

        call get_program_node(arena, prog_idx, prog)
        if (.not. associated(prog)) return
        if (.not. allocated(prog%body_indices)) return
        if (size(prog%body_indices) == 0) return

        promote_capacity = max(1, size(prog%body_indices))
        allocate (character(len=64) :: promote_names(promote_capacity))
        promote_names = ''
        promote_count = 0

        do i = 1, size(prog%body_indices)
            stmt_idx = prog%body_indices(i)
            if (stmt_idx < 1 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (assign => arena%entries(stmt_idx)%node)
            type is (assignment_node)
                value_idx = assign%value_index
                if (value_idx < 1 .or. value_idx > arena%size) cycle
                if (.not. allocated(arena%entries(value_idx)%node)) cycle
                select type (lit => arena%entries(value_idx)%node)
                type is (literal_node)
                    if (.not. allocated(lit%value)) cycle
                    lowered = to_lower(trim(lit%value))
                    if (index(lowered, 'd') <= 0) cycle
                class default
                    cycle
                end select

                target_idx = assign%target_index
                if (target_idx < 1 .or. target_idx > arena%size) cycle
                if (.not. allocated(arena%entries(target_idx)%node)) cycle
                select type (id => arena%entries(target_idx)%node)
                type is (identifier_node)
                    if (.not. allocated(id%name)) cycle
                    target_name = adjustl(trim(id%name))
                    if (len_trim(target_name) == 0) cycle
                    already_present = .false.
                    do j = 1, promote_count
                        if (trim(promote_names(j)) == target_name) then
                            already_present = .true.
                            exit
                        end if
                    end do
                    if (already_present) cycle
                    if (promote_count >= promote_capacity) then
                        call resize_character_array(promote_names, &
                                                    max(2 * promote_capacity, 1))
                        promote_capacity = size(promote_names)
                    end if
                    promote_count = promote_count + 1
                    promote_names(promote_count) = target_name
                end select
            end select
        end do

        if (promote_count == 0) return

        do i = 1, size(prog%body_indices)
            stmt_idx = prog%body_indices(i)
            if (stmt_idx < 1 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (decl => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration) cycle
                if (.not. allocated(decl%var_name)) cycle
                target_name = adjustl(trim(decl%var_name))
                if (len_trim(target_name) == 0) cycle
                do j = 1, promote_count
                    if (trim(promote_names(j)) == target_name) then
                        decl%type_name = 'double precision'
                        decl%has_kind = .false.
                        decl%kind_value = 0
                        exit
                    end if
                end do
            end select
        end do
    end subroutine promote_double_literal_assignments

    subroutine finalize_monomorphized_root(arena, root_index, root_prog, &
                                           is_multi_unit, preserved_indices, &
                                           preserved_count, &
                                           module_indices, &
                                           module_count, module_names, &
                                           program_indices, &
                                           program_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(program_node), pointer, intent(inout) :: root_prog
        logical, intent(in) :: is_multi_unit
        integer, intent(in) :: preserved_indices(:)
        integer, intent(in) :: preserved_count
        integer, intent(in) :: module_indices(:)
        integer, intent(in) :: module_count
        character(len=*), intent(in) :: module_names(:)
        integer, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer, allocatable :: final_body(:)
        integer :: original_root_index
        integer :: i, child_idx

        if (is_multi_unit) then
            allocate (final_body(module_count + preserved_count))
            final_body(1:module_count) = module_indices(1:module_count)
            if (preserved_count > 0) then
                final_body(module_count + 1:) = preserved_indices(1:preserved_count)
            end if
            root_prog%body_indices = final_body
            do i = 1, module_count
                call set_parent_if_valid(arena, module_indices(i), root_index)
            end do
        else
            root_prog%body_indices = preserved_indices(1:preserved_count)
            original_root_index = root_index
            call create_multi_unit_root(arena, root_index, module_indices, &
                                        module_count, original_root_index, &
                                        root_prog%line, &
                                        root_prog%column)
            call get_program_node(arena, root_index, root_prog)
            program_count = 1
            program_indices(1) = original_root_index
        end if

        if (program_count == 0) then
            if (is_multi_unit) then
                do i = 1, size(root_prog%body_indices)
                    child_idx = root_prog%body_indices(i)
                    if (child_idx < 1 .or. child_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(child_idx)%node)) cycle
                    select type (prog_node => arena%entries(child_idx)%node)
                    type is (program_node)
                        if (trim(prog_node%name) /= "__MULTI_UNIT__") then
                            program_count = program_count + 1
                            program_indices(program_count) = child_idx
                        end if
                    end select
                end do
            else
                program_count = program_count + 1
                program_indices(program_count) = root_index
            end if
        end if

        do i = 1, module_count
            call set_parent_if_valid(arena, module_indices(i), root_index)
        end do

        do i = 1, program_count
            call ensure_program_has_uses(arena, program_indices(i), &
                                         module_names, module_count)
        end do
    end subroutine finalize_monomorphized_root

    subroutine create_multi_unit_root(arena, root_index, module_indices, &
                                      module_count, original_prog_idx, line, column)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer, intent(in) :: module_indices(:)
        integer, intent(in) :: module_count
        integer, intent(in) :: original_prog_idx
        integer, intent(in) :: line
        integer, intent(in) :: column
        integer, allocatable :: body(:)
        type(program_node) :: multi_prog
        integer :: i

        allocate (body(module_count + 1))
        if (module_count > 0) then
            body(1:module_count) = module_indices(1:module_count)
        end if
        body(module_count + 1) = original_prog_idx

        multi_prog = create_program("__MULTI_UNIT__", body, line=line, &
                                    column=column)
        call arena%push(multi_prog)
        root_index = arena%size
        do i = 1, module_count
            call set_parent_if_valid(arena, module_indices(i), root_index)
        end do
        call set_parent_if_valid(arena, original_prog_idx, root_index)
    end subroutine create_multi_unit_root

    subroutine ensure_program_has_uses(arena, prog_idx, module_names, module_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_idx
        character(len=*), intent(in) :: module_names(:)
        integer, intent(in) :: module_count
        type(program_node), pointer :: prog
        logical, allocatable :: need_use(:)
        integer :: i, insert_count
        integer, allocatable :: inserted_indices(:)
        integer :: orig_size
        integer, allocatable :: updated_body(:)
        integer, allocatable :: original_body_indices(:)

        if (module_count <= 0) return

        call get_program_node(arena, prog_idx, prog)
        if (.not. associated(prog)) return

        ! CRITICAL: Copy body_indices before ANY arena modifications
        ! create_use_statement_node pushes to arena which may invalidate prog pointer
        if (allocated(prog%body_indices)) then
            allocate (original_body_indices(size(prog%body_indices)))
            original_body_indices = prog%body_indices
        else
            allocate (original_body_indices(0))
        end if

        allocate (need_use(module_count))
        need_use = .true.

        ! Use the copied indices (safe from arena modifications)
        do i = 1, size(original_body_indices)
            if (original_body_indices(i) < 1 .or. &
                original_body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(original_body_indices(i))%node)) cycle
            select type (use_node => arena%entries(original_body_indices(i))%node)
            type is (use_statement_node)
                call mark_existing_use(use_node%module_name, module_names, &
                                       need_use)
            end select
        end do

        insert_count = count(need_use)
        if (insert_count <= 0) then
            deallocate (original_body_indices)
            return
        end if

        allocate (inserted_indices(insert_count))
        insert_count = 0
        do i = 1, module_count
            if (.not. need_use(i)) cycle
            insert_count = insert_count + 1
            inserted_indices(insert_count) = create_use_statement_node( &
                                             arena, trim(module_names(i)))
            call set_parent_if_valid(arena, inserted_indices(insert_count), &
                                     prog_idx)
        end do

        ! Safe to use original_body_indices (it's a copy)
        orig_size = size(original_body_indices)
        allocate (updated_body(insert_count + orig_size))

        if (insert_count > 0) updated_body(1:insert_count) = inserted_indices
        if (orig_size > 0) then
            updated_body(insert_count + 1:) = original_body_indices
        end if

        ! Reload prog pointer before writing (arena may have been reallocated)
        call get_program_node(arena, prog_idx, prog)
        if (associated(prog)) then
            prog%body_indices = updated_body
        end if

        deallocate (original_body_indices)
    end subroutine ensure_program_has_uses

    subroutine mark_existing_use(use_name, module_names, need_use)
        character(len=*), intent(in) :: use_name
        character(len=*), intent(in) :: module_names(:)
        logical, intent(inout) :: need_use(:)
        integer :: i
        integer :: limit

        limit = min(size(module_names), size(need_use))
        do i = 1, limit
            if (.not. need_use(i)) cycle
            if (trim(module_names(i)) == trim(use_name)) then
                need_use(i) = .false.
            end if
        end do
    end subroutine mark_existing_use

    subroutine set_parent_if_valid(arena, child_idx, parent_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: child_idx
        integer, intent(in) :: parent_idx

        if (child_idx < 1 .or. child_idx > arena%size) return
        if (.not. allocated(arena%entries(child_idx)%node)) return
        arena%entries(child_idx)%parent_index = parent_idx
    end subroutine set_parent_if_valid

    subroutine normalize_signature_param_types(signature)
        type(type_signature_t), intent(inout) :: signature
        integer :: fallback_kind
        integer :: i
        integer :: mapped_kind
        character(len=:), allocatable :: temp_type_string
        logical :: standardize

        if (.not. allocated(signature%param_kinds)) return
        if (size(signature%param_kinds) == 0) return

        call get_type_standardization(standardize)
        if (standardize) then
            call promote_real_to_double(signature)
        end if

        fallback_kind = determine_fallback_kind(signature%param_kinds, &
                                                signature%param_type_strings)
        if (fallback_kind <= 0 .or. fallback_kind == TVAR) return

        do i = 1, size(signature%param_kinds)
            if (signature%param_kinds(i) == TVAR .or. &
                signature%param_kinds(i) <= 0) then
                signature%param_kinds(i) = fallback_kind
            end if
            if (allocated(signature%param_type_strings)) then
                if (i <= size(signature%param_type_strings)) then
                    if (len_trim(signature%param_type_strings(i)) == 0) cycle

                    mapped_kind = &
                        map_type_string_to_kind(signature%param_type_strings(i))
                    if (signature%param_kinds(i) == TARRAY) then
                        if (index(to_lower(signature%param_type_strings(i)), &
                                  'dimension') > 0) then
                            temp_type_string = signature%param_type_strings(i)
                            call normalize_array_type_string(temp_type_string)
                            signature%param_type_strings(i) = temp_type_string
                        end if
                    else if (mapped_kind == TVAR .or. mapped_kind /= &
                             signature%param_kinds(i)) then
                        signature%param_type_strings(i) = kind_to_string_local( &
                                                          signature%param_kinds(i))
                    end if
                    if (signature%param_kinds(i) == TCHAR) then
                        signature%param_type_strings(i) = "character(len=*)"
                    end if
                end if
            end if
        end do

        if (allocated(signature%return_type_string)) then
            if (signature%return_kind == TCHAR) then
                signature%return_type_string = "character(len=*)"
            end if
        end if

        if (signature%return_kind > 0 .and. &
            signature%return_kind /= TVAR) then
            signature%return_type_string = &
                kind_to_string_local(signature%return_kind)
        end if
    end subroutine normalize_signature_param_types

    subroutine promote_real_to_double(signature)
        type(type_signature_t), intent(inout) :: signature
        integer :: i, n, max_len
        character(len=:), allocatable :: new_strings(:)

        do i = 1, size(signature%param_kinds)
            if (signature%param_kinds(i) == TREAL) then
                signature%param_kinds(i) = TDOUBLE
            end if
        end do
        if (signature%return_kind == TREAL) then
            signature%return_kind = TDOUBLE
        end if

        if (.not. allocated(signature%param_type_strings)) return
        n = size(signature%param_type_strings)

        max_len = 32
        do i = 1, n
            if (len_trim(signature%param_type_strings(i)) > max_len) then
                max_len = len_trim(signature%param_type_strings(i))
            end if
        end do

        allocate (character(len=max_len) :: new_strings(n))
        do i = 1, n
            new_strings(i) = signature%param_type_strings(i)
        end do
        do i = 1, n
            if (signature%param_kinds(i) == TDOUBLE) then
                if (index(to_lower(new_strings(i)), 'real') == 1 .and. &
                    index(to_lower(new_strings(i)), 'real(') == 0) then
                    new_strings(i) = 'double precision'
                end if
            else if (signature%param_kinds(i) == TCOMPLEX) then
                if (to_lower(trim(new_strings(i))) == 'complex') then
                    new_strings(i) = 'complex(8)'
                end if
            else if (signature%param_kinds(i) == TARRAY) then
                call promote_array_element_type(new_strings(i))
            end if
        end do
        deallocate (signature%param_type_strings)
        signature%param_type_strings = new_strings

        if (allocated(signature%return_type_string)) then
            if (signature%return_kind == TDOUBLE) then
                if (index(to_lower(signature%return_type_string), 'real') == 1 &
                    .and. index(to_lower(signature%return_type_string), &
                    'real(') == 0) then
                    signature%return_type_string = 'double precision'
                end if
            else if (signature%return_kind == TCOMPLEX) then
                if (to_lower(trim(signature%return_type_string)) == 'complex') &
                    then
                    signature%return_type_string = 'complex(8)'
                end if
            end if
        end if
    end subroutine promote_real_to_double

    subroutine promote_array_element_type(type_str)
        character(len=*), intent(inout) :: type_str
        character(len=:), allocatable :: lowered
        integer :: dim_pos

        lowered = to_lower(trim(type_str))
        if (index(lowered, 'real') /= 1) return
        if (index(lowered, 'real(') == 1) return

        dim_pos = index(lowered, ', dimension')
        if (dim_pos == 0) dim_pos = index(lowered, ',dimension')
        if (dim_pos > 0) then
            type_str = 'double precision' // type_str(dim_pos:len_trim(type_str))
        else
            type_str = 'double precision'
        end if
    end subroutine promote_array_element_type

    subroutine normalize_array_type_string(type_string)
        character(len=:), allocatable, intent(inout) :: type_string
        character(len=:), allocatable :: base_type, dim_spec, normalized_dim_spec
        integer :: dim_pos, paren_start, paren_end, mapped_kind
        integer :: type_len

        if (.not. allocated(type_string)) return
        if (len_trim(type_string) == 0) return

        type_len = len(type_string)

        ! Find "dimension(" in the string
        dim_pos = index(to_lower(type_string), ", dimension(")
        if (dim_pos == 0) then
            dim_pos = index(to_lower(type_string), "dimension(")
            if (dim_pos /= 1) return  ! No dimension spec found
            paren_start = 10  ! len("dimension(")
            base_type = ""
        else
            base_type = trim(type_string(1:dim_pos - 1))
            paren_start = dim_pos + 12  ! len(", dimension(")
        end if

        ! Safety check - ensure paren_start is within bounds
        if (paren_start > type_len) then
            ! Malformed dimension spec, abort normalization
            return
        end if

        ! Extract base type and normalize it
        if (len_trim(base_type) > 0) then
            ! Remove attributes like intent(in), etc
            if (index(base_type, ',') > 0) then
                base_type = base_type(1:index(base_type, ',') - 1)
            end if
            base_type = trim(adjustl(base_type))

            ! Map to canonical type name
            mapped_kind = map_type_string_to_kind(base_type)
            if (mapped_kind /= TVAR .and. mapped_kind > 0) then
                base_type = kind_to_string_local(mapped_kind)
            end if
        else
            base_type = "integer"  ! default
        end if

        ! Extract dimension specification and count rank
        ! For normalization/deduplication, we normalize all arrays to assumed-shape
        ! This ensures dimension(3) and dimension(5) are treated as identical
        if (paren_start <= type_len) then
            paren_end = index(type_string(paren_start:), ")")
            if (paren_end > 0) then
                paren_end = paren_start + paren_end - 2
                if (paren_end >= paren_start .and. paren_end <= type_len) then
                    dim_spec = trim(type_string(paren_start:paren_end))
                    ! Count rank by counting commas + 1
                    ! dimension(:) has rank 1, dimension(:,:) has rank 2, etc.
                    call count_array_rank(dim_spec, normalized_dim_spec)
                else
                    normalized_dim_spec = ":"  ! default to rank-1 assumed-shape
                end if
            else
                normalized_dim_spec = ":"  ! default to rank-1 assumed-shape
            end if
        else
            normalized_dim_spec = ":"  ! default to rank-1 assumed-shape
        end if

        ! Reconstruct in canonical form with assumed-shape
        type_string = trim(base_type) // ", dimension(" // &
            trim(normalized_dim_spec) // ")"
    end subroutine normalize_array_type_string

    subroutine count_array_rank(dim_spec_in, dim_spec_out)
        character(len=*), intent(in) :: dim_spec_in
        character(len=:), allocatable, intent(out) :: dim_spec_out
        integer :: rank, i, comma_count

        ! Count rank from dimension spec
        ! Examples: "3" -> rank 1, "3,5" -> rank 2, ":" -> rank 1, ":,:" -> rank 2
        comma_count = 0
        do i = 1, len_trim(dim_spec_in)
            if (dim_spec_in(i:i) == ',') comma_count = comma_count + 1
        end do
        rank = comma_count + 1

        ! Generate assumed-shape spec for the rank
        ! rank 1 -> ":", rank 2 -> ":,:", rank 3 -> ":,:,:", etc.
        if (rank == 1) then
            dim_spec_out = ":"
        else if (rank == 2) then
            dim_spec_out = ":,:"
        else if (rank == 3) then
            dim_spec_out = ":,:,:"
        else if (rank == 4) then
            dim_spec_out = ":,:,:,:"
        else
            ! For higher ranks, build dynamically
            dim_spec_out = ":"
            do i = 2, rank
                dim_spec_out = dim_spec_out // ",:"
            end do
        end if
    end subroutine count_array_rank

    pure logical function signatures_are_identical(sig1, sig2) result(identical)
        type(type_signature_t), intent(in) :: sig1, sig2
        integer :: i

        identical = .false.

        if (allocated(sig1%param_kinds) .neqv. allocated(sig2%param_kinds)) return
        if (allocated(sig1%param_kinds)) then
            if (size(sig1%param_kinds) /= size(sig2%param_kinds)) return
            if (.not. all(sig1%param_kinds == sig2%param_kinds)) return
        end if

        if (sig1%return_kind /= sig2%return_kind) return

        if (allocated(sig1%param_type_strings) .and. &
            allocated(sig2%param_type_strings)) then
            if (size(sig1%param_type_strings) /= size(sig2%param_type_strings)) &
                return
            do i = 1, size(sig1%param_type_strings)
                if (trim(sig1%param_type_strings(i)) /= &
                    trim(sig2%param_type_strings(i))) return
            end do
        end if

        if (allocated(sig1%return_type_string) .and. &
            allocated(sig2%return_type_string)) then
            if (trim(sig1%return_type_string) /= trim(sig2%return_type_string)) &
                return
        end if

        identical = .true.
    end function signatures_are_identical

    integer function determine_fallback_kind(param_kinds, param_type_strings) &
        result(kind_value)
        integer, intent(in) :: param_kinds(:)
        character(len=*), intent(in), optional :: param_type_strings(:)
        integer :: i
        integer :: candidate

        kind_value = 0
        do i = 1, size(param_kinds)
            candidate = rank_kind(param_kinds(i))
            if (candidate > rank_kind(kind_value)) then
                kind_value = param_kinds(i)
            end if
            if (kind_value == TDOUBLE) return
        end do

        if (kind_value /= 0 .and. kind_value /= TVAR) return

        if (present(param_type_strings)) then
            do i = 1, size(param_type_strings)
                candidate = map_type_string_to_kind(param_type_strings(i))
                if (candidate == TVAR) cycle
                if (rank_kind(candidate) > rank_kind(kind_value)) then
                    kind_value = candidate
                end if
                if (kind_value == TDOUBLE) exit
            end do
        end if
        if (kind_value == 0) kind_value = TVAR
    contains
        integer function rank_kind(kind_val) result(rank)
            integer, intent(in) :: kind_val
            select case (kind_val)
            case (TDOUBLE)
                rank = 6
            case (TCOMPLEX)
                rank = 5
            case (TREAL)
                rank = 4
            case (TINT)
                rank = 3
            case (TLOGICAL)
                rank = 2
            case (TCHAR)
                rank = 1
            case default
                rank = 0
            end select
        end function rank_kind
    end function determine_fallback_kind

    integer function map_type_string_to_kind(type_str) result(kind_val)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: lowered

        lowered = to_lower(adjustl(trim(type_str)))
        if (len_trim(lowered) == 0) then
            kind_val = TVAR
            return
        end if

        if (index(lowered, 'double precision') == 1 .or. &
            index(lowered, 'real(8)') == 1 .or. &
            index(lowered, 'real(dp)') == 1 .or. &
            index(lowered, 'real(kind=8)') == 1) then
            kind_val = TDOUBLE
        else if (index(lowered, 'complex') == 1) then
            kind_val = TCOMPLEX
        else if (index(lowered, 'real') == 1) then
            kind_val = TREAL
        else if (index(lowered, 'integer') == 1) then
            kind_val = TINT
        else if (index(lowered, 'logical') == 1) then
            kind_val = TLOGICAL
        else if (index(lowered, 'character') == 1) then
            kind_val = TCHAR
        else
            kind_val = TVAR
        end if
    end function map_type_string_to_kind

    function kind_to_string_local(kind_value) result(str)
        integer, intent(in) :: kind_value
        character(len=:), allocatable :: str

        select case (kind_value)
        case (TDOUBLE)
            str = 'double precision'
        case (TCOMPLEX)
            str = 'complex'
        case (TREAL)
            str = 'real'
        case (TINT)
            str = 'integer'
        case (TLOGICAL)
            str = 'logical'
        case (TCHAR)
            str = 'character'
        case default
            str = ''
        end select
    end function kind_to_string_local

    function clone_function_with_signature(arena, func_idx, signature) &
        result(new_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_idx
        type(type_signature_t), intent(in) :: signature
        integer :: new_idx
        type(function_def_node), pointer :: orig_func
        type(function_def_node) :: new_func
        integer, allocatable :: new_param_indices(:)
        integer, allocatable :: body_indices_copy(:)
        character(len=:), allocatable :: mangled_name, return_type
        character(len=:), allocatable :: result_name

        call get_function_node(arena, func_idx, orig_func)
        if (.not. associated(orig_func)) then
            new_idx = 0
            return
        end if

        if (allocated(signature%param_type_strings)) then
            mangled_name = mangle_procedure_name(orig_func%name, &
                                                 signature%param_kinds, &
                                                 signature%param_type_strings)
        else
            mangled_name = mangle_procedure_name(orig_func%name, &
                                                 signature%param_kinds)
        end if
        return_type = determine_return_type_string(arena, signature, orig_func)

        new_param_indices = clone_function_parameters(arena, orig_func, signature)
        result_name = determine_result_name(orig_func, mangled_name)
        body_indices_copy = clone_function_body_with_updated_result( &
                            arena, orig_func, result_name, return_type)

        new_func = create_function_def( &
                   name=mangled_name, &
                   param_indices=new_param_indices, &
                   return_type=return_type, &
                   body_indices=body_indices_copy, &
                   line=orig_func%line, &
                   column=orig_func%column, &
                   result_variable=result_name)
        new_func%inferred_type = orig_func%inferred_type

        call arena%push(new_func)
        new_idx = arena%size
    end function clone_function_with_signature

    function clone_function_parameters(arena, orig_func, signature) &
        result(new_param_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), pointer, intent(in) :: orig_func
        type(type_signature_t), intent(in) :: signature
        integer, allocatable :: new_param_indices(:)

        new_param_indices = clone_parameter_list(arena, orig_func%param_indices, &
                                                 signature)
    end function clone_function_parameters

    function clone_parameter_list(arena, param_indices, signature) &
        result(new_param_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        type(type_signature_t), intent(in) :: signature
        integer, allocatable :: new_param_indices(:)
        integer :: count, i
        integer :: kind_value

        if (.not. allocated(param_indices)) then
            allocate (new_param_indices(0))
            return
        end if

        count = size(param_indices)
        allocate (new_param_indices(count))

        do i = 1, count
            if (allocated(signature%param_kinds) .and. &
                size(signature%param_kinds) > 0) then
                if (i <= size(signature%param_kinds)) then
                    kind_value = signature%param_kinds(i)
                else
                    kind_value = signature%param_kinds(size(signature%param_kinds))
                end if
            else
                kind_value = 0
            end if

            if (allocated(signature%param_type_strings)) then
                if (i <= size(signature%param_type_strings)) then
                    new_param_indices(i) = clone_parameter_with_kind( &
                                           arena, param_indices(i), kind_value, &
                                           signature%param_type_strings(i))
                    cycle
                end if
            end if

            new_param_indices(i) = clone_parameter_with_kind( &
                                   arena, param_indices(i), kind_value)
        end do
    end function clone_parameter_list

    function clone_subroutine_with_signature(arena, sub_idx, signature) &
        result(new_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: sub_idx
        type(type_signature_t), intent(in) :: signature
        integer :: new_idx
        type(subroutine_def_node), pointer :: orig_subr
        type(subroutine_def_node) :: new_subr
        integer, allocatable :: new_param_indices(:)
        integer, allocatable :: body_indices_copy(:)
        character(len=:), allocatable :: mangled_name

        call get_subroutine_node(arena, sub_idx, orig_subr)
        if (.not. associated(orig_subr)) then
            new_idx = 0
            return
        end if

        if (allocated(signature%param_type_strings)) then
            mangled_name = mangle_procedure_name(orig_subr%name, &
                                                 signature%param_kinds, &
                                                 signature%param_type_strings)
        else
            mangled_name = mangle_procedure_name(orig_subr%name, &
                                                 signature%param_kinds)
        end if

        new_param_indices = clone_parameter_list(arena, orig_subr%param_indices, &
                                                 signature)

        if (allocated(orig_subr%body_indices)) then
            allocate (body_indices_copy(size(orig_subr%body_indices)))
            body_indices_copy = orig_subr%body_indices
        else
            allocate (body_indices_copy(0))
        end if

        new_subr = create_subroutine_def( &
                   name=mangled_name, &
                   param_indices=new_param_indices, &
                   body_indices=body_indices_copy, &
                   line=orig_subr%line, &
                   column=orig_subr%column, &
                   prefix_keywords=orig_subr%prefix_keywords, &
                   is_recursive=orig_subr%is_recursive)
        new_subr%inferred_type = orig_subr%inferred_type
        if (allocated(orig_subr%bind_c_clause)) then
            new_subr%bind_c_clause = orig_subr%bind_c_clause
        end if

        call arena%push(new_subr)
        new_idx = arena%size

        call infer_intent_for_cloned_subroutine(arena, new_idx, new_subr)
    end function clone_subroutine_with_signature

    subroutine infer_intent_for_cloned_subroutine(arena, sub_idx, sub_def)
        use ast_nodes_data, only: parameter_declaration_node, INTENT_IN
        use ast_nodes_data, only: INTENT_INOUT, INTENT_OUT
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: sub_idx
        type(subroutine_def_node), intent(in) :: sub_def
        type(param_metadata_t) :: metadata
        type(parameter_declaration_node), pointer :: param_ptr
        integer :: n_params, i

        if (.not. allocated(sub_def%param_indices)) return
        n_params = size(sub_def%param_indices)
        if (n_params == 0) return

        call init_param_metadata(metadata, n_params)

        do i = 1, n_params
            call get_parameter_node(arena, sub_def%param_indices(i), param_ptr)
            if (associated(param_ptr)) then
                if (allocated(param_ptr%name)) then
                    metadata%names(i) = param_ptr%name
                else
                    write (metadata%names(i), '(a,i0)') "param", i
                end if
            else
                write (metadata%names(i), '(a,i0)') "param", i
            end if
        end do

        if (allocated(sub_def%body_indices)) then
            call infer_subroutine_parameter_intents(arena, sub_def%body_indices, &
                                                    metadata)

            do i = 1, n_params
                call get_parameter_node(arena, sub_def%param_indices(i), param_ptr)
                if (associated(param_ptr)) then
                    select case (trim(metadata%intent(i)))
                    case ("in")
                        param_ptr%intent_type = INTENT_IN
                    case ("out")
                        param_ptr%intent_type = INTENT_OUT
                    case ("inout")
                        param_ptr%intent_type = INTENT_INOUT
                    end select
                    arena%entries(sub_def%param_indices(i))%node = param_ptr
                end if
            end do
        end if
    end subroutine infer_intent_for_cloned_subroutine
