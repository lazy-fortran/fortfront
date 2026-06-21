
    subroutine transform_monomorphization(arena, root_index, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        logical :: is_multi_unit, found
        integer :: body_size
        integer, allocatable :: body_indices(:)
        integer, allocatable :: preserved_indices(:)
        integer, allocatable :: module_indices(:)
        integer, allocatable :: program_indices(:)
        character(len=128), allocatable :: module_names(:)
        integer :: preserved_count, module_count, program_count, i, j

        call get_root_body(arena, root_index, body_indices, is_multi_unit, found)
        if (.not. found) then
            call handle_non_program_root(arena, root_index, signatures)
            return
        end if
        if (size(body_indices) == 0) then
            if (debug_logging_enabled()) then
                write (error_unit, '(A)') 'DEBUG root body not allocated'
            end if
            return
        end if
        body_size = size(body_indices)

        allocate (preserved_indices(max(1, body_size)))
        allocate (module_indices(max(1, body_size)))
        allocate (program_indices(max(1, body_size)))
        allocate (character(len=128) :: module_names(max(1, body_size)))

        preserved_count = 0
        module_count = 0
        program_count = 0

        call process_program_body_children(arena, body_indices, signatures, &
                                           preserved_indices, preserved_count, &
                                           module_indices, &
                                           module_count, module_names, &
                                           program_indices, &
                                           program_count)
        if (module_count == 0) then
            call set_root_body(arena, root_index, &
                               preserved_indices(1:preserved_count))
            return
        end if

        call finalize_monomorphized_root(arena, root_index, &
                                         is_multi_unit, preserved_indices, &
                                         preserved_count, &
                                         module_indices, module_count, module_names, &
                                         program_indices, &
                                         program_count)
    end subroutine transform_monomorphization

    ! Read the root's body indices, reporting whether the root is the multi-unit
    ! container. Both a top-level program and the container expose body_indices.
    subroutine get_root_body(arena, root_index, body_indices, is_multi_unit, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        integer, allocatable, intent(out) :: body_indices(:)
        logical, intent(out) :: is_multi_unit, found

        found = .false.
        is_multi_unit = .false.
        allocate (body_indices(0))
        if (root_index < 1 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (n => arena%entries(root_index)%node)
        type is (program_node)
            if (allocated(n%body_indices)) body_indices = n%body_indices
            found = .true.
        type is (multi_unit_container_node)
            if (allocated(n%body_indices)) body_indices = n%body_indices
            is_multi_unit = .true.
            found = .true.
        end select
    end subroutine get_root_body

    ! Replace the root's body indices in place (program or container root).
    subroutine set_root_body(arena, root_index, body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, intent(in) :: body_indices(:)

        if (root_index < 1 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (n => arena%entries(root_index)%node)
        type is (program_node)
            n%body_indices = body_indices
        type is (multi_unit_container_node)
            n%body_indices = body_indices
        end select
    end subroutine set_root_body

    ! Handle root node that is not a program node
    subroutine handle_non_program_root(arena, root_index, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        integer :: i

        if (root_index <= 0 .or. root_index > arena%size) then
            if (debug_logging_enabled()) then
                write (error_unit, '(A)') &
                    'DEBUG root_prog not associated (invalid index)'
            end if
            return
        end if

        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (container => arena%entries(root_index)%node)
        type is (mixed_construct_container_node)
            call debug_log_mixed_container(arena, container)
            call process_mixed_container(arena, root_index, signatures)
        class default
            if (debug_logging_enabled()) then
                write (error_unit, '(A)') &
                    'DEBUG root_prog not associated and not container'
            end if
        end select
    end subroutine handle_non_program_root

    ! Debug log mixed container contents
    subroutine debug_log_mixed_container(arena, container)
        type(ast_arena_t), intent(in) :: arena
        type(mixed_construct_container_node), intent(in) :: container

        if (.not. debug_logging_enabled()) return

        write (error_unit, '(A)') 'DEBUG root is mixed container'

        if (allocated(container%explicit_program_indices)) then
            call debug_log_index_array(arena, container%explicit_program_indices, &
                                       'explicit programs')
        else
            write (error_unit, '(A)') 'DEBUG explicit programs=<none>'
        end if

        if (allocated(container%implicit_declaration_indices)) then
            call debug_log_index_array(arena, &
                                       container%implicit_declaration_indices, &
                                       'implicit indices')
        else
            write (error_unit, '(A)') 'DEBUG implicit indices=<none>'
        end if
    end subroutine debug_log_mixed_container

    ! Debug log array of node indices with node types
    subroutine debug_log_index_array(arena, indices, label)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        character(len=*), intent(in) :: label
        integer :: i

        write (error_unit, '(A,*(1X,I0))') 'DEBUG ' // trim(label) // '=', indices

        do i = 1, size(indices)
            if (indices(i) > 0 .and. indices(i) <= arena%size) then
                if (allocated(arena%entries(indices(i))%node_type)) then
                    write (error_unit, '(A,1X,I0,1X,A)') &
                        'DEBUG ' // trim(label) // ' node', indices(i), &
                        trim(arena%entries(indices(i))%node_type)
                end if
            end if
        end do
    end subroutine debug_log_index_array

    subroutine process_program_body_children(arena, body_indices, signatures, &
                                             preserved_indices, preserved_count, &
                                             module_indices, &
                                             module_count, &
                                             module_names, program_indices, &
                                             program_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        type(signatures_map_t), intent(in) :: signatures
        integer, allocatable, intent(inout) :: preserved_indices(:)
        integer, intent(inout) :: preserved_count
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)
        integer, allocatable, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer :: i, child_idx
        logical :: handled
        integer, allocatable :: local_indices(:)
        character(len=:), allocatable :: proc_name

        ! Copy indices before the loop: process_specializable_procedure mutates
        ! the arena via create_procedure_variants, which can move entries.
        allocate (local_indices(size(body_indices)))
        local_indices = body_indices

        do i = 1, size(local_indices)
            child_idx = local_indices(i)
            if (child_idx < 1 .or. child_idx > arena%size) cycle
            if (.not. allocated(arena%entries(child_idx)%node)) cycle

            select type (node => arena%entries(child_idx)%node)
            type is (function_def_node)
                if (debug_logging_enabled()) then
                    write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=function', &
                        child_idx, &
                        trim(node%name)
                end if
                proc_name = node%name
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     proc_name, .true., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    preserved_count = preserved_count + 1
                    preserved_indices(preserved_count) = child_idx
                end if
            type is (program_node)
                if (debug_logging_enabled()) then
                    write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=program', &
                        child_idx, trim(node%name)
                end if
                program_count = program_count + 1
                program_indices(program_count) = child_idx
                preserved_count = preserved_count + 1
                preserved_indices(preserved_count) = child_idx
            type is (subroutine_def_node)
                if (debug_logging_enabled()) then
                    write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=subroutine', &
                        child_idx, trim(node%name)
                end if
                proc_name = node%name
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     proc_name, .false., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    preserved_count = preserved_count + 1
                    preserved_indices(preserved_count) = child_idx
                end if
            class default
                if (debug_logging_enabled()) then
                    write (error_unit, '(A,1X,I0)') 'DEBUG node=other', child_idx
                end if
                preserved_count = preserved_count + 1
                preserved_indices(preserved_count) = child_idx
            end select
        end do

        deallocate (local_indices)
    end subroutine process_program_body_children

    subroutine process_specializable_procedure(arena, signatures, proc_idx, &
                                               proc_name, is_function, handled, &
                                               module_indices, &
                                               module_count, &
                                               module_names)
        type(ast_arena_t), intent(inout) :: arena
        type(signatures_map_t), intent(in) :: signatures
        integer, intent(in) :: proc_idx
        character(len=*), intent(in) :: proc_name
        logical, intent(in) :: is_function
        logical, intent(out) :: handled
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)
        type(type_signature_t), allocatable :: proc_sigs(:)
        integer, allocatable :: variant_indices(:)
        integer :: mod_idx

        if (procedure_has_explicit_types(arena, proc_idx)) then
            handled = .false.
            return
        end if

        call debug_log_signatures(signatures, proc_name)
        proc_sigs = get_procedure_signatures(signatures, proc_name)
        call debug_log_procedure_signatures(proc_name, proc_sigs)

        if (size(proc_sigs) <= 1) then
            handled = .false.
            if (allocated(proc_sigs)) deallocate (proc_sigs)
            return
        end if

        call normalize_and_deduplicate_signatures(proc_sigs, handled)
        if (.not. handled) then
            if (allocated(proc_sigs)) deallocate (proc_sigs)
            return
        end if

        call create_procedure_variants(arena, proc_idx, is_function, proc_sigs, &
                                       variant_indices)
        mod_idx = create_module_with_interface(arena, proc_name, &
                                               variant_indices)
        call register_generated_module(mod_idx, proc_name, module_indices, &
                                       module_count, module_names)

        if (allocated(proc_sigs)) deallocate (proc_sigs)
        if (allocated(variant_indices)) deallocate (variant_indices)
    end subroutine process_specializable_procedure

    subroutine debug_log_signatures(signatures, proc_name)
        type(signatures_map_t), intent(in) :: signatures
        character(len=*), intent(in) :: proc_name
        integer :: j

        if (.not. debug_logging_enabled()) return

        write (error_unit, '(A,1X,I0)') 'DEBUG signatures map count', &
            signatures%proc_count
        if (signatures%proc_count > 0) then
            do j = 1, signatures%proc_count
                if (.not. allocated(signatures%proc_sigs(j)%procedure_name)) cycle
                write (error_unit, '(A,1X,A,1X,I0)') 'DEBUG entry', &
                    trim(signatures%proc_sigs(j)%procedure_name), &
                    signatures%proc_sigs(j)%sig_count
            end do
        end if
    end subroutine debug_log_signatures

    subroutine debug_log_procedure_signatures(proc_name, proc_sigs)
        character(len=*), intent(in) :: proc_name
        type(type_signature_t), intent(in) :: proc_sigs(:)
        integer :: j, t
        character(len=:), allocatable :: label

        if (.not. debug_logging_enabled()) return

        write (error_unit, '(A,1X,A,1X,I0)') 'DEBUG monomorph: signatures for', &
            trim(proc_name), size(proc_sigs)

        if (size(proc_sigs) == 0) return

        do j = 1, size(proc_sigs)
            if (allocated(proc_sigs(j)%param_kinds)) then
                write (error_unit, '(A,1X,A,1X,*(I0,1X))') &
                    'DEBUG signature kinds', &
                    trim(proc_name), proc_sigs(j)%param_kinds
            end if

            if (allocated(proc_sigs(j)%param_type_strings)) then
                label = ''
                do t = 1, size(proc_sigs(j)%param_type_strings)
                    if (t > 1) label = label // ' '
                    if (len_trim(proc_sigs(j)%param_type_strings(t)) == 0) then
                        label = label // '<empty>'
                    else
                        label = label // &
                                trim(proc_sigs(j)%param_type_strings(t))
                    end if
                end do
                write (error_unit, '(A,1X,A,1X,A)') &
                    'DEBUG signature types', trim(proc_name), trim(label)
            end if
        end do
    end subroutine debug_log_procedure_signatures

    subroutine normalize_and_deduplicate_signatures(proc_sigs, handled)
        type(type_signature_t), allocatable, intent(inout) :: proc_sigs(:)
        logical, intent(out) :: handled
        type(type_signature_t), allocatable :: unique_sigs(:)
        integer :: unique_count, j, k
        logical :: is_duplicate

        do j = 1, size(proc_sigs)
            call normalize_signature_param_types(proc_sigs(j))
        end do

        allocate (unique_sigs(size(proc_sigs)))
        unique_count = 0

        do j = 1, size(proc_sigs)
            is_duplicate = .false.
            do k = 1, unique_count
                if (signatures_are_identical(proc_sigs(j), unique_sigs(k))) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) then
                unique_count = unique_count + 1
                unique_sigs(unique_count) = proc_sigs(j)
            end if
        end do

        if (unique_count <= 1) then
            handled = .false.
            if (allocated(unique_sigs)) deallocate (unique_sigs)
            return
        end if

        deallocate (proc_sigs)
        allocate (proc_sigs(unique_count))
        proc_sigs(1:unique_count) = unique_sigs(1:unique_count)
        deallocate (unique_sigs)
        handled = .true.
    end subroutine normalize_and_deduplicate_signatures

    subroutine create_procedure_variants(arena, proc_idx, is_function, &
                                         proc_sigs, variant_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: proc_idx
        logical, intent(in) :: is_function
        type(type_signature_t), intent(in) :: proc_sigs(:)
        integer, allocatable, intent(out) :: variant_indices(:)
        integer :: j

        allocate (variant_indices(size(proc_sigs)))
        do j = 1, size(proc_sigs)
            if (is_function) then
                variant_indices(j) = clone_function_with_signature(arena, &
                                                                   proc_idx, &
                                                                   proc_sigs(j))
            else
                variant_indices(j) = clone_subroutine_with_signature(arena, &
                                                                     proc_idx, &
                                                                     proc_sigs(j))
            end if
        end do
    end subroutine create_procedure_variants

    function create_module_with_interface(arena, proc_name, variant_indices) &
        result(mod_idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: proc_name
        integer, intent(in) :: variant_indices(:)
        integer :: mod_idx
        integer :: mod_proc_idx, interface_idx, iso_use_idx, j

        mod_proc_idx = create_module_procedure_node(arena, proc_name, &
                                                    variant_indices)
        interface_idx = create_interface_node(arena, proc_name, mod_proc_idx)
        iso_use_idx = create_dp_kind_use_statement_node(arena)
        mod_idx = create_module_for_function(arena, proc_name, iso_use_idx, &
                                             interface_idx, variant_indices)

        call set_parent_if_valid(arena, iso_use_idx, mod_idx)
        call set_parent_if_valid(arena, mod_proc_idx, mod_idx)
        call set_parent_if_valid(arena, interface_idx, mod_idx)
        do j = 1, size(variant_indices)
            call set_parent_if_valid(arena, variant_indices(j), mod_idx)
        end do
    end function create_module_with_interface

    subroutine register_generated_module(mod_idx, proc_name, module_indices, &
                                         module_count, module_names)
        integer, intent(in) :: mod_idx
        character(len=*), intent(in) :: proc_name
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)

        module_count = module_count + 1
        if (module_count > size(module_indices)) then
            call resize_integer_array(module_indices, module_count * 2)
        end if
        if (module_count > size(module_names)) then
            call resize_character_array(module_names, module_count * 2)
        end if
        module_indices(module_count) = mod_idx
        module_names(module_count) = adjustl("auto_"//trim(proc_name))
    end subroutine register_generated_module

    subroutine initialize_mixed_container_arrays(explicit_count, implicit_count, &
                                                 preserved_indices, &
                                                 implicit_preserved, &
                                                 module_indices, module_names, &
                                                 program_indices)
        integer, intent(in) :: explicit_count, implicit_count
        integer, allocatable, intent(out) :: preserved_indices(:)
        integer, allocatable, intent(out) :: implicit_preserved(:)
        integer, allocatable, intent(out) :: module_indices(:)
        character(len=128), allocatable, intent(out) :: module_names(:)
        integer, allocatable, intent(out) :: program_indices(:)

        allocate (preserved_indices(max(1, explicit_count)))
        if (implicit_count > 0) then
            allocate (implicit_preserved(max(1, implicit_count)))
        end if
        allocate (module_indices(max(1, max(explicit_count, implicit_count))))
        allocate (character(len=128) :: module_names(max(1, max(explicit_count, &
                                                                implicit_count))))
        allocate (program_indices(max(1, explicit_count)))
    end subroutine initialize_mixed_container_arrays

    subroutine process_implicit_declarations(arena, signatures, implicit_indices, &
                                             implicit_preserved, &
                                             implicit_preserved_count, &
                                             module_indices, module_count, &
                                             module_names)
        type(ast_arena_t), intent(inout) :: arena
        type(signatures_map_t), intent(in) :: signatures
        integer, intent(in) :: implicit_indices(:)
        integer, allocatable, intent(inout) :: implicit_preserved(:)
        integer, intent(inout) :: implicit_preserved_count
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)
        integer :: i, child_idx, implicit_count
        logical :: handled
        character(len=:), allocatable :: proc_name

        implicit_count = size(implicit_indices)

        do i = 1, implicit_count
            child_idx = implicit_indices(i)
            if (child_idx < 1 .or. child_idx > arena%size) cycle
            if (.not. allocated(arena%entries(child_idx)%node)) cycle

            select type (node => arena%entries(child_idx)%node)
            type is (function_def_node)
                proc_name = node%name
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     proc_name, .true., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    call add_to_implicit_preserved(implicit_preserved, &
                                                   implicit_preserved_count, child_idx)
                end if
            type is (subroutine_def_node)
                proc_name = node%name
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     proc_name, .false., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    call add_to_implicit_preserved(implicit_preserved, &
                                                   implicit_preserved_count, child_idx)
                end if
            type is (program_node)
                ! Extract statements from nested program node (issue #2160)
                ! When bare statements are parsed together, they create a program node
                ! We need to extract the body statements instead of nesting programs
                if (allocated(node%body_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%body_indices)
                            call add_to_implicit_preserved(implicit_preserved, &
                                                           implicit_preserved_count, &
                                                           node%body_indices(j))
                        end do
                    end block
                end if
            class default
                call add_to_implicit_preserved(implicit_preserved, &
                                               implicit_preserved_count, child_idx)
            end select
        end do
    end subroutine process_implicit_declarations

    subroutine add_to_implicit_preserved(implicit_preserved, &
                                         implicit_preserved_count, child_idx)
        integer, allocatable, intent(inout) :: implicit_preserved(:)
        integer, intent(inout) :: implicit_preserved_count
        integer, intent(in) :: child_idx

        implicit_preserved_count = implicit_preserved_count + 1
        if (implicit_preserved_count > size(implicit_preserved)) then
            call resize_integer_array(implicit_preserved, &
                                      implicit_preserved_count * 2)
        end if
        implicit_preserved(implicit_preserved_count) = child_idx
    end subroutine add_to_implicit_preserved

    subroutine process_explicit_programs(arena, signatures, explicit_indices, &
                                         explicit_count, module_indices, &
                                         module_count, module_names, &
                                         preserved_indices, preserved_count, &
                                         program_indices, program_count)
        type(ast_arena_t), intent(inout) :: arena
        type(signatures_map_t), intent(in) :: signatures
        integer, intent(inout) :: explicit_indices(:)
        integer, intent(in) :: explicit_count
        integer, allocatable, intent(inout) :: module_indices(:)
        integer, intent(inout) :: module_count
        character(len=*), allocatable, intent(inout) :: module_names(:)
        integer, allocatable, intent(inout) :: preserved_indices(:)
        integer, intent(inout) :: preserved_count
        integer, allocatable, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer :: i, child_idx
        integer :: updated_idx

        do i = 1, explicit_count
            child_idx = explicit_indices(i)
            call process_container_entry(arena, signatures, child_idx, &
                                         module_indices, module_count, module_names, &
                                         preserved_indices, &
                                         preserved_count, program_indices, &
                                         program_count, &
                                         updated_idx)
            explicit_indices(i) = updated_idx
        end do
    end subroutine process_explicit_programs

    subroutine finalize_no_modules(arena, root_index, preserved_indices, &
                                   preserved_count, implicit_preserved, &
                                   implicit_preserved_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, allocatable, intent(in) :: preserved_indices(:)
        integer, intent(in) :: preserved_count
        integer, allocatable, intent(in) :: implicit_preserved(:)
        integer, intent(in) :: implicit_preserved_count

        call set_container_explicit_indices(arena, root_index, preserved_indices, &
                                            preserved_count)
        call set_container_implicit_indices(arena, root_index, implicit_preserved, &
                                            implicit_preserved_count)
    end subroutine finalize_no_modules

    subroutine find_program_nodes(arena, preserved_indices, preserved_count, &
                                  program_indices, program_count)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: preserved_indices(:)
        integer, intent(in) :: preserved_count
        integer, allocatable, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer :: i

        if (program_count > 0) return

        if (preserved_count > 0) then
            do i = 1, preserved_count
                if (preserved_indices(i) < 1 .or. &
                    preserved_indices(i) > arena%size) cycle
                if (.not. &
                    allocated(arena%entries(preserved_indices(i))%node)) cycle
                select type (prog_check => &
                             arena%entries(preserved_indices(i))%node)
                type is (program_node)
                    program_count = program_count + 1
                    if (program_count > size(program_indices)) then
                        call resize_integer_array(program_indices, &
                                                  program_count * 2)
                    end if
                    program_indices(program_count) = preserved_indices(i)
                end select
            end do
        end if
    end subroutine find_program_nodes

    subroutine create_main_program_from_preserved(arena, root_index, &
                                                  module_indices, module_count, &
                                                  module_names, &
                                                  implicit_preserved, &
                                                  implicit_preserved_count, &
                                                  preserved_indices, &
                                                  preserved_count, &
                                                  program_indices, program_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, allocatable, intent(in) :: module_indices(:)
        integer, intent(in) :: module_count
        character(len=*), allocatable, intent(in) :: module_names(:)
        integer, allocatable, intent(in) :: implicit_preserved(:)
        integer, intent(in) :: implicit_preserved_count
        integer, allocatable, intent(inout) :: preserved_indices(:)
        integer, intent(inout) :: preserved_count
        integer, allocatable, intent(inout) :: program_indices(:)
        integer, intent(inout) :: program_count
        integer, allocatable :: program_body(:)
        integer :: total_body, idx, prog_idx

        total_body = module_count + implicit_preserved_count + preserved_count
        if (total_body == 0) return

        call build_main_program_body(arena, module_indices, module_count, &
                                     module_names, implicit_preserved, &
                                     implicit_preserved_count, preserved_indices, &
                                     preserved_count, program_body)

        call create_and_register_main_program(arena, root_index, program_body, &
                                              prog_idx)

        if (program_count >= size(program_indices)) then
            call resize_integer_array(program_indices, max(1, &
                                                           size(program_indices) * 2))
        end if
        program_count = 1
        program_indices(1) = prog_idx

        preserved_count = 1
        preserved_indices(1) = prog_idx
        call clear_container_implicit_indices(arena, root_index)
    end subroutine create_main_program_from_preserved

    subroutine build_main_program_body(arena, module_indices, module_count, &
                                       module_names, implicit_preserved, &
                                       implicit_preserved_count, preserved_indices, &
                                       preserved_count, program_body)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: module_indices(:)
        integer, intent(in) :: module_count
        character(len=*), allocatable, intent(in) :: module_names(:)
        integer, allocatable, intent(in) :: implicit_preserved(:)
        integer, intent(in) :: implicit_preserved_count
        integer, allocatable, intent(in) :: preserved_indices(:)
        integer, intent(in) :: preserved_count
        integer, allocatable, intent(out) :: program_body(:)
        integer :: total_body, idx, i

        total_body = module_count + implicit_preserved_count + preserved_count
        allocate (program_body(total_body))
        idx = 0

        if (module_count > 0) then
            do i = 1, module_count
                idx = idx + 1
                program_body(idx) = create_use_statement_node( &
                                    arena, trim(module_names(i)))
            end do
        end if

        if (implicit_preserved_count > 0) then
            do i = 1, implicit_preserved_count
                idx = idx + 1
                program_body(idx) = implicit_preserved(i)
            end do
        end if

        if (preserved_count > 0) then
            do i = 1, preserved_count
                idx = idx + 1
                program_body(idx) = preserved_indices(i)
            end do
        end if
    end subroutine build_main_program_body

    subroutine create_and_register_main_program(arena, root_index, program_body, &
                                                prog_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, allocatable, intent(in) :: program_body(:)
        integer, intent(out) :: prog_idx
        type(program_node) :: new_prog
        integer :: i

        new_prog = create_program("main", program_body, line=0, column=0)
        call arena%push(new_prog)
        prog_idx = arena%size
        call set_parent_if_valid(arena, prog_idx, root_index)
        do i = 1, size(program_body)
            call set_parent_if_valid(arena, program_body(i), prog_idx)
        end do
    end subroutine create_and_register_main_program

    subroutine promote_double_literals_for_programs(arena, program_indices, &
                                                    program_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(in) :: program_indices(:)
        integer, intent(in) :: program_count
        integer :: i

        do i = 1, program_count
            call promote_double_literal_assignments(arena, program_indices(i))
        end do
    end subroutine promote_double_literals_for_programs

    subroutine finalize_container_indices(arena, root_index, &
                                          module_indices, module_count, &
                                          preserved_indices, preserved_count, &
                                          module_names, program_indices, &
                                          program_count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer, allocatable, intent(in) :: module_indices(:)
        integer, intent(in) :: module_count
        integer, allocatable, intent(in) :: preserved_indices(:)
        integer, intent(in) :: preserved_count
        character(len=*), allocatable, intent(in) :: module_names(:)
        integer, allocatable, intent(in) :: program_indices(:)
        integer, intent(in) :: program_count
        integer, allocatable :: new_explicit(:)
        integer :: i

        if (module_count + preserved_count > 0) then
            allocate (new_explicit(module_count + preserved_count))
            if (module_count > 0) then
                new_explicit(1:module_count) = module_indices(1:module_count)
                do i = 1, module_count
                    call set_parent_if_valid(arena, module_indices(i), root_index)
                end do
            end if
            if (preserved_count > 0) then
                new_explicit(module_count + 1:) = &
                    preserved_indices(1:preserved_count)
            end if
            call set_container_explicit_indices(arena, root_index, new_explicit, &
                                                size(new_explicit))
        else
            call clear_container_explicit_indices(arena, root_index)
        end if

        do i = 1, program_count
            if (program_indices(i) < 1 .or. program_indices(i) > arena%size) cycle
            call ensure_program_has_uses(arena, program_indices(i), module_names, &
                                         module_count)
        end do
    end subroutine finalize_container_indices

    subroutine process_mixed_container(arena, root_index, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        type(mixed_construct_container_node), pointer :: container
        integer, allocatable :: explicit_indices(:), implicit_indices(:)
        integer :: explicit_count, implicit_count
        integer, allocatable :: preserved_indices(:), implicit_preserved(:)
        integer :: preserved_count, implicit_preserved_count
        integer, allocatable :: module_indices(:), program_indices(:)
        integer :: module_count, program_count
        character(len=128), allocatable :: module_names(:)

        call get_mixed_container_node(arena, root_index, container)
        if (.not. associated(container)) return

        explicit_count = 0
        if (allocated(container%explicit_program_indices)) then
            explicit_count = size(container%explicit_program_indices)
            allocate (explicit_indices(explicit_count))
            explicit_indices = container%explicit_program_indices
        else
            allocate (explicit_indices(0))
        end if

        implicit_count = 0
        if (allocated(container%implicit_declaration_indices)) then
            implicit_count = size(container%implicit_declaration_indices)
            allocate (implicit_indices(implicit_count))
            implicit_indices = container%implicit_declaration_indices
        else
            allocate (implicit_indices(0))
        end if

        if (explicit_count == 0 .and. implicit_count == 0) return

        call initialize_mixed_container_arrays(explicit_count, implicit_count, &
                                               preserved_indices, implicit_preserved, &
                                               module_indices, module_names, &
                                               program_indices)

        preserved_count = 0
        implicit_preserved_count = 0
        module_count = 0
        program_count = 0

        if (implicit_count > 0) then
            call process_implicit_declarations(arena, signatures, implicit_indices, &
                                               implicit_preserved, &
                                               implicit_preserved_count, &
                                               module_indices, module_count, &
                                               module_names)
        end if

        call process_explicit_programs(arena, signatures, explicit_indices, &
                                       explicit_count, module_indices, &
                                       module_count, module_names, &
                                       preserved_indices, preserved_count, &
                                       program_indices, program_count)

        if (module_count == 0) then
            call finalize_no_modules(arena, root_index, preserved_indices, &
                                     preserved_count, implicit_preserved, &
                                     implicit_preserved_count)
            return
        end if

        call find_program_nodes(arena, preserved_indices, preserved_count, &
                                program_indices, program_count)

        if (program_count == 0) then
            call create_main_program_from_preserved(arena, root_index, &
                                                    module_indices, module_count, &
                                                    module_names, &
                                                    implicit_preserved, &
                                                    implicit_preserved_count, &
                                                    preserved_indices, &
                                                    preserved_count, &
                                                    program_indices, program_count)
        end if

        if (program_count > 0) then
            call promote_double_literals_for_programs(arena, program_indices, &
                                                      program_count)
        end if

        call finalize_container_indices(arena, root_index, &
                                        module_indices, module_count, &
                                        preserved_indices, preserved_count, &
                                        module_names, program_indices, &
                                        program_count)
    end subroutine process_mixed_container
