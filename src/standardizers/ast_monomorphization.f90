module ast_monomorphization
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, call_or_subscript_node, &
                              create_program
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   get_procedure_name, get_procedure_params, &
                                   get_procedure_body, get_procedure_return_type, &
                                   create_function_def, create_subroutine_def
    use ast_nodes_data, only: module_node, parameter_declaration_node, &
                              declaration_node, mixed_construct_container_node, &
                              create_module
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
                              use_statement_node, create_interface_block, &
                              create_module_procedure, create_use_statement
    use ast_base, only: string_t
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t
    use codegen_name_mangling, only: mangle_procedure_name
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
                                   TLOGICAL, TCHAR, TCOMPLEX, TDOUBLE, TARRAY
    use uid_generator, only: generate_uid
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: transform_monomorphization

contains

    subroutine transform_monomorphization(arena, root_index, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        type(program_node), pointer :: root_prog
        logical :: is_multi_unit
        integer :: body_size
        integer, allocatable :: preserved_indices(:)
        integer, allocatable :: module_indices(:)
        integer, allocatable :: program_indices(:)
        character(len=128), allocatable :: module_names(:)
        integer :: preserved_count, module_count, program_count, i, j

        call get_program_node(arena, root_index, root_prog)
        if (.not. associated(root_prog)) then
            if (root_index > 0 .and. root_index <= arena%size) then
                if (allocated(arena%entries(root_index)%node)) then
                    select type (container => arena%entries(root_index)%node)
                    type is (mixed_construct_container_node)
                        write (error_unit, '(A)') 'DEBUG root is mixed container'
                        if (allocated(container%explicit_program_indices)) then
                            write (error_unit, '(A,*(1X,I0))') &
                                'DEBUG explicit programs=', &
                                container%explicit_program_indices
                            do i = 1, size(container%explicit_program_indices)
                                if (container%explicit_program_indices(i) > 0 .and. &
                                    container%explicit_program_indices(i) <= arena%size) then
                                    if (allocated(arena%entries(container%explicit_program_indices(i))%node_type)) then
                                        write (error_unit, '(A,1X,I0,1X,A)') &
                                            'DEBUG explicit node', &
                                            container%explicit_program_indices(i), &
                                            trim(arena%entries(container%explicit_program_indices(i))%node_type)
                                    end if
                                end if
                            end do
                        else
                            write (error_unit, '(A)') 'DEBUG explicit programs=<none>'
                        end if
                        if (allocated(container%implicit_declaration_indices)) then
                            write (error_unit, '(A,*(1X,I0))') &
                                'DEBUG implicit indices=', &
                                container%implicit_declaration_indices
                            do i = 1, size(container%implicit_declaration_indices)
                                if (container%implicit_declaration_indices(i) > 0 .and. &
                                    container%implicit_declaration_indices(i) <= arena%size) then
                                    if (allocated(arena%entries(container%implicit_declaration_indices(i))%node_type)) then
                                        write (error_unit, '(A,1X,I0,1X,A)') &
                                            'DEBUG implicit node', &
                                            container%implicit_declaration_indices(i), &
                                            trim(arena%entries(container%implicit_declaration_indices(i))%node_type)
                                    end if
                                end if
                            end do
                        else
                            write (error_unit, '(A)') 'DEBUG implicit indices=<none>'
                        end if
                        call process_mixed_container(arena, root_index, container, &
                                                     signatures)
                    class default
                        write (error_unit, '(A)') &
                            'DEBUG root_prog not associated and not container'
                    end select
                end if
            else
                write (error_unit, '(A)') &
                    'DEBUG root_prog not associated (invalid index)'
            end if
            return
        end if
        write (error_unit, '(A,1X,A)') 'DEBUG root_prog name', trim(root_prog%name)
        if (.not. allocated(root_prog%body_indices)) then
            write (error_unit, '(A)') 'DEBUG root_prog body not allocated'
            return
        end if
        write (error_unit, '(A,1X,I0)') 'DEBUG root_prog body size', &
            size(root_prog%body_indices)

        is_multi_unit = trim(root_prog%name) == "__MULTI_UNIT__"
        body_size = size(root_prog%body_indices)

        allocate (preserved_indices(max(1, body_size)))
        allocate (module_indices(max(1, body_size)))
        allocate (program_indices(max(1, body_size)))
        allocate (character(len=128) :: module_names(max(1, body_size)))

        preserved_count = 0
        module_count = 0
        program_count = 0

        call process_program_body_children(arena, root_prog, signatures, &
                                           preserved_indices, preserved_count, &
                                           module_indices, &
                                           module_count, module_names, &
                                           program_indices, &
                                           program_count)

        if (module_count == 0) then
            root_prog%body_indices = preserved_indices(1:preserved_count)
            return
        end if

        call finalize_monomorphized_root(arena, root_index, root_prog, &
                                         is_multi_unit, preserved_indices, &
                                         preserved_count, &
                                         module_indices, module_count, module_names, &
                                         program_indices, &
                                         program_count)

    end subroutine transform_monomorphization

    subroutine process_program_body_children(arena, root_prog, signatures, &
                                             preserved_indices, preserved_count, &
                                             module_indices, &
                                             module_count, &
                                             module_names, program_indices, &
                                             program_count)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), pointer, intent(in) :: root_prog
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

        do i = 1, size(root_prog%body_indices)
            child_idx = root_prog%body_indices(i)
            if (child_idx < 1 .or. child_idx > arena%size) cycle
            if (.not. allocated(arena%entries(child_idx)%node)) cycle

            select type (node => arena%entries(child_idx)%node)
            type is (function_def_node)
                write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=function', &
                    child_idx, &
                    trim(node%name)
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     node%name, .true., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    preserved_count = preserved_count + 1
                    preserved_indices(preserved_count) = child_idx
                end if
            type is (program_node)
                write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=program', child_idx, &
                    trim(node%name)
                if (trim(node%name) /= "__MULTI_UNIT__") then
                    program_count = program_count + 1
                    program_indices(program_count) = child_idx
                end if
                preserved_count = preserved_count + 1
                preserved_indices(preserved_count) = child_idx
            type is (subroutine_def_node)
                write (error_unit, '(A,1X,I0,1X,A)') 'DEBUG node=subroutine', &
                    child_idx, trim(node%name)
                call process_specializable_procedure(arena, signatures, child_idx, &
                                                     node%name, .false., handled, &
                                                     module_indices, &
                                                     module_count, &
                                                     module_names)
                if (.not. handled) then
                    preserved_count = preserved_count + 1
                    preserved_indices(preserved_count) = child_idx
                end if
            class default
                write (error_unit, '(A,1X,I0)') 'DEBUG node=other', child_idx
                preserved_count = preserved_count + 1
                preserved_indices(preserved_count) = child_idx
            end select
        end do
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
        integer :: mod_proc_idx, interface_idx, mod_idx
        integer :: j

        write (error_unit, '(A,1X,I0)') 'DEBUG signatures map count', signatures%proc_count
        if (signatures%proc_count > 0) then
            do j = 1, signatures%proc_count
                if (.not. allocated(signatures%proc_sigs(j)%procedure_name)) cycle
                write (error_unit, '(A,1X,A,1X,I0)') 'DEBUG entry', &
                    trim(signatures%proc_sigs(j)%procedure_name), &
                    signatures%proc_sigs(j)%sig_count
            end do
        end if

        proc_sigs = get_procedure_signatures(signatures, proc_name)
        write (error_unit, '(A,1X,A,1X,I0)') 'DEBUG monomorph: signatures for', &
            trim(proc_name), size(proc_sigs)
        if (size(proc_sigs) > 0) then
            do j = 1, size(proc_sigs)
                if (allocated(proc_sigs(j)%param_kinds)) then
                    write (error_unit, '(A,1X,A,1X,*(I0,1X))') 'DEBUG signature kinds', &
                        trim(proc_name), proc_sigs(j)%param_kinds
                end if
                if (allocated(proc_sigs(j)%param_type_strings)) then
                    block
                        integer :: t
                        character(len=:), allocatable :: label
                        label = ''
                        do t = 1, size(proc_sigs(j)%param_type_strings)
                            if (t > 1) label = label // ' '
                            if (len_trim(proc_sigs(j)%param_type_strings(t)) == 0) then
                                label = label // '<empty>'
                            else
                                label = label // trim(proc_sigs(j)%param_type_strings(t))
                            end if
                        end do
                        write (error_unit, '(A,1X,A,1X,A)') 'DEBUG signature types', &
                            trim(proc_name), trim(label)
                    end block
                end if
            end do
        end if
        if (size(proc_sigs) <= 1) then
            handled = .false.
            if (allocated(proc_sigs)) deallocate (proc_sigs)
            return
        end if

        handled = .true.
        allocate (variant_indices(size(proc_sigs)))
        do j = 1, size(proc_sigs)
            if (is_function) then
                variant_indices(j) = clone_function_with_signature(arena, proc_idx, &
                                                                   proc_sigs(j))
            else
                variant_indices(j) = clone_subroutine_with_signature(arena, proc_idx, &
                                                                     proc_sigs(j))
            end if
        end do

        mod_proc_idx = create_module_procedure_node(arena, proc_name, &
                                                    variant_indices)
        interface_idx = create_interface_node(arena, proc_name, mod_proc_idx)
        mod_idx = create_module_for_function(arena, proc_name, interface_idx, &
                                             variant_indices)

        module_count = module_count + 1
        if (module_count > size(module_indices)) then
            call resize_integer_array(module_indices, module_count * 2)
        end if
        if (module_count > size(module_names)) then
            call resize_character_array(module_names, module_count * 2)
        end if
        module_indices(module_count) = mod_idx
        module_names(module_count) = adjustl("auto_"//trim(proc_name))

        call set_parent_if_valid(arena, mod_proc_idx, mod_idx)
        call set_parent_if_valid(arena, interface_idx, mod_idx)
        do j = 1, size(variant_indices)
            call set_parent_if_valid(arena, variant_indices(j), mod_idx)
        end do

        if (allocated(proc_sigs)) deallocate (proc_sigs)
        if (allocated(variant_indices)) deallocate (variant_indices)
    end subroutine process_specializable_procedure

    subroutine process_mixed_container(arena, root_index, container, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(mixed_construct_container_node), intent(inout) :: container
        type(signatures_map_t), intent(in) :: signatures
        integer :: explicit_count
        integer :: implicit_count
        integer, allocatable :: preserved_indices(:)
        integer, allocatable :: implicit_preserved(:)
        integer :: preserved_count
        integer :: implicit_preserved_count
        integer, allocatable :: module_indices(:)
        integer :: module_count
        character(len=128), allocatable :: module_names(:)
        integer, allocatable :: program_indices(:)
        integer :: program_count
        integer :: i, child_idx
        logical :: handled
        integer, allocatable :: new_explicit(:)
        integer, allocatable :: new_implicit(:)

        explicit_count = 0
        if (allocated(container%explicit_program_indices)) then
            explicit_count = size(container%explicit_program_indices)
        end if

        implicit_count = 0
        if (allocated(container%implicit_declaration_indices)) then
            implicit_count = size(container%implicit_declaration_indices)
        end if

        if (explicit_count == 0 .and. implicit_count == 0) return

        allocate (preserved_indices(max(1, explicit_count)))
        if (implicit_count > 0) then
            allocate (implicit_preserved(max(1, implicit_count)))
        end if
        allocate (module_indices(max(1, max(explicit_count, implicit_count))))
        allocate (character(len=128) :: module_names(max(1, max(explicit_count, &
                                                     implicit_count))))
        allocate (program_indices(max(1, explicit_count)))

        preserved_count = 0
        implicit_preserved_count = 0
        module_count = 0
        program_count = 0

        if (implicit_count > 0) then
            do i = 1, implicit_count
                child_idx = container%implicit_declaration_indices(i)
                if (child_idx < 1 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle

                select type (node => arena%entries(child_idx)%node)
                type is (function_def_node)
                    call process_specializable_procedure(arena, signatures, child_idx, &
                        node%name, .true., handled, module_indices, module_count, &
                        module_names)
                    if (.not. handled) then
                        implicit_preserved_count = implicit_preserved_count + 1
                        if (implicit_preserved_count > size(implicit_preserved)) then
                            call resize_integer_array(implicit_preserved, &
                                                      implicit_preserved_count * 2)
                        end if
                        implicit_preserved(implicit_preserved_count) = child_idx
                    end if
                type is (subroutine_def_node)
                    call process_specializable_procedure(arena, signatures, child_idx, &
                        node%name, .false., handled, module_indices, module_count, &
                        module_names)
                    if (.not. handled) then
                        implicit_preserved_count = implicit_preserved_count + 1
                        if (implicit_preserved_count > size(implicit_preserved)) then
                            call resize_integer_array(implicit_preserved, &
                                                      implicit_preserved_count * 2)
                        end if
                        implicit_preserved(implicit_preserved_count) = child_idx
                    end if
                class default
                    implicit_preserved_count = implicit_preserved_count + 1
                    if (implicit_preserved_count > size(implicit_preserved)) then
                        call resize_integer_array(implicit_preserved, &
                                                  implicit_preserved_count * 2)
                    end if
                    implicit_preserved(implicit_preserved_count) = child_idx
                end select
            end do

            if (implicit_preserved_count > 0) then
                allocate (new_implicit(implicit_preserved_count))
                new_implicit = implicit_preserved(1:implicit_preserved_count)
                container%implicit_declaration_indices = new_implicit
            else
                if (allocated(container%implicit_declaration_indices)) then
                    deallocate (container%implicit_declaration_indices)
                end if
            end if
        else
            if (allocated(container%implicit_declaration_indices)) then
                if (size(container%implicit_declaration_indices) == 0) then
                    deallocate (container%implicit_declaration_indices)
                end if
            end if
        end if

        do i = 1, explicit_count
            child_idx = container%explicit_program_indices(i)
            call process_container_entry(arena, signatures, child_idx, &
                        module_indices, module_count, module_names, preserved_indices, &
                                      preserved_count, program_indices, program_count, &
                                         container%explicit_program_indices(i))
        end do

        if (module_count == 0) then
            if (preserved_count /= explicit_count) then
                allocate (new_explicit(preserved_count))
                if (preserved_count > 0) new_explicit = &
                    preserved_indices(1:preserved_count)
                container%explicit_program_indices = new_explicit
            end if
            return
        end if

        allocate (new_explicit(module_count + preserved_count))
        if (module_count > 0) then
            new_explicit(1:module_count) = module_indices(1:module_count)
            do i = 1, module_count
                call set_parent_if_valid(arena, module_indices(i), root_index)
            end do
        end if
        if (preserved_count > 0) then
            new_explicit(module_count + 1:) = preserved_indices(1:preserved_count)
        end if
        container%explicit_program_indices = new_explicit

        if (program_count == 0) then
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
        end if

        do i = 1, program_count
            if (program_indices(i) < 1 .or. program_indices(i) > arena%size) cycle
            call ensure_program_has_uses(arena, program_indices(i), module_names, &
                                         module_count)
        end do
    end subroutine process_mixed_container

    subroutine process_container_entry(arena, signatures, child_idx, module_indices, &
                       module_count, module_names, preserved_indices, preserved_count, &
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
                                           node%name, .true., handled, module_indices, &
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
                                          node%name, .false., handled, module_indices, &
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

        if (module_count <= 0) return

        call get_program_node(arena, prog_idx, prog)
        if (.not. associated(prog)) return

        allocate (need_use(module_count))
        need_use = .true.

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) < 1 .or. &
                    prog%body_indices(i) > arena%size) cycle
                if (.not. allocated(arena%entries(prog%body_indices(i))%node)) cycle
                select type (use_node => arena%entries(prog%body_indices(i))%node)
                type is (use_statement_node)
                    call mark_existing_use(use_node%module_name, module_names, &
                                           need_use)
                end select
            end do
        end if

        insert_count = count(need_use)
        if (insert_count <= 0) return

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

        orig_size = 0
        if (allocated(prog%body_indices)) orig_size = size(prog%body_indices)
        allocate (updated_body(insert_count + orig_size))

        if (insert_count > 0) updated_body(1:insert_count) = inserted_indices
        if (orig_size > 0) then
            updated_body(insert_count + 1:) = prog%body_indices
        end if

        prog%body_indices = updated_body
    end subroutine ensure_program_has_uses

    subroutine mark_existing_use(use_name, module_names, need_use)
        character(len=*), intent(in) :: use_name
        character(len=*), intent(in) :: module_names(:)
        logical, intent(inout) :: need_use(:)
        integer :: i

        do i = 1, size(module_names)
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
            if (allocated(signature%param_kinds)) then
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
    end function clone_subroutine_with_signature

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

        if (len_trim(return_type) > 0) then
            lowered_return = to_lower(return_type)
        else
            lowered_return = ""
        end if

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
                    call arena%push(decl_copy)
                    body_indices_copy(i) = arena%size
                end select
            end do
        else
            allocate (body_indices_copy(0))
        end if
    end function clone_function_body_with_updated_result

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
            if (index(to_lower(new_param%type_name), "dimension(") > 0) then
                if (allocated(new_param%dimension_indices)) then
                    deallocate (new_param%dimension_indices)
                end if
                new_param%is_array = .false.
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

        mod_proc = create_module_procedure(proc_names)
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

        type_str = ""

        if (allocated(signature%return_type_string)) then
            if (len_trim(signature%return_type_string) > 0) then
                type_str = trim(signature%return_type_string)
            end if
        end if

        if (len_trim(type_str) == 0) then
            if (signature%call_site_node > 0) then
                call get_call_node(arena, signature%call_site_node, call_node)
                if (associated(call_node)) then
                    inferred_type = call_node%inferred_type
                    call inferred_type%sync_from_arena()
                    type_str = trim(mono_type_to_string(inferred_type, &
                                                        include_shape=.true., &
                                                        fallback=''))
                    if (len_trim(type_str) == 0) then
                        type_str = get_kind_type_string(inferred_type%kind)
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
                end if
            end if
        end if

        if (len_trim(type_str) == 0 .and. associated(orig_func)) then
            type_str = trim(mono_type_to_string(orig_func%inferred_type, &
                                                include_shape=.true., fallback=''))
        end if

        if (len_trim(type_str) == 0) then
            type_str = get_kind_type_string(signature%return_kind)
        end if

        if (allocated(signature%param_kinds)) then
            if (size(signature%param_kinds) > 0) then
                if (len_trim(type_str) == 0) then
                    type_str = fallback_return_type_from_params( &
                        signature%param_kinds)
                else if (trim(type_str) == "integer") then
                    if (any(signature%param_kinds /= 2)) then
                        type_str = fallback_return_type_from_params( &
                            signature%param_kinds)
                    end if
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

        if (call_index <= 0 .or. call_index > arena%size) return
        if (.not. allocated(arena%entries(call_index)%node)) return
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

end module ast_monomorphization
