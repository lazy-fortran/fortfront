module ast_monomorphization
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, &
                                   get_procedure_name, get_procedure_params, &
                                   get_procedure_body, get_procedure_return_type, &
                                   create_function_def
    use ast_nodes_data, only: module_node, parameter_declaration_node, &
                              declaration_node, create_module
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
        integer, intent(in) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        type(program_node), pointer :: prog
        integer :: i, j, func_idx
        character(len=:), allocatable :: func_name
        type(type_signature_t), allocatable :: func_sigs(:)
        integer, allocatable :: new_body_indices(:), variant_indices(:)
        integer, allocatable :: module_indices(:)
        integer :: new_body_count, module_count, interface_idx, mod_proc_idx
        integer :: mod_idx, use_idx

        call get_program_node(arena, root_index, prog)
        if (.not. associated(prog)) return
        if (.not. allocated(prog%body_indices)) return

        new_body_count = 0
        module_count = 0
        allocate (new_body_indices(size(prog%body_indices) * 2))
        allocate (module_indices(64))

        do i = 1, size(prog%body_indices)
            func_idx = prog%body_indices(i)
            if (.not. is_function_node(arena, func_idx)) then
                new_body_count = new_body_count + 1
                new_body_indices(new_body_count) = func_idx
                cycle
            end if

            func_name = get_function_name(arena, func_idx)
            func_sigs = get_function_signatures(signatures, func_name)

            if (size(func_sigs) <= 1) then
                new_body_count = new_body_count + 1
                new_body_indices(new_body_count) = func_idx
                cycle
            end if

            allocate (variant_indices(size(func_sigs)))
            do j = 1, size(func_sigs)
                variant_indices(j) = clone_function_with_signature( &
                                     arena, func_idx, func_sigs(j))
            end do

            mod_proc_idx = create_module_procedure_node( &
                           arena, func_name, variant_indices)
            interface_idx = create_interface_node( &
                            arena, func_name, mod_proc_idx)
            mod_idx = create_module_for_function( &
                      arena, func_name, interface_idx, variant_indices)
            use_idx = create_use_statement_node( &
                      arena, "auto_"//func_name)

            module_count = module_count + 1
            if (module_count > size(module_indices)) then
                call resize_integer_array(module_indices, module_count * 2)
            end if
            module_indices(module_count) = mod_idx

            new_body_count = new_body_count + 1
            new_body_indices(new_body_count) = use_idx

            deallocate (variant_indices)
            deallocate (func_sigs)
        end do

        do i = 1, module_count
            new_body_count = new_body_count + 1
            new_body_indices(new_body_count) = module_indices(i)
        end do

        prog%body_indices = new_body_indices(1:new_body_count)
    end subroutine transform_monomorphization

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
        character(len=:), allocatable :: lowered_return
        integer :: i
        type(declaration_node) :: decl_copy

        call get_function_node(arena, func_idx, orig_func)
        if (.not. associated(orig_func)) then
            new_idx = 0
            return
        end if

        mangled_name = mangle_procedure_name(orig_func%name, signature%param_kinds)
        return_type = determine_return_type_string(arena, signature, orig_func)

        if (allocated(orig_func%param_indices)) then
            allocate (new_param_indices(size(orig_func%param_indices)))
            do i = 1, size(orig_func%param_indices)
                if (allocated(signature%param_type_strings)) then
                    if (size(signature%param_type_strings) >= i) then
                        new_param_indices(i) = clone_parameter_with_kind( &
                                               arena, orig_func%param_indices(i), &
                                               signature%param_kinds(i), &
                                               signature%param_type_strings(i))
                    else
                        new_param_indices(i) = clone_parameter_with_kind( &
                                               arena, orig_func%param_indices(i), &
                                               signature%param_kinds(i))
                    end if
                else
                    new_param_indices(i) = clone_parameter_with_kind( &
                                           arena, orig_func%param_indices(i), &
                                           signature%param_kinds(i))
                end if
            end do
        else
            allocate (new_param_indices(0))
        end if

        if (allocated(orig_func%result_variable)) then
            if (len_trim(orig_func%result_variable) > 0) then
                result_name = trim(orig_func%result_variable)
            end if
        end if

        if (.not. allocated(result_name)) then
            if (allocated(orig_func%name)) then
                if (len_trim(orig_func%name) > 0) then
                    result_name = trim(orig_func%name)
                end if
            end if
        end if

        if (.not. allocated(result_name)) then
            result_name = mangled_name
        else if (len_trim(result_name) == 0) then
            result_name = mangled_name
        end if

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
                    if (allocated(result_name)) then
                        if (len_trim(result_name) > 0) then
                            if (allocated(decl%var_name)) then
                                if (trim(decl%var_name) == trim(result_name)) then
                                    decl_copy = decl
                                    decl_copy%uid = generate_uid()
                                    if (len_trim(return_type) > 0) then
                                        decl_copy%type_name = trim(return_type)
                                    end if
                                    decl_copy%has_kind = .false.
                                    if (allocated(lowered_return)) then
                                        if (index(lowered_return, &
                                                  "allocatable") > 0) then
                                            decl_copy%is_allocatable = .true.
                                        else
                                            decl_copy%is_allocatable = .false.
                                        end if
                                        if (index(lowered_return, &
                                                  "dimension(") > 0) then
                                            decl_copy%is_array = .true.
                                        else
                                            decl_copy%is_array = .false.
                                        end if
                                    else
                                        decl_copy%is_allocatable = .false.
                                        decl_copy%is_array = .false.
                                    end if
                                    call arena%push(decl_copy)
                                    body_indices_copy(i) = arena%size
                                end if
                            end if
                        end if
                    end if
                end select
            end do
        else
            allocate (body_indices_copy(0))
        end if

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
            proc_names(i)%s = get_function_name(arena, variant_indices(i))
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

    function get_function_signatures(signatures, func_name) result(sigs)
        type(signatures_map_t), intent(in) :: signatures
        character(len=*), intent(in) :: func_name
        type(type_signature_t), allocatable :: sigs(:)
        integer :: num_sigs

        ! Use the type-bound procedure to get signatures
        num_sigs = signatures%get_signatures(func_name, sigs)

        ! If no signatures found, allocate empty array
        if (num_sigs == 0 .and. .not. allocated(sigs)) then
            allocate (sigs(0))
        end if
    end function get_function_signatures

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

end module ast_monomorphization
