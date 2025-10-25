module ast_monomorphization
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node, &
                                   get_procedure_name, get_procedure_params, &
                                   get_procedure_body, get_procedure_return_type, &
                                   create_function_def
    use ast_nodes_data, only: module_node, parameter_declaration_node, &
                              create_module, INTENT_IN
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
                              use_statement_node, create_interface_block, &
                              create_module_procedure, create_use_statement
    use ast_base, only: string_t
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t
    use codegen_name_mangling, only: mangle_procedure_name
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: transform_monomorphization

contains

    subroutine transform_monomorphization(arena, root_index, signatures)
        use, intrinsic :: iso_fortran_env, only: error_unit
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
        character(len=:), allocatable :: mangled_name, return_type
        integer :: i

        call get_function_node(arena, func_idx, orig_func)
        if (.not. associated(orig_func)) then
            new_idx = 0
            return
        end if

        mangled_name = mangle_procedure_name(orig_func%name, signature%param_kinds)
        return_type = get_kind_type_string(signature%return_kind)

        if (allocated(orig_func%param_indices)) then
            allocate (new_param_indices(size(orig_func%param_indices)))
            do i = 1, size(orig_func%param_indices)
                new_param_indices(i) = clone_parameter_with_kind(arena, &
                    orig_func%param_indices(i), signature%param_kinds(i))
            end do
        else
            allocate (new_param_indices(0))
        end if

        new_func = create_function_def( &
                   name=mangled_name, &
                   param_indices=new_param_indices, &
                   return_type=return_type, &
                   body_indices=orig_func%body_indices, &
                   line=orig_func%line, &
                   column=orig_func%column, &
                   result_variable=mangled_name)

        call arena%push(new_func)
        new_idx = arena%size
    end function clone_function_with_signature

    function clone_parameter_with_kind(arena, param_idx, kind_value) result(new_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: param_idx
        integer, intent(in) :: kind_value
        integer :: new_idx
        type(parameter_declaration_node), pointer :: orig_param
        type(parameter_declaration_node) :: new_param
        character(len=:), allocatable :: type_name

        call get_parameter_node(arena, param_idx, orig_param)
        if (.not. associated(orig_param)) then
            new_idx = 0
            return
        end if

        type_name = get_kind_type_string(kind_value)

        new_param%uid = generate_uid()
        new_param%name = orig_param%name
        new_param%type_name = type_name
        new_param%kind_value = get_actual_kind_value(kind_value)
        new_param%has_kind = .true.
        new_param%intent_type = INTENT_IN
        new_param%is_optional = orig_param%is_optional
        new_param%is_target = orig_param%is_target
        new_param%is_array = orig_param%is_array
        if (allocated(orig_param%dimension_indices)) then
            new_param%dimension_indices = orig_param%dimension_indices
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
        case default
            type_str = "integer"  ! Default fallback
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
            allocate(sigs(0))
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
