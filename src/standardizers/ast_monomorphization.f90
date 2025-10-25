module ast_monomorphization
    ! AST-level monomorphization transformation
    ! Operates on typed AST after semantic analysis
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: module_node, create_module
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
                              use_statement_node, create_interface_block, &
                              create_module_procedure, create_use_statement
    use ast_base, only: string_t
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t
    use codegen_name_mangling, only: mangle_procedure_name
    implicit none
    private

    public :: transform_monomorphization

contains

    ! Main entry point: transform AST to add monomorphized variants
    subroutine transform_monomorphization(arena, root_index, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(signatures_map_t), intent(in) :: signatures
        integer :: i, num_sigs, num_procs
        type(function_def_node), pointer :: func
        type(type_signature_t), allocatable :: unique_sigs(:)
        character(len=:), allocatable :: func_name

        ! NOTE: Monomorphization infrastructure is in place
        ! This module provides the foundation for AST-level monomorphization:
        ! - Signature collection during semantic analysis (COMPLETE)
        ! - AST transformation hooks (COMPLETE)
        ! - Node creation utilities (COMPLETE)
        !
        ! What remains for full implementation:
        ! - Parameter type cloning with signature-specific types
        ! - Call site updates to use correct variant
        ! - Integration testing with codegen
        !
        ! For now, signatures are collected and available to codegen for
        ! validation or future use. The transformation logic below is stubbed
        ! to avoid breaking the build.

        ! Early return - full transformation not yet enabled
        return

        ! Traverse entire AST to find all function definitions
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle

            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                func => node
                func_name = func%name

                ! Get signatures for this function
                num_sigs = signatures%get_signatures(func_name, unique_sigs)

                ! If multiple signatures exist, perform monomorphization
                if (num_sigs > 1) then
                    call monomorphize_function(arena, root_index, i, func_name, unique_sigs)
                end if
            end select
        end do

    end subroutine transform_monomorphization

    ! Monomorphize a single function with multiple signatures
    subroutine monomorphize_function(arena, prog_index, func_index, &
                                      func_name, signatures)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: func_name
        type(type_signature_t), intent(in) :: signatures(:)

        integer :: i, num_variants
        integer, allocatable :: variant_indices(:)
        integer :: interface_idx, mod_proc_idx, module_idx, use_stmt_idx
        character(len=:), allocatable :: mangled_name, module_name
        type(function_def_node), pointer :: orig_func
        type(program_node), pointer :: prog

        ! Get original function
        if (.not. arena%has_node_at(func_index)) return
        select type (node => arena%entries(func_index)%node)
        type is (function_def_node)
            orig_func => node
        class default
            return
        end select

        ! Create variants for each signature
        num_variants = size(signatures)
        allocate(variant_indices(num_variants))

        do i = 1, num_variants
            mangled_name = signature_to_mangled_name(func_name, signatures(i))
            variant_indices(i) = clone_function_with_signature(arena, func_index, &
                signatures(i), mangled_name)
        end do

        ! Create module procedure node listing all variants
        mod_proc_idx = create_module_procedure_node(arena, variant_indices, &
            func_name, signatures)

        ! Create interface block with the module procedure
        interface_idx = create_interface_node(arena, func_name, mod_proc_idx)

        ! Create module wrapping everything
        module_name = "auto_" // func_name
        module_idx = create_monomorphization_module(arena, module_name, &
            interface_idx, variant_indices)

        ! Add use statement to program
        use_stmt_idx = create_use_statement_node(arena, module_name)

        ! Update program node to include use statement and remove original function
        call update_program_with_module(arena, prog_index, func_index, &
            use_stmt_idx, module_idx)

    end subroutine monomorphize_function

    ! Clone function with specific signature and mangled name
    function clone_function_with_signature(arena, func_index, signature, &
                                            mangled_name) result(new_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        type(type_signature_t), intent(in) :: signature
        character(len=*), intent(in) :: mangled_name
        integer :: new_index

        type(function_def_node), pointer :: orig_func
        type(function_def_node) :: new_func
        integer :: i

        ! Get original function
        if (.not. arena%has_node_at(func_index)) then
            new_index = 0
            return
        end if

        select type (node => arena%entries(func_index)%node)
        type is (function_def_node)
            orig_func => node
        class default
            new_index = 0
            return
        end select

        ! Clone function structure
        new_func = orig_func

        ! Update name to mangled version
        new_func%name = mangled_name

        ! Update return type based on signature
        new_func%return_type = kind_to_type_string(signature%return_kind)

        ! TODO: Update parameter types based on signature
        ! This requires cloning parameter nodes and updating their types
        ! For now, keeping original parameter structure (types will be inferred)

        ! Push cloned function to arena
        call arena%push(new_func, "function_def")
        new_index = arena%size

    end function clone_function_with_signature

    ! Convert kind value to type string
    function kind_to_type_string(kind_value) result(type_str)
        integer, intent(in) :: kind_value
        character(len=:), allocatable :: type_str

        select case(kind_value)
        case(2)
            type_str = "integer(2)"
        case(4)
            type_str = "integer(4)"
        case(8)
            type_str = "integer(8)"
        case default
            type_str = "integer"
        end select
    end function kind_to_type_string

    ! Convert type_signature_t to mangled name
    function signature_to_mangled_name(base_name, signature) &
        result(mangled_name)
        character(len=*), intent(in) :: base_name
        type(type_signature_t), intent(in) :: signature
        character(len=:), allocatable :: mangled_name
        integer, allocatable :: kinds(:)
        integer :: total_kinds, i

        ! Build kinds array: param_kinds + return_kind
        total_kinds = 0
        if (allocated(signature%param_kinds)) then
            total_kinds = size(signature%param_kinds)
        end if

        allocate(kinds(total_kinds + 1))

        ! Copy param kinds
        if (allocated(signature%param_kinds)) then
            do i = 1, size(signature%param_kinds)
                kinds(i) = signature%param_kinds(i)
            end do
        end if

        ! Append return kind
        kinds(total_kinds + 1) = signature%return_kind

        ! Use name mangling utility
        mangled_name = mangle_procedure_name(base_name, kinds)

    end function signature_to_mangled_name

    ! Create module procedure node
    function create_module_procedure_node(arena, variant_indices, func_name, &
                                          signatures) result(mod_proc_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: variant_indices(:)
        character(len=*), intent(in) :: func_name
        type(type_signature_t), intent(in) :: signatures(:)
        integer :: mod_proc_idx

        type(string_t), allocatable :: proc_names(:)
        type(module_procedure_node) :: mod_proc
        character(len=:), allocatable :: mangled_name
        integer :: i

        ! Build list of procedure names
        allocate(proc_names(size(variant_indices)))
        do i = 1, size(variant_indices)
            mangled_name = signature_to_mangled_name(func_name, signatures(i))
            proc_names(i)%s = mangled_name
        end do

        ! Create module procedure node
        mod_proc = create_module_procedure(procedure_names=proc_names)

        ! Push to arena
        call arena%push(mod_proc, "module_procedure")
        mod_proc_idx = arena%size

    end function create_module_procedure_node

    ! Create interface block node
    function create_interface_node(arena, func_name, mod_proc_idx) &
        result(interface_idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: mod_proc_idx
        integer :: interface_idx

        type(interface_block_node) :: interface_block
        integer, allocatable :: proc_indices(:)

        ! Create interface block with module procedure
        allocate(proc_indices(1))
        proc_indices(1) = mod_proc_idx

        interface_block = create_interface_block(name=func_name, &
            procedure_indices=proc_indices)

        ! Push to arena
        call arena%push(interface_block, "interface_block")
        interface_idx = arena%size

    end function create_interface_node

    ! Create monomorphization module
    function create_monomorphization_module(arena, module_name, &
                                             interface_idx, variant_indices) &
        result(module_idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        integer, intent(in) :: interface_idx
        integer, intent(in) :: variant_indices(:)
        integer :: module_idx

        type(module_node) :: mod
        integer, allocatable :: decl_indices(:), proc_indices(:)

        ! Interface goes in declarations
        allocate(decl_indices(1))
        decl_indices(1) = interface_idx

        ! Variants go in procedures (after contains)
        proc_indices = variant_indices

        ! Create module
        mod = create_module(name=module_name, &
            declaration_indices=decl_indices, &
            procedure_indices=proc_indices, &
            has_contains=.true.)

        ! Push to arena
        call arena%push(mod, "module")
        module_idx = arena%size

    end function create_monomorphization_module

    ! Create use statement node
    function create_use_statement_node(arena, module_name) result(use_idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        integer :: use_idx

        type(use_statement_node) :: use_stmt

        ! Create use statement
        use_stmt = create_use_statement(module_name=module_name)

        ! Push to arena
        call arena%push(use_stmt, "use_statement")
        use_idx = arena%size

    end function create_use_statement_node

    ! Update program node to include use statement and module
    subroutine update_program_with_module(arena, prog_index, old_func_index, &
                                           use_stmt_idx, module_idx)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: old_func_index
        integer, intent(in) :: use_stmt_idx
        integer, intent(in) :: module_idx

        type(program_node), pointer :: prog
        integer, allocatable :: new_body(:)
        integer :: i, j, new_size

        ! Get program node
        if (.not. arena%has_node_at(prog_index)) return

        select type (node => arena%entries(prog_index)%node)
        type is (program_node)
            prog => node
        class default
            return
        end select

        if (.not. allocated(prog%body_indices)) return

        ! Build new body: use statement + module + existing body (minus old function)
        new_size = size(prog%body_indices) + 1  ! +1 for use, -1 for old func, +1 for module
        allocate(new_body(new_size))

        ! Add use statement first
        new_body(1) = use_stmt_idx

        ! Add module second
        new_body(2) = module_idx

        ! Copy remaining items (excluding old function)
        j = 3
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) /= old_func_index) then
                new_body(j) = prog%body_indices(i)
                j = j + 1
            end if
        end do

        ! Update program body
        prog%body_indices = new_body(1:j-1)

    end subroutine update_program_with_module

end module ast_monomorphization
