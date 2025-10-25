module codegen_arena_interface
    use ast_arena_modern, only: ast_arena_t
    use call_graph_signatures_mod, only: signatures_map_t
    implicit none
    private

    ! Interface for code generation from arena
    abstract interface
        function arena_generator_interface(arena, node_index) result(code)
            import :: ast_arena_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            character(len=:), allocatable :: code
        end function arena_generator_interface
    end interface

    ! Module variable to hold the actual implementation
    procedure(arena_generator_interface), pointer :: arena_generator => null()

    ! Module variable to hold call site signatures for monomorphization
    type(signatures_map_t), save :: global_signatures

    public :: set_arena_generator, generate_code_from_arena
    public :: set_global_signatures, get_global_signatures

contains

    ! Set the arena generator implementation
    subroutine set_arena_generator(generator)
        procedure(arena_generator_interface) :: generator
        arena_generator => generator
    end subroutine set_arena_generator

    ! Main dispatcher function
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (.not. associated(arena_generator)) then
            ! Provide an actionable diagnostic if initialization was skipped
            code = "! ERROR: Arena generator not set (call initialize_codegen())"
            return
        end if

        code = arena_generator(arena, node_index)
    end function generate_code_from_arena

    ! Set the global signatures map for monomorphization
    subroutine set_global_signatures(signatures)
        type(signatures_map_t), intent(in) :: signatures
        global_signatures = signatures
    end subroutine set_global_signatures

    ! Get the global signatures map
    function get_global_signatures() result(signatures)
        type(signatures_map_t) :: signatures
        signatures = global_signatures
    end function get_global_signatures

end module codegen_arena_interface
