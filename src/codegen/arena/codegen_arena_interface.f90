module codegen_arena_interface
    use ast_arena_modern, only: ast_arena_t
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

    public :: set_arena_generator, generate_code_from_arena

contains

    ! Set the arena generator implementation
    subroutine set_arena_generator(generator)
        procedure(arena_generator_interface) :: generator
        arena_generator => generator
    end subroutine set_arena_generator

    ! Main dispatcher function
    recursive function generate_code_from_arena(arena, node_index) result(code)
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

end module codegen_arena_interface
