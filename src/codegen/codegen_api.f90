module codegen_api
    ! Public code generation API for library consumers
    ! Provides Standard Fortran code generation from AST
    use codegen_arena_interface, only: &
        generate_code_from_arena, &
        set_arena_generator
    use frontend_core, only: emit_fortran
    use codegen_core, only: &
        generate_code_polymorphic, &
        initialize_codegen
    use codegen_type_utils, only: &
        set_type_standardization, &
        get_type_standardization
    use codegen_basic_utils, only: &
        add_line_continuations
    use codegen_indent, only: &
        set_indent_config, &
        get_indent_config, &
        set_line_length_config, &
        get_line_length_config

    implicit none
    private

    ! Main code generation functions
    public :: generate_code_from_arena
    public :: generate_code_polymorphic
    public :: initialize_codegen
    public :: emit_fortran

    ! Configuration functions
    public :: set_type_standardization
    public :: get_type_standardization
    public :: set_indent_config
    public :: get_indent_config
    public :: set_line_length_config
    public :: get_line_length_config

    ! Utility functions
    public :: add_line_continuations
    public :: set_arena_generator

end module codegen_api
