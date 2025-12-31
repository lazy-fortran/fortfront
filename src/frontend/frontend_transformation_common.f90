module frontend_transformation_common
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use semantic_operating_mode, only: OPERATING_MODE_INFER
    use compiler_arena, only: compiler_arena_t
    implicit none
    private

    public :: format_options_t
    public :: transform_context_t
    public :: shared_arena
    public :: shared_arena_initialized

    type(compiler_arena_t), save :: shared_arena
    logical, save :: shared_arena_initialized = .false.

    type :: format_options_t
        integer :: indent_size = 4
        logical :: use_tabs = .false.
        character(len=1) :: indent_char = ' '
        logical :: standardize_types = .true.
        integer :: line_length = 130
    end type format_options_t

    type :: transform_context_t
        character(len=:), allocatable :: source_name
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: program_name
        logical :: has_filename = .false.
        integer :: input_mode = INPUT_MODE_LAZY
        integer :: operating_mode = OPERATING_MODE_INFER
    end type transform_context_t

end module frontend_transformation_common
