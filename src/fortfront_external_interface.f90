module fortfront_external_interface
    ! External Fortran interface for fortfront library
    ! Provides high-level Fortran interface for external Fortran programs
    
    use frontend, only: transform_lazy_fortran_string, compilation_options_t, &
                        compile_source, format_options_t, &
                        transform_lazy_fortran_string_with_format
    use error_handling, only: result_t, success_result, create_error_result, &
                               ERROR_VALIDATION, ERROR_MEMORY, ERROR_PARSER
    
    implicit none
    private

    ! Public interface for external Fortran programs
    public :: fortfront_transform_source, fortfront_transform_source_with_format
    public :: fortfront_compile_file, fortfront_compilation_options_t
    public :: fortfront_format_options_t, fortfront_result_t

    ! Re-export types for external use
    type :: fortfront_compilation_options_t
        logical :: debug_tokens = .false.
        logical :: debug_ast = .false.
        logical :: debug_semantic = .false.
        logical :: debug_standardize = .false.
        logical :: debug_codegen = .false.
        character(len=:), allocatable :: output_file
    end type fortfront_compilation_options_t

    type :: fortfront_format_options_t
        integer :: indent_size = 4
        logical :: use_tabs = .false.
        character(len=1) :: indent_char = ' '
        logical :: standardize_types = .true.
        integer :: line_length = 130
    end type fortfront_format_options_t

    type :: fortfront_result_t
        logical :: success = .false.
        character(len=:), allocatable :: error_message
        character(len=:), allocatable :: output
    end type fortfront_result_t

contains

    ! Transform Fortran source code from string
    function fortfront_transform_source(input_source) result(result_data)
        character(len=*), intent(in) :: input_source
        type(fortfront_result_t) :: result_data
        
        character(len=:), allocatable :: output, error_msg
        
        ! Initialize result
        result_data%success = .false.
        
        ! Validate input
        if (len_trim(input_source) == 0) then
            result_data%error_message = "Empty input source code"
            return
        end if
        
        ! Call the core transformation function
        call transform_lazy_fortran_string(input_source, output, error_msg)
        
        if (error_msg == "") then
            ! Success
            result_data%success = .true.
            result_data%output = output
        else
            ! Error occurred
            result_data%success = .false.
            result_data%error_message = error_msg
            if (allocated(output) .and. len_trim(output) > 0) then
                ! Partial output available
                result_data%output = output
            end if
        end if
    end function fortfront_transform_source

    ! Transform Fortran source code with formatting options
    function fortfront_transform_source_with_format(input_source, format_opts) result(result_data)
        character(len=*), intent(in) :: input_source
        type(fortfront_format_options_t), intent(in) :: format_opts
        type(fortfront_result_t) :: result_data
        
        character(len=:), allocatable :: output, error_msg
        type(format_options_t) :: internal_format_opts
        
        ! Initialize result
        result_data%success = .false.
        
        ! Validate input
        if (len_trim(input_source) == 0) then
            result_data%error_message = "Empty input source code"
            return
        end if
        
        ! Convert external format options to internal format options
        internal_format_opts%indent_size = format_opts%indent_size
        internal_format_opts%use_tabs = format_opts%use_tabs
        internal_format_opts%indent_char = format_opts%indent_char
        internal_format_opts%standardize_types = format_opts%standardize_types
        internal_format_opts%line_length = format_opts%line_length
        
        ! Call the core transformation function with formatting
        call transform_lazy_fortran_string_with_format(input_source, output, error_msg, internal_format_opts)
        
        if (error_msg == "") then
            ! Success
            result_data%success = .true.
            result_data%output = output
        else
            ! Error occurred
            result_data%success = .false.
            result_data%error_message = error_msg
            if (allocated(output) .and. len_trim(output) > 0) then
                ! Partial output available
                result_data%output = output
            end if
        end if
    end function fortfront_transform_source_with_format

    ! Compile a Fortran file
    function fortfront_compile_file(input_file, options) result(result_data)
        character(len=*), intent(in) :: input_file
        type(fortfront_compilation_options_t), intent(in) :: options
        type(fortfront_result_t) :: result_data
        
        character(len=:), allocatable :: error_msg
        type(compilation_options_t) :: internal_options
        
        ! Initialize result
        result_data%success = .false.
        allocate(character(len=0) :: error_msg)
        
        ! Validate input
        if (len_trim(input_file) == 0) then
            result_data%error_message = "Empty input file path"
            return
        end if
        
        ! Convert external options to internal options
        internal_options%debug_tokens = options%debug_tokens
        internal_options%debug_ast = options%debug_ast
        internal_options%debug_semantic = options%debug_semantic
        internal_options%debug_standardize = options%debug_standardize
        internal_options%debug_codegen = options%debug_codegen
        
        if (allocated(options%output_file)) then
            internal_options%output_file = options%output_file
        end if
        
        ! Call the compilation function
        call compile_source(input_file, internal_options, error_msg)
        
        if (error_msg == "") then
            ! Success
            result_data%success = .true.
            if (allocated(options%output_file)) then
                result_data%output = "Compilation successful. Output written to: " // options%output_file
            else
                result_data%output = "Compilation successful. Output written to stdout."
            end if
        else
            ! Error occurred
            result_data%success = .false.
            result_data%error_message = error_msg
        end if
    end function fortfront_compile_file

end module fortfront_external_interface