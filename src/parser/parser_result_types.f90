module parser_result_types
    use error_handling, only: result_t, success_result, create_error_result, &
                             ERROR_ERROR, ERROR_PARSER, error_collection_t, &
                             create_error_collection, ERROR_WARNING
    implicit none
    private

    ! Parser result type for single AST nodes
    type, public :: parse_result_t
        type(result_t) :: result
        integer :: node_index = 0  ! AST node index if successful
    contains
        procedure :: is_success => parse_result_is_success
        procedure :: is_failure => parse_result_is_failure
        procedure :: get_node => parse_result_get_node
        procedure :: set_error => parse_result_set_error
        procedure :: set_success => parse_result_set_success
    end type parse_result_t

    ! Compilation result for top-level parsing
    type, public :: compile_result_t
        type(result_t) :: result
        integer :: program_index = 0  ! Root AST node if successful
        type(error_collection_t) :: warnings
    contains
        procedure :: is_success => compile_result_is_success
        procedure :: is_failure => compile_result_is_failure
        procedure :: get_program => compile_result_get_program
        procedure :: has_warnings => compile_result_has_warnings
        procedure :: get_warning_count => compile_result_get_warning_count
        procedure :: add_warning => compile_result_add_warning
        procedure :: set_error => compile_result_set_error
        procedure :: set_success => compile_result_set_success
    end type compile_result_t

    ! Factory functions
    public :: success_parse_result, error_parse_result
    public :: success_compile_result, error_compile_result

contains

    ! parse_result_t methods
    pure function parse_result_is_success(this) result(success)
        class(parse_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success()
    end function parse_result_is_success

    pure function parse_result_is_failure(this) result(failure)
        class(parse_result_t), intent(in) :: this
        logical :: failure
        failure = this%result%is_failure()
    end function parse_result_is_failure

    pure function parse_result_get_node(this) result(node_index)
        class(parse_result_t), intent(in) :: this
        integer :: node_index
        node_index = this%node_index
    end function parse_result_get_node

    subroutine parse_result_set_error(this, message, code, component, context, suggestion)
        class(parse_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        call this%result%set_error(message, code, component, context, suggestion)
        this%node_index = 0
    end subroutine parse_result_set_error

    subroutine parse_result_set_success(this, node_index)
        class(parse_result_t), intent(inout) :: this
        integer, intent(in) :: node_index
        
        this%result = success_result()
        this%node_index = node_index
    end subroutine parse_result_set_success

    ! compile_result_t methods
    pure function compile_result_is_success(this) result(success)
        class(compile_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success()
    end function compile_result_is_success

    pure function compile_result_is_failure(this) result(failure)
        class(compile_result_t), intent(in) :: this
        logical :: failure
        failure = this%result%is_failure()
    end function compile_result_is_failure

    pure function compile_result_get_program(this) result(program_index)
        class(compile_result_t), intent(in) :: this
        integer :: program_index
        program_index = this%program_index
    end function compile_result_get_program

    function compile_result_has_warnings(this) result(has_warn)
        class(compile_result_t), intent(in) :: this
        logical :: has_warn
        has_warn = this%warnings%get_error_count() > 0
    end function compile_result_has_warnings

    function compile_result_get_warning_count(this) result(count)
        class(compile_result_t), intent(in) :: this
        integer :: count
        count = this%warnings%get_error_count()
    end function compile_result_get_warning_count

    subroutine compile_result_add_warning(this, message, code, component, context, suggestion)
        class(compile_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        ! Initialize warnings collection if not already done
        if (this%warnings%get_error_count() == 0 .and. .not. allocated(this%warnings%errors)) then
            this%warnings = create_error_collection()
        end if
        
        call this%warnings%add_error(message, code, severity=ERROR_WARNING, &
                                     component=component, context=context, &
                                     suggestion=suggestion)
    end subroutine compile_result_add_warning

    subroutine compile_result_set_error(this, message, code, component, context, suggestion)
        class(compile_result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        call this%result%set_error(message, code, component, context, suggestion)
        this%program_index = 0
    end subroutine compile_result_set_error

    subroutine compile_result_set_success(this, program_index)
        class(compile_result_t), intent(inout) :: this
        integer, intent(in) :: program_index
        
        this%result = success_result()
        this%program_index = program_index
    end subroutine compile_result_set_success

    ! Factory functions
    function success_parse_result(node_index) result(parse_res)
        integer, intent(in) :: node_index
        type(parse_result_t) :: parse_res
        
        call parse_res%set_success(node_index)
    end function success_parse_result

    function error_parse_result(message, code, component, context, suggestion) result(parse_res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(parse_result_t) :: parse_res
        
        call parse_res%set_error(message, code, component, context, suggestion)
    end function error_parse_result

    function success_compile_result(program_index) result(compile_res)
        integer, intent(in) :: program_index
        type(compile_result_t) :: compile_res
        
        ! Initialize warnings collection
        compile_res%warnings = create_error_collection()
        call compile_res%set_success(program_index)
    end function success_compile_result

    function error_compile_result(message, code, component, context, suggestion) result(compile_res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(compile_result_t) :: compile_res
        
        ! Initialize warnings collection
        compile_res%warnings = create_error_collection()
        call compile_res%set_error(message, code, component, context, suggestion)
    end function error_compile_result

end module parser_result_types