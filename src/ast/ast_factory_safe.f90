module ast_factory_safe
    use ast_core
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use error_handling, only: result_t, success_result, create_error_result, critical_result, &
                              ERROR_VALIDATION, ERROR_MEMORY, ERROR_INTERNAL
    implicit none
    private

    ! Safe AST factory with structured error handling
    public :: safe_push_program, safe_push_assignment, safe_push_binary_op
    public :: safe_push_call_or_subscript, safe_push_identifier, safe_push_literal
    public :: safe_push_forall, safe_push_where
    public :: safe_push_function_def, safe_push_subroutine_def
    public :: validate_arena, validate_node_index
    
    ! Result type for factory operations
    type, public :: factory_result_t
        type(result_t) :: result
        integer :: node_index = 0  ! Valid only if result%success = .true.
    contains
        procedure :: is_success => factory_is_success
        procedure :: get_index => factory_get_index
        procedure :: get_error => factory_get_error
    end type factory_result_t

contains

    ! Factory result methods
    function factory_is_success(this) result(success)
        class(factory_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success()
    end function factory_is_success

    function factory_get_index(this) result(index)
        class(factory_result_t), intent(in) :: this
        integer :: index
        if (this%result%is_success()) then
            index = this%node_index
        else
            index = 0
        end if
    end function factory_get_index

    function factory_get_error(this) result(error_msg)
        class(factory_result_t), intent(in) :: this
        character(len=:), allocatable :: error_msg
        error_msg = this%result%get_full_message()
    end function factory_get_error

    ! Arena validation utility
    function validate_arena(arena, context) result(validation_result)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: context
        type(result_t) :: validation_result
        
        if (.not. allocated(arena%entries)) then
            validation_result = critical_result( &
                "Arena entries not allocated", &
                ERROR_MEMORY, &
                component="ast_factory", &
                context=context, &
                suggestion="Initialize arena with create_ast_arena() before use" &
            )
            return
        end if
        
        if (arena%size < 0 .or. arena%size > arena%capacity) then
            validation_result = critical_result( &
                "Arena size inconsistent with capacity", &
                ERROR_INTERNAL, &
                component="ast_factory", &
                context=context, &
                suggestion="Check for memory corruption or incorrect initialization" &
            )
            return
        end if
        
        validation_result = success_result()
    end function validate_arena

    ! Node index validation utility  
    function validate_node_index(arena, node_index, context, allow_zero) result(validation_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: context
        logical, intent(in), optional :: allow_zero
        type(result_t) :: validation_result
        logical :: zero_ok
        
        zero_ok = .false.
        if (present(allow_zero)) zero_ok = allow_zero
        
        if (node_index == 0 .and. zero_ok) then
            validation_result = success_result()
            return
        end if
        
        if (node_index <= 0) then
            validation_result = create_error_result( &
                "Invalid node index: must be positive", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context=context, &
                suggestion="Ensure node index comes from valid push_* operation" &
            )
            return
        end if
        
        if (node_index > arena%size) then
            validation_result = create_error_result( &
                "Node index out of bounds", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context=context, &
                suggestion="Check that referenced node was created in this arena" &
            )
            return
        end if
        
        if (.not. allocated(arena%entries(node_index)%node)) then
            validation_result = create_error_result( &
                "Node index references unallocated node", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context=context, &
                suggestion="Ensure referenced node is still valid and not deallocated" &
            )
            return
        end if
        
        validation_result = success_result()
    end function validate_node_index

    ! Safe program node creation
    function safe_push_program(arena, name, body_indices, line, column) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(factory_result_t) :: factory_result
        
        type(program_node) :: prog
        type(result_t) :: validation
        integer :: i
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_program")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate program name
        if (len_trim(name) == 0) then
            factory_result%result = create_error_result( &
                "Program name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_program", &
                suggestion="Provide a valid program name" &
            )
            return
        end if
        
        ! Validate body indices
        do i = 1, size(body_indices)
            validation = validate_node_index(arena, body_indices(i), "safe_push_program body")
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end do
        
        ! Create program node
        prog = create_program(name, body_indices, line, column)
        call arena%push(prog, "program", 0)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_program

    ! Safe assignment node creation
    function safe_push_assignment(arena, target_index, value_index, line, column, &
                                 parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(assignment_node) :: assign
        type(result_t) :: validation
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_assignment")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate target index
        validation = validate_node_index(arena, target_index, "safe_push_assignment target")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate value index
        validation = validate_node_index(arena, value_index, "safe_push_assignment value")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_assignment parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create assignment node
        assign = create_assignment(target_index, value_index, line, column)
        call arena%push(assign, "assignment", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_assignment

    ! Safe binary operation node creation
    function safe_push_binary_op(arena, left_index, right_index, operator, &
                                line, column, parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(binary_op_node) :: binop
        type(result_t) :: validation
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_binary_op")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate operator
        if (len_trim(operator) == 0) then
            factory_result%result = create_error_result( &
                "Binary operator cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_binary_op", &
                suggestion="Provide a valid operator (+, -, *, /, etc.)" &
            )
            return
        end if
        
        ! Validate left operand
        validation = validate_node_index(arena, left_index, "safe_push_binary_op left")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate right operand
        validation = validate_node_index(arena, right_index, "safe_push_binary_op right")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_binary_op parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create binary operation node
        binop = create_binary_op(left_index, right_index, operator, line, column)
        call arena%push(binop, "binary_op", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_binary_op

    ! Safe call/subscript node creation
    function safe_push_call_or_subscript(arena, name, arg_indices, line, column, &
                                        parent_index) result(factory_result)
        use intrinsic_registry, only: get_intrinsic_info
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(call_or_subscript_node) :: call_node
        type(result_t) :: validation
        integer :: i
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_call_or_subscript")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate function/variable name
        if (len_trim(name) == 0) then
            factory_result%result = create_error_result( &
                "Function/variable name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_call_or_subscript", &
                suggestion="Provide a valid identifier name" &
            )
            return
        end if
        
        ! Validate argument indices
        do i = 1, size(arg_indices)
            validation = validate_node_index(arena, arg_indices(i), "safe_push_call_or_subscript argument")
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end do
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_call_or_subscript parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create call/subscript node
        call_node = create_call_or_subscript(name, arg_indices, line, column)
        
        ! Set intrinsic function information safely
        call get_intrinsic_info(name, call_node%is_intrinsic, call_node%intrinsic_signature)
        
        call arena%push(call_node, "call_or_subscript", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_call_or_subscript

    ! Safe identifier node creation
    function safe_push_identifier(arena, name, line, column, parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(identifier_node) :: id
        type(result_t) :: validation
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_identifier")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate identifier name
        if (len_trim(name) == 0) then
            factory_result%result = create_error_result( &
                "Identifier name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_identifier", &
                suggestion="Provide a valid identifier name" &
            )
            return
        end if
        
        ! Check for reasonable identifier length (Fortran limit is 63 characters)
        if (len_trim(name) > 63) then
            factory_result%result = create_error_result( &
                "Identifier name exceeds Fortran standard limit", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_identifier", &
                suggestion="Use identifier names with 63 characters or fewer" &
            )
            return
        end if
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_identifier parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create identifier node
        id = create_identifier(name, line, column)
        call arena%push(id, "identifier", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_identifier

    ! Safe literal node creation
    function safe_push_literal(arena, value, kind, line, column, parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(literal_node) :: lit
        type(result_t) :: validation
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_literal")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate literal value
        if (len_trim(value) == 0) then
            factory_result%result = create_error_result( &
                "Literal value cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_literal", &
                suggestion="Provide a valid literal value" &
            )
            return
        end if
        
        ! Validate kind (basic check for known literal kinds)
        select case (kind)
        case (LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL, LITERAL_COMPLEX)
            ! Valid kinds
        case default
            factory_result%result = create_error_result( &
                "Unknown literal kind", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_literal", &
                suggestion="Use valid literal kind constants (LITERAL_INTEGER, LITERAL_REAL, etc.)" &
            )
            return
        end select
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_literal parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create literal node
        lit = create_literal(value, kind, line, column)
        call arena%push(lit, "literal", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_literal

    ! Safe FORALL node creation - replaces error_stop pattern
    function safe_push_forall(arena, index_var, start_index, end_index, step_index, &
                             mask_index, body_indices, line, column, parent_index) result(factory_result)
        use ast_nodes_control, only: MAX_INDEX_NAME_LENGTH
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: index_var
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index, mask_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(forall_node) :: forall_stmt
        type(result_t) :: validation
        integer :: i, index_var_len
        
        ! Validate arena (replaces error_stop "Arena not properly initialized")
        validation = validate_arena(arena, "safe_push_forall")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate index variable name (replaces error_stop "Empty index variable name")
        index_var_len = len_trim(index_var)
        if (index_var_len == 0) then
            factory_result%result = create_error_result( &
                "FORALL index variable name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_forall", &
                suggestion="Provide a valid index variable name" &
            )
            return
        end if
        
        ! Validate index variable length (replaces error_stop "Index variable name too long")
        if (index_var_len > MAX_INDEX_NAME_LENGTH) then
            factory_result%result = create_error_result( &
                "FORALL index variable name exceeds maximum length", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_forall", &
                suggestion="Use shorter index variable name" &
            )
            return
        end if
        
        ! Validate start index (replaces error_stop "Invalid start index")
        validation = validate_node_index(arena, start_index, "safe_push_forall start")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate end index (replaces error_stop "Invalid end index")
        validation = validate_node_index(arena, end_index, "safe_push_forall end")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate optional step index (replaces error_stop "Invalid step index")
        if (present(step_index)) then
            if (step_index > 0) then
                validation = validate_node_index(arena, step_index, "safe_push_forall step")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end if
        end if
        
        ! Validate optional mask index (replaces error_stop "Invalid mask index")
        if (present(mask_index)) then
            if (mask_index > 0) then
                validation = validate_node_index(arena, mask_index, "safe_push_forall mask")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end if
        end if
        
        ! Validate body indices (replaces error_stop "Invalid body index")
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                validation = validate_node_index(arena, body_indices(i), "safe_push_forall body")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! All validations passed - create FORALL node
        ! For simple single-index FORALL, use first element of arrays
        forall_stmt%num_indices = 1
        allocate(character(len=len(index_var)) :: forall_stmt%index_names(1))
        forall_stmt%index_names(1) = index_var

        ! Set start and end expression indices
        allocate(forall_stmt%lower_bound_indices(1))
        allocate(forall_stmt%upper_bound_indices(1))
        allocate(forall_stmt%stride_indices(1))
        
        forall_stmt%lower_bound_indices(1) = start_index
        forall_stmt%upper_bound_indices(1) = end_index

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                forall_stmt%stride_indices(1) = step_index
            else
                forall_stmt%stride_indices(1) = 0
            end if
        else
            forall_stmt%stride_indices(1) = 0
        end if

        ! Set optional mask expression index
        if (present(mask_index)) then
            if (mask_index > 0) then
                forall_stmt%has_mask = .true.
                forall_stmt%mask_expr_index = mask_index
            else
                forall_stmt%has_mask = .false.
            end if
        else
            forall_stmt%has_mask = .false.
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                forall_stmt%body_indices = body_indices
            end if
        end if

        if (present(line)) forall_stmt%line = line
        if (present(column)) forall_stmt%column = column

        call arena%push(forall_stmt, "forall", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_forall

    ! Safe WHERE node creation - replaces error_stop pattern
    function safe_push_where(arena, mask_expr_index, where_body_indices, &
                            elsewhere_body_indices, line, column, parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(where_node) :: where_stmt
        type(result_t) :: validation
        integer :: i
        
        ! Validate arena (replaces error_stop "Arena not properly initialized")
        validation = validate_arena(arena, "safe_push_where")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate mask expression index (replaces error_stop "Invalid mask expression index")
        validation = validate_node_index(arena, mask_expr_index, "safe_push_where mask")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate where body indices (replaces error_stop "Invalid where body index")
        if (present(where_body_indices)) then
            do i = 1, size(where_body_indices)
                validation = validate_node_index(arena, where_body_indices(i), "safe_push_where body")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! Validate elsewhere body indices (replaces error_stop "Invalid elsewhere body index")
        if (present(elsewhere_body_indices)) then
            do i = 1, size(elsewhere_body_indices)
                validation = validate_node_index(arena, elsewhere_body_indices(i), "safe_push_where elsewhere")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! All validations passed - create WHERE node
        where_stmt = create_where(mask_expr_index=mask_expr_index, &
                                  where_body_indices=where_body_indices, &
                                  elsewhere_body_indices=elsewhere_body_indices, &
                                  line=line, column=column)

        call arena%push(where_stmt, "where_node", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_where

    ! Safe function definition creation
    function safe_push_function_def(arena, name, param_indices, return_type, body_indices, &
                                   line, column, parent_index, result_variable) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:), body_indices(:)
        character(len=*), intent(in), optional :: return_type, result_variable
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(function_def_node) :: func_def
        type(result_t) :: validation
        integer :: i
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_function_def")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate function name
        if (len_trim(name) == 0) then
            factory_result%result = create_error_result( &
                "Function name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_function_def", &
                suggestion="Provide a valid function name" &
            )
            return
        end if
        
        ! Validate parameter indices
        if (present(param_indices)) then
            do i = 1, size(param_indices)
                validation = validate_node_index(arena, param_indices(i), "safe_push_function_def parameter")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! Validate body indices
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                validation = validate_node_index(arena, body_indices(i), "safe_push_function_def body")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_function_def parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create function definition
        func_def = create_function_def(name, param_indices, return_type, &
                                       body_indices, line, column, result_variable)
        call arena%push(func_def, "function_def", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_function_def

    ! Safe subroutine definition creation
    function safe_push_subroutine_def(arena, name, param_indices, body_indices, &
                                     line, column, parent_index) result(factory_result)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:), body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        type(factory_result_t) :: factory_result
        
        type(subroutine_def_node) :: sub_def
        type(result_t) :: validation
        integer :: i
        
        ! Validate arena
        validation = validate_arena(arena, "safe_push_subroutine_def")
        if (validation%is_failure()) then
            factory_result%result = validation
            return
        end if
        
        ! Validate subroutine name
        if (len_trim(name) == 0) then
            factory_result%result = create_error_result( &
                "Subroutine name cannot be empty", &
                ERROR_VALIDATION, &
                component="ast_factory", &
                context="safe_push_subroutine_def", &
                suggestion="Provide a valid subroutine name" &
            )
            return
        end if
        
        ! Validate parameter indices
        if (present(param_indices)) then
            do i = 1, size(param_indices)
                validation = validate_node_index(arena, param_indices(i), "safe_push_subroutine_def parameter")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! Validate body indices
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                validation = validate_node_index(arena, body_indices(i), "safe_push_subroutine_def body")
                if (validation%is_failure()) then
                    factory_result%result = validation
                    return
                end if
            end do
        end if
        
        ! Validate parent index if provided
        if (present(parent_index)) then
            validation = validate_node_index(arena, parent_index, "safe_push_subroutine_def parent", allow_zero=.true.)
            if (validation%is_failure()) then
                factory_result%result = validation
                return
            end if
        end if
        
        ! Create subroutine definition
        sub_def = create_subroutine_def(name, param_indices, body_indices, line, column)
        call arena%push(sub_def, "subroutine_def", parent_index)
        
        factory_result%node_index = arena%size
        factory_result%result = success_result()
    end function safe_push_subroutine_def

end module ast_factory_safe