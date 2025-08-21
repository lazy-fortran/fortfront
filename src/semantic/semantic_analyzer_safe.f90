module semantic_analyzer_safe
    use ast_core
    use type_system_hm
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use error_handling, only: result_t, success_result, create_error_result, critical_result, &
                              error_collection_t, create_error_collection, &
                              ERROR_TYPE_SYSTEM, ERROR_SEMANTIC, ERROR_INTERNAL
    implicit none
    private

    ! Safe semantic analyzer with structured error handling
    type, public :: safe_semantic_result_t
        type(result_t) :: result
        type(semantic_context_t) :: context
        logical :: context_valid = .false.
    contains
        procedure :: is_success => semantic_is_success
        procedure :: get_context => semantic_get_context
        procedure :: get_error => semantic_get_error
    end type safe_semantic_result_t

    public :: safe_analyze_assignment, safe_analyze_binary_op, safe_analyze_literal
    public :: safe_analyze_call_or_subscript, safe_unify_types
    public :: create_safe_semantic_context

contains

    ! Safe semantic result methods
    function semantic_is_success(this) result(success)
        class(safe_semantic_result_t), intent(in) :: this
        logical :: success
        success = this%result%is_success() .and. this%context_valid
    end function semantic_is_success

    function semantic_get_context(this) result(context)
        class(safe_semantic_result_t), intent(in) :: this
        type(semantic_context_t) :: context
        if (this%context_valid) then
            context = this%context
        else
            ! Return empty context for failed operations
            context = create_semantic_context()
        end if
    end function semantic_get_context

    function semantic_get_error(this) result(error_msg)
        class(safe_semantic_result_t), intent(in) :: this
        character(len=:), allocatable :: error_msg
        error_msg = this%result%get_full_message()
    end function semantic_get_error

    ! Create safe semantic context
    function create_safe_semantic_context() result(context)
        type(semantic_context_t) :: context
        context = create_semantic_context()
    end function create_safe_semantic_context

    ! Safe assignment analysis - replaces error_stop patterns
    function safe_analyze_assignment(arena, assign_index, context) result(semantic_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        type(semantic_context_t), intent(inout) :: context
        type(safe_semantic_result_t) :: semantic_result
        
        class(ast_node), allocatable :: node, target_node, value_node
        type(result_t) :: temp_result
        
        ! Validate assignment node index
        if (assign_index <= 0 .or. assign_index > arena%size) then
            semantic_result%result = create_error_result( &
                "Invalid assignment node index", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_assignment", &
                suggestion="Ensure assignment node was created properly" &
            )
            return
        end if
        
        if (.not. allocated(arena%entries(assign_index)%node)) then
            semantic_result%result = create_error_result( &
                "Assignment node not allocated", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_assignment", &
                suggestion="Check that assignment node is still valid" &
            )
            return
        end if
        
        ! Get assignment node
        ! TODO: Implement GCC 15.2.1 safe polymorphic copying
        ! Temporarily disabled to avoid 'allocate source=' compatibility issues
        ! allocate(node, source=arena%entries(assign_index)%node)
        
        select type (assignment => node)
        type is (assignment_node)
            ! Validate target and value indices
            if (assignment%target_index <= 0 .or. assignment%target_index > arena%size) then
                semantic_result%result = create_error_result( &
                    "Assignment target index is invalid", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_assignment", &
                    suggestion="Check assignment node creation" &
                )
                return
            end if
            
            if (assignment%value_index <= 0 .or. assignment%value_index > arena%size) then
                semantic_result%result = create_error_result( &
                    "Assignment value index is invalid", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_assignment", &
                    suggestion="Check assignment node creation" &
                )
                return
            end if
            
            ! Get target and value nodes
            if (.not. allocated(arena%entries(assignment%target_index)%node)) then
                semantic_result%result = create_error_result( &
                    "Assignment target node not allocated", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_assignment" &
                )
                return
            end if
            
            if (.not. allocated(arena%entries(assignment%value_index)%node)) then
                semantic_result%result = create_error_result( &
                    "Assignment value node not allocated", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_assignment" &
                )
                return
            end if
            
            ! TODO: Implement GCC 15.2.1 safe polymorphic copying
            ! Temporarily disabled to avoid 'allocate source=' compatibility issues
            ! allocate(target_node, source=arena%entries(assignment%target_index)%node)
            ! allocate(value_node, source=arena%entries(assignment%value_index)%node)
            
            ! Validate assignment target (replaces error_stop "Assignment target must be identifier")
            select type (target => target_node)
            type is (identifier_node)
                ! Valid assignment target
            class default
                semantic_result%result = create_error_result( &
                    "Assignment target must be an identifier", &
                    ERROR_TYPE_SYSTEM, &
                    component="semantic_analyzer", &
                    context="safe_analyze_assignment", &
                    suggestion="Use variable name as assignment target" &
                )
                return
            end select
            
            ! Type analysis would continue here...
            ! For now, return success
            semantic_result%context = context
            semantic_result%context_valid = .true.
            semantic_result%result = success_result()
            
        class default
            semantic_result%result = create_error_result( &
                "Node is not an assignment", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_assignment", &
                suggestion="Ensure node type matches expected assignment" &
            )
            return
        end select
    end function safe_analyze_assignment

    ! Safe binary operation analysis - replaces error_stop patterns
    function safe_analyze_binary_op(arena, binop_index, context) result(semantic_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binop_index
        type(semantic_context_t), intent(inout) :: context
        type(safe_semantic_result_t) :: semantic_result
        
        class(ast_node), allocatable :: node
        
        ! Validate binary operation node index
        if (binop_index <= 0 .or. binop_index > arena%size) then
            semantic_result%result = create_error_result( &
                "Invalid binary operation node index", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_binary_op" &
            )
            return
        end if
        
        if (.not. allocated(arena%entries(binop_index)%node)) then
            semantic_result%result = create_error_result( &
                "Binary operation node not allocated", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_binary_op" &
            )
            return
        end if
        
        ! Get binary operation node
        ! TODO: Implement GCC 15.2.1 safe polymorphic copying
        ! Temporarily disabled to avoid 'allocate source=' compatibility issues
        ! allocate(node, source=arena%entries(binop_index)%node)
        
        select type (binop => node)
        type is (binary_op_node)
            ! Validate operator (replaces error_stop "Unknown binary operator")
            select case (trim(binop%operator))
            case ("+", "-", "*", "/", "**", "//")
                ! Arithmetic operators
            case ("==", "/=", "<", "<=", ">", ">=")
                ! Comparison operators
            case (".and.", ".or.", ".not.", ".eqv.", ".neqv.")
                ! Logical operators
            case default
                semantic_result%result = create_error_result( &
                    "Unknown binary operator: " // trim(binop%operator), &
                    ERROR_TYPE_SYSTEM, &
                    component="semantic_analyzer", &
                    context="safe_analyze_binary_op", &
                    suggestion="Use valid Fortran operators (+, -, *, /, ==, etc.)" &
                )
                return
            end select
            
            ! Validate operand indices
            if (binop%left_index <= 0 .or. binop%left_index > arena%size) then
                semantic_result%result = create_error_result( &
                    "Invalid left operand index", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_binary_op" &
                )
                return
            end if
            
            if (binop%right_index <= 0 .or. binop%right_index > arena%size) then
                semantic_result%result = create_error_result( &
                    "Invalid right operand index", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_binary_op" &
                )
                return
            end if
            
            ! Type analysis would continue here...
            semantic_result%context = context
            semantic_result%context_valid = .true.
            semantic_result%result = success_result()
            
        class default
            semantic_result%result = create_error_result( &
                "Node is not a binary operation", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_binary_op" &
            )
            return
        end select
    end function safe_analyze_binary_op

    ! Safe literal analysis - replaces error_stop patterns
    function safe_analyze_literal(arena, literal_index, context) result(semantic_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: literal_index
        type(semantic_context_t), intent(inout) :: context
        type(safe_semantic_result_t) :: semantic_result
        
        class(ast_node), allocatable :: node
        
        ! Validate literal node index
        if (literal_index <= 0 .or. literal_index > arena%size) then
            semantic_result%result = create_error_result( &
                "Invalid literal node index", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_literal" &
            )
            return
        end if
        
        if (.not. allocated(arena%entries(literal_index)%node)) then
            semantic_result%result = create_error_result( &
                "Literal node not allocated", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_literal" &
            )
            return
        end if
        
        ! Get literal node
        ! TODO: Implement GCC 15.2.1 safe polymorphic copying
        ! Temporarily disabled to avoid 'allocate source=' compatibility issues
        ! allocate(node, source=arena%entries(literal_index)%node)
        
        select type (literal => node)
        type is (literal_node)
            ! Validate literal kind (replaces error_stop "Unknown literal kind")
            select case (literal%literal_kind)
            case (LITERAL_INTEGER)
                ! Integer literal
            case (LITERAL_REAL)
                ! Real literal
            case (LITERAL_STRING)
                ! String literal
            case (LITERAL_LOGICAL)
                ! Logical literal
            case (LITERAL_COMPLEX)
                ! Complex literal
            case default
                semantic_result%result = create_error_result( &
                    "Unknown literal kind", &
                    ERROR_TYPE_SYSTEM, &
                    component="semantic_analyzer", &
                    context="safe_analyze_literal", &
                    suggestion="Use valid literal kind constants" &
                )
                return
            end select
            
            ! Type analysis would continue here...
            semantic_result%context = context
            semantic_result%context_valid = .true.
            semantic_result%result = success_result()
            
        class default
            semantic_result%result = create_error_result( &
                "Node is not a literal", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_literal" &
            )
            return
        end select
    end function safe_analyze_literal

    ! Safe call/subscript analysis
    function safe_analyze_call_or_subscript(arena, call_index, context) result(semantic_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        type(semantic_context_t), intent(inout) :: context
        type(safe_semantic_result_t) :: semantic_result
        
        class(ast_node), allocatable :: node
        integer :: i
        
        ! Validate call node index
        if (call_index <= 0 .or. call_index > arena%size) then
            semantic_result%result = create_error_result( &
                "Invalid call/subscript node index", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_call_or_subscript" &
            )
            return
        end if
        
        if (.not. allocated(arena%entries(call_index)%node)) then
            semantic_result%result = create_error_result( &
                "Call/subscript node not allocated", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_call_or_subscript" &
            )
            return
        end if
        
        ! Get call/subscript node
        ! TODO: Implement GCC 15.2.1 safe polymorphic copying
        ! Temporarily disabled to avoid 'allocate source=' compatibility issues
        ! allocate(node, source=arena%entries(call_index)%node)
        
        select type (call_node => node)
        type is (call_or_subscript_node)
            ! Validate function/variable name
            if (len_trim(call_node%name) == 0) then
                semantic_result%result = create_error_result( &
                    "Call/subscript name cannot be empty", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="safe_analyze_call_or_subscript" &
                )
                return
            end if
            
            ! Validate argument indices
            if (allocated(call_node%arg_indices)) then
                do i = 1, size(call_node%arg_indices)
                    if (call_node%arg_indices(i) <= 0 .or. call_node%arg_indices(i) > arena%size) then
                        semantic_result%result = create_error_result( &
                            "Invalid argument index in call/subscript", &
                            ERROR_SEMANTIC, &
                            component="semantic_analyzer", &
                            context="safe_analyze_call_or_subscript" &
                        )
                        return
                    end if
                    
                    if (.not. allocated(arena%entries(call_node%arg_indices(i))%node)) then
                        semantic_result%result = create_error_result( &
                            "Call/subscript argument node not allocated", &
                            ERROR_SEMANTIC, &
                            component="semantic_analyzer", &
                            context="safe_analyze_call_or_subscript" &
                        )
                        return
                    end if
                end do
            end if
            
            ! Type analysis would continue here...
            semantic_result%context = context
            semantic_result%context_valid = .true.
            semantic_result%result = success_result()
            
        class default
            semantic_result%result = create_error_result( &
                "Node is not a call or subscript", &
                ERROR_SEMANTIC, &
                component="semantic_analyzer", &
                context="safe_analyze_call_or_subscript" &
            )
            return
        end select
    end function safe_analyze_call_or_subscript

    ! Safe type unification - replaces error_stop patterns in type system
    function safe_unify_types(type1, type2, context) result(unify_result)
        type(mono_type_t), intent(in) :: type1, type2
        character(len=*), intent(in) :: context
        type(result_t) :: unify_result
        
        ! Basic type compatibility check (simplified)
        if (type1%var%id == type2%var%id) then
            unify_result = success_result()
            return
        end if
        
        ! Check for type conversion possibilities
        select case (type1%var%id)
        case (TINT)
            select case (type2%var%id)
            case (TREAL)
                ! Integer can be promoted to real
                unify_result = success_result()
                return
            case default
                unify_result = create_error_result( &
                    "Cannot unify integer with incompatible type", &
                    ERROR_TYPE_SYSTEM, &
                    component="type_system", &
                    context=context, &
                    suggestion="Check type compatibility or add explicit conversion" &
                )
                return
            end select
        case (TREAL)
            select case (type2%var%id)
            case (TINT)
                ! Integer can be promoted to real
                unify_result = success_result()
                return
            case default
                unify_result = create_error_result( &
                    "Cannot unify real with incompatible type", &
                    ERROR_TYPE_SYSTEM, &
                    component="type_system", &
                    context=context &
                )
                return
            end select
        case default
            unify_result = create_error_result( &
                "Type unification failed for unknown types", &
                ERROR_TYPE_SYSTEM, &
                component="type_system", &
                context=context, &
                suggestion="Ensure both types are properly defined" &
            )
            return
        end select
    end function safe_unify_types

end module semantic_analyzer_safe