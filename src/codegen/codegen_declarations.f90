module codegen_declarations
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_data
    use ast_nodes_misc, only: implicit_statement_node
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_utilities, only: parameter_info_t, int_to_string, &
        generate_grouped_body, generate_grouped_body_with_params, &
        generate_grouped_body_context, find_parameter_info
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_type_utils, only: get_type_standardization
    implicit none
    private

    public :: generate_code_function_def
    public :: generate_code_subroutine_def
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_module
    public :: generate_code_derived_type
    public :: generate_code_program

contains

    ! Generate code for function definitions
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: return_type_code, params_code, body_code
        integer :: i

        ! Start function definition with return type
        if (allocated(node%return_type) .and. len_trim(node%return_type) > 0) then
            code = node%return_type // " function " // node%name
        else
            code = "function " // node%name
        end if

        ! Generate parameters (names only)
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code // "("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code // ", "
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (p => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            code = code // p%name
                        type is (parameter_declaration_node)
                            code = code // p%name
                        type is (declaration_node)
                            code = code // p%var_name
                        class default
                            code = code // "param"//trim(adjustl(int_to_string(i)))
                        end select
                    end if
                end if
            end do
            code = code // ")"
        else
            code = code // "()"
        end if

        ! Add result clause if present
        if (allocated(node%result_variable) .and. len_trim(node%result_variable) > 0) then
            code = code // " result(" // node%result_variable // ")"
        end if

        code = code // new_line('A')

        ! Build parameter map by matching parameter names to body declarations
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, j
            
            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)
            
            allocate(param_map(param_count))
            
            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.
                
                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        type is (parameter_declaration_node)
                            ! Get attributes directly from parameter node
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = &
                                intent_type_to_string(param_node%intent_type)
                            param_map(i)%is_optional = param_node%is_optional
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code // generate_grouped_body_with_params(arena, &
                                    node%body_indices, 1, param_map, node)
            end if
        end block

        ! End function
        code = code // "end function " // node%name
    end function generate_code_function_def

    ! Generate code for subroutine definitions
    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: params_code
        integer :: i

        ! Start subroutine definition
        code = "subroutine " // node%name

        ! Generate parameters (names only)
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code // "("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code // ", "
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (p => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            code = code // p%name
                        type is (parameter_declaration_node)
                            code = code // p%name
                        type is (declaration_node)
                            code = code // p%var_name
                        class default
                            code = code // "param"//trim(adjustl(int_to_string(i)))
                        end select
                    end if
                end if
            end do
            code = code // ")"
        else
            code = code // "()"
        end if
        code = code // new_line('A')
        
        ! Build parameter map by matching parameter names to body declarations  
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, j
            
            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)
            
            allocate(param_map(param_count))
            
            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.
                
                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        type is (parameter_declaration_node)
                            ! Get attributes directly from parameter node
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = &
                                intent_type_to_string(param_node%intent_type)
                            param_map(i)%is_optional = param_node%is_optional
                        end select
                    end if
                end if
            end do
            
            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if
            
            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code // generate_grouped_body_with_params(arena, &
                                    node%body_indices, 1, param_map, node)
            end if
        end block

        ! End subroutine
        code = code // "end subroutine " // node%name
    end function generate_code_subroutine_def

    ! Generate code for declarations
    function generate_code_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: init_code, type_str
        integer :: i, j
        logical :: standardize_types_enabled

        ! Get type standardization setting
        call get_type_standardization(standardize_types_enabled)

        ! Determine the type string
        if (len_trim(node%type_name) > 0) then
            type_str = node%type_name
        else if (node%inferred_type%kind > 0) then
            ! Handle type inference
            select case (node%inferred_type%kind)
            case (TINT)
                type_str = "integer"
            case (TREAL)
                if (standardize_types_enabled) then
                    type_str = "real(8)"
                else
                    type_str = "real"
                end if
            case (TCHAR)
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    type_str = "character(len=:)"
                else if (node%inferred_type%size > 0) then
                    type_str = "character(len=" // &
                        trim(adjustl(int_to_string(node%inferred_type%size))) // ")"
                else
                    ! For zero-length or unknown strings, use explicit length 0
                    ! character(*) is only valid in parameter declarations
                    type_str = "character(len=0)"
                end if
            case (TLOGICAL)
                type_str = "logical"
            case (TCOMPLEX)
                type_str = "complex"
            case (TDOUBLE)
                type_str = "double precision"
            case (TDERIVED)
                ! For derived types, use the type name from the node
                if (len_trim(node%type_name) > 0) then
                    type_str = node%type_name
                else
                    type_str = "type(unknown_t)"  ! Fallback for derived types
                end if
            case default
                type_str = "real"  ! Default to real
            end select
        else
            type_str = "real"  ! Default fallback
        end if

        ! Generate basic declaration
        code = type_str

        ! Add kind if present (but not for character which uses len)
        if (node%has_kind .and. node%type_name /= "character") then
            code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) // ")"
        else if (node%type_name == "character" .and. node%has_kind) then
            ! For character, kind_value is actually the length
            code = "character(len=" // trim(adjustl(int_to_string(node%kind_value))) // ")"
        end if

        ! Add intent if present
        if (node%has_intent .and. allocated(node%intent)) then
            code = code // ", intent(" // node%intent // ")"
        end if

        ! Add allocatable if present or if string needs allocatable
        if (node%is_allocatable) then
            code = code // ", allocatable"
        else if (node%inferred_type%kind > 0) then
            if (node%inferred_type%alloc_info%needs_allocatable_string) then
                code = code // ", allocatable"
            end if
        end if
        
        ! Add optional if present
        if (node%is_optional) then
            code = code // ", optional"
        end if
        
        ! Add pointer if present
        if (node%is_pointer) then
            code = code // ", pointer"
        end if
        
        ! Add target if present
        if (node%is_target) then
            code = code // ", target"
        end if
        
        ! Add parameter if present
        if (node%is_parameter) then
            code = code // ", parameter"
        end if

        ! Add variable names - handle both single and multi declarations
        code = code // " :: "
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            ! Multi-variable declaration
            do i = 1, size(node%var_names)
                if (i > 1) code = code // ", "
                code = code // trim(node%var_names(i))
                ! Add dimensions per variable if needed
                if (node%is_array .and. allocated(node%dimension_indices)) then
                    code = code // "("
                    do j = 1, size(node%dimension_indices)
                        if (j > 1) code = code // ","
                        if (node%dimension_indices(j) > 0 .and. &
                            node%dimension_indices(j) <= arena%size) then
                            code = code // generate_code_from_arena(arena, &
                                                                 node%dimension_indices(j))
                        else
                            code = code // ":"  ! Default for unspecified dimensions
                        end if
                    end do
                    code = code // ")"
                end if
            end do
        else
            ! Single variable declaration
            code = code // node%var_name
            
            ! Add array dimensions if present
            if (node%is_array .and. allocated(node%dimension_indices)) then
                ! Generate dimension expressions
                code = code // "("
                do i = 1, size(node%dimension_indices)
                    if (i > 1) code = code // ","
                    if (node%dimension_indices(i) > 0 .and. &
                        node%dimension_indices(i) <= arena%size) then
                        code = code // generate_code_from_arena(arena, node%dimension_indices(i))
                    else
                        code = code // ":"  ! Default for unspecified dimensions
                    end if
                end do
                code = code // ")"
            end if
        end if

        ! Add initializer if present
        if (node%initializer_index > 0 .and. node%initializer_index <= arena%size) then
            init_code = generate_code_from_arena(arena, node%initializer_index)
            code = code // " = " // init_code
        end if
    end function generate_code_declaration

    ! Generate code for parameter declarations
    function generate_code_parameter_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        ! For parameter_declaration_node, emit only the name. Attributes are handled
        ! by separate declaration nodes in the body and should not appear in signatures.
        code = node%name
    end function generate_code_parameter_declaration

    ! Generate code for modules
    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i
        logical :: has_implicit

        ! Module header
        code = "module " // node%name // new_line('A')

        ! Ensure module includes implicit none (quality requirement for lazy Fortran)
        has_implicit = .false.
        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (node%declaration_indices(i) > 0 .and. node%declaration_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%declaration_indices(i))%node)) then
                        select type (decl => arena%entries(node%declaration_indices(i))%node)
                        type is (implicit_statement_node)
                            if (decl%is_none) then
                                has_implicit = .true.
                                exit
                            end if
                        type is (literal_node)
                            if (allocated(decl%value)) then
                                if (index(decl%value, 'implicit none') > 0) then
                                    has_implicit = .true.
                                    exit
                                end if
                            end if
                        end select
                    end if
                end if
            end do
        end if
        if (.not. has_implicit) then
            code = code // "    implicit none" // new_line('A')
        end if

        ! Generate module declarations
        if (allocated(node%declaration_indices)) then
            body_code = generate_grouped_body(arena, node%declaration_indices, 1)
            if (len(body_code) > 0) then
                code = code // body_code
            end if
        end if

        ! Check for contains section
        if (node%has_contains .and. allocated(node%procedure_indices)) then
            code = code // "contains" // new_line('A')
            
            ! Generate contained procedures
            do i = 1, size(node%procedure_indices)
                if (node%procedure_indices(i) > 0 .and. &
                    node%procedure_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, node%procedure_indices(i))
                    if (len(body_code) > 0) then
                        ! Add proper indentation for contained procedures
                        code = code // "    " // body_code
                        if (i < size(node%procedure_indices)) then
                            code = code // new_line('A') // new_line('A')
                        else
                            code = code // new_line('A')
                        end if
                    end if
                end if
            end do
        end if

        ! Module end
        code = code // "end module " // node%name
    end function generate_code_module

    ! Generate code for derived types
    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: component_code
        integer :: i

        ! Type definition header
        code = "type :: " // node%name // new_line('A')

        ! Generate components
        if (allocated(node%component_indices)) then
            do i = 1, size(node%component_indices)
                if (node%component_indices(i) > 0 .and. &
                    node%component_indices(i) <= arena%size) then
                    component_code = generate_code_from_arena(arena, node%component_indices(i))
                    code = code // "    " // component_code // new_line('A')
                end if
            end do
        end if

        ! Type definition end
        code = code // "end type " // node%name
    end function generate_code_derived_type

    ! Generate code for program nodes
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i, j
        logical :: in_contains_section
        logical :: found_contains
        logical :: has_non_trivial_body
        logical :: context_has_executable_before_contains

        context_has_executable_before_contains = .false.

        ! Check if there's a non-trivial body before contains
        has_non_trivial_body = .false.
        found_contains = .false.
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%body_indices(i))%node)) then
                        select type (body_node => arena%entries(node%body_indices(i))%node)
                        type is (contains_node)
                            found_contains = .true.
                            exit
                        type is (comment_node)
                            ! Comments don't count as non-trivial
                        type is (blank_line_node)
                            ! Blank lines don't count as non-trivial
                        class default
                            has_non_trivial_body = .true.
                        end select
                    end if
                end if
            end do
        end if

        context_has_executable_before_contains = has_non_trivial_body .and. found_contains

        ! Handle special multi-unit container
        if (node%name == "__MULTI_UNIT__") then
            ! Generate code for each unit as siblings without program wrapper
            code = ""
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            select type (child => arena%entries(node%body_indices(i))%node)
                            type is (program_node)
                                ! Skip empty implicit main programs to avoid duplicate minimal outputs
                                if ((child%name == "main" .or. child%name == "__IMPLICIT_MAIN__") .and. &
                                    (.not. allocated(child%body_indices) .or. size(child%body_indices) == 0)) cycle
                            type is (subroutine_def_node)
                                ! Skip duplicate empty subroutines (defensive check)
                                if (.not. allocated(child%body_indices) .or. size(child%body_indices) == 0) then
                                    if (.not. allocated(child%param_indices) .or. size(child%param_indices) == 0) then
                                        ! Check if this is a duplicate of a previous subroutine
                                        block
                                            integer :: j
                                            logical :: is_duplicate
                                            is_duplicate = .false.
                                            do j = 1, i-1
                                                if (node%body_indices(j) > 0 .and. node%body_indices(j) <= arena%size) then
                                                    if (allocated(arena%entries(node%body_indices(j))%node)) then
                                                        select type (prev => arena%entries(node%body_indices(j))%node)
                                                        type is (subroutine_def_node)
                                                            if (prev%name == child%name) then
                                                                is_duplicate = .true.
                                                                exit
                                                            end if
                                                        end select
                                                    end if
                                                end if
                                            end do
                                            if (is_duplicate) cycle
                                        end block
                                    end if
                                end if
                            end select
                        end if
                        if (len(code) > 0) then
                            code = code // new_line('A') // new_line('A')
                        end if
                        code = code // generate_code_from_arena(arena, node%body_indices(i))
                    end if
                end do
            end if
            return
        end if

        ! Program header
        code = "program " // node%name // new_line('A')

        ! Add implicit none only if not already present in body
        block
            logical :: has_implicit
            has_implicit = .false.
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            select type (ib => arena%entries(node%body_indices(i))%node)
                            ! Detect either a proper implicit statement node marked as none,
                            ! or a literal node that already contains the text 'implicit none'.
                            type is (implicit_statement_node)
                                if (ib%is_none) has_implicit = .true.
                            type is (literal_node)
                                if (allocated(ib%value)) then
                                    if (index(ib%value, 'implicit none') > 0) has_implicit = .true.
                                end if
                            end select
                        end if
                    end if
                end do
            end if
            if (.not. has_implicit) then
                code = code // "    implicit none" // new_line('A')
            end if
        end block

        ! Generate body with proper grouping
        if (allocated(node%body_indices)) then
            body_code = generate_grouped_body_with_context(arena, node%body_indices, 1, &
                                                          context_has_executable_before_contains)
            if (len(body_code) > 0) then
                code = code // body_code
            end if
        end if

        ! Program end
        code = code // "end program " // node%name
    end function generate_code_program


    ! Generate grouped body with context
    function generate_grouped_body_with_context(arena, body_indices, indent, has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code
        
        ! Pass context to utilities module
        code = generate_grouped_body_context(arena, body_indices, indent, has_exec_before_contains)
    end function generate_grouped_body_with_context

end module codegen_declarations
