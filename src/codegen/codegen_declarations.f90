module codegen_declarations
    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        derived_type_node, intent_type_to_string, module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_core, only: program_node, identifier_node, literal_node, assignment_node, &
        array_literal_node
    use ast_nodes_misc, only: implicit_statement_node, contains_node, comment_node, blank_line_node, &
        use_statement_node
    use ast_nodes_loops, only: do_loop_node
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

        ! Start function definition with optional recursive keyword and return type
        code = ""
        if (node%is_recursive) then
            code = "recursive "
        end if

        if (allocated(node%return_type) .and. len_trim(node%return_type) > 0) then
            code = code // trim(node%return_type) // " function " // node%name
        else
            code = code // "function " // node%name
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

        ! Add result clause if present (but NOT if result name equals function name)
        if (allocated(node%result_variable) .and. len_trim(node%result_variable) > 0) then
            ! Don't add result() clause if result variable name equals function name
            ! (Fortran doesn't allow result(foo) for function foo - just use typed function signature)
            if (.not. (allocated(node%name) .and. trim(node%result_variable) == trim(node%name))) then
                code = code // " result(" // node%result_variable // ")"
            end if
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

        ! Add kind if present and valid (>0) (but not for character which uses len)
        if (node%has_kind .and. node%kind_value > 0 .and. node%type_name /= "character") then
            code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) // ")"
        else if (node%type_name == "character" .and. node%has_kind .and. node%kind_value > 0) then
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

        if (node%is_external) then
            code = code // ", external"
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
                        ! Valid arena index
                        code = code // generate_code_from_arena(arena, node%dimension_indices(i))
                    else if (node%dimension_indices(i) > arena%size) then
                        ! Direct integer value (for inferred dimensions)
                        code = code // int_to_string(node%dimension_indices(i))
                    else
                        code = code // ":"  ! Default for unspecified dimensions (allocatable)
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
        character(len=:), allocatable :: intent_str
        integer :: j
        
        ! Check if this node has a parent that needs just the name (parameter list)
        ! vs full declaration (in body). For now, generate full declaration when
        ! the node has type and attributes.
        if (len_trim(node%type_name) > 0) then
            ! Generate full declaration (when in body)
            code = node%type_name
            
            if (node%has_kind) then
                code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) // ")"
            end if
            
            ! Add intent attribute
            intent_str = intent_type_to_string(node%intent_type)
            if (len_trim(intent_str) > 0) then
                code = code // ", intent(" // intent_str // ")"
            end if
            
            ! Add optional attribute
            if (node%is_optional) then
                code = code // ", optional"
            end if
            
            code = code // " :: " // node%name
            
            ! Add dimensions if present
            if (allocated(node%dimension_indices) .and. size(node%dimension_indices) > 0) then
                code = code // "("
                do j = 1, size(node%dimension_indices)
                    if (j > 1) code = code // ", "
                    code = code // generate_code_from_arena(arena, node%dimension_indices(j))
                end do
                code = code // ")"
            end if
        else
            ! Just emit the name (when in parameter list)
            code = node%name
        end if
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
        integer, allocatable :: non_use_indices(:)
        integer :: non_use_count

        context_has_executable_before_contains = .false.
        non_use_count = 0

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
                                ! Skip trivial implicit main wrappers that only contain comments/blank lines
                                if (program_is_trivial_wrapper(arena, node%body_indices(i), child%name)) then
                                    block
                                        character(len=:), allocatable :: trivia_code
                                        trivia_code = collect_trivial_program_trivia(arena, node%body_indices(i))
                                        if (len_trim(trivia_code) > 0) then
                                            if (len(code) > 0) code = code // new_line('A') // new_line('A')
                                            code = code // trivia_code
                                        end if
                                    end block
                                    cycle
                                end if
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

        ! Process use statements first, then add implicit none, then rest of body
        block
            logical :: has_implicit
            logical :: is_use_stmt
            character(len=:), allocatable :: use_statements_code
            character(len=:), allocatable :: loop_var_declarations
            
            has_implicit = .false.
            use_statements_code = ""
            loop_var_declarations = ""
            
            ! First pass: collect use statements and check for implicit none
            if (allocated(node%body_indices)) then
                allocate(non_use_indices(size(node%body_indices)))
                non_use_count = 0
                
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            is_use_stmt = .false.
                            
                            select type (ib => arena%entries(node%body_indices(i))%node)
                            type is (use_statement_node)
                                ! Generate use statement code
                                is_use_stmt = .true.
                                use_statements_code = use_statements_code // "    " // &
                                    generate_code_from_arena(arena, node%body_indices(i)) // new_line('A')
                                
                            type is (implicit_statement_node)
                                if (ib%is_none) has_implicit = .true.
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)
                                
                            type is (literal_node)
                                if (allocated(ib%value)) then
                                    if (index(ib%value, 'implicit none') > 0) has_implicit = .true.
                                end if
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)
                                
                            class default
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)
                            end select
                            
                            ! Don't add use statements to non_use_indices
                            if (is_use_stmt) then
                                ! Use statement already processed, skip
                            end if
                        end if
                    end if
                end do
            end if
            
            ! Add use statements first
            if (len(use_statements_code) > 0) then
                code = code // use_statements_code
            end if
            
            ! Then add implicit none if not present
            if (.not. has_implicit) then
                code = code // "    implicit none" // new_line('A')
            end if
        end block
        
        ! Generate rest of body (non-use statements) with proper grouping
        if (allocated(node%body_indices) .and. non_use_count > 0) then
            body_code = generate_grouped_body_with_context(arena, non_use_indices(1:non_use_count), 1, &
                                                          context_has_executable_before_contains)
            
            ! Check if body contains implied do loops and add loop variables after implicit none
            if (len(body_code) > 0) then
                block
                    integer :: pos, start_pos, end_pos, impl_pos, insert_pos
                    character(len=:), allocatable :: before_code, after_code, var_name
                    character(len=:), allocatable :: loop_vars(:)
                    integer :: n_vars, i, j
                    logical :: already_declared
                    
                    ! Find all implied do loop variables
                    allocate(character(len=32) :: loop_vars(20))  ! Support up to 20 loop variables
                    n_vars = 0
                    
                    
                    ! Search for patterns like "(var=" in implied do loops (both old and new syntax)
                    pos = 1
                    do while (pos <= len(body_code))
                        ! Find next occurrence of either "= (/ (" or "= [("
                        start_pos = index(body_code(pos:), "= (/ (")
                        if (start_pos == 0) then
                            ! Try new syntax
                            start_pos = index(body_code(pos:), "= [(")
                            if (start_pos > 0) then
                                start_pos = pos + start_pos - 1
                                ! Find the end with "]" for new syntax
                                end_pos = index(body_code(start_pos:), ")]")
                                if (end_pos > 0) then
                                    end_pos = start_pos + end_pos - 1
                                    ! Extract variables from this implied do section
                                    call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                       loop_vars, n_vars)
                                end if
                                pos = start_pos + 3  ! Move past "= [("
                            else
                                exit  ! No more patterns found
                            end if
                        else
                            start_pos = pos + start_pos - 1
                            ! Find the loop variable patterns for old syntax
                            end_pos = index(body_code(start_pos:), " /)")
                            if (end_pos > 0) then
                                end_pos = start_pos + end_pos - 1
                                ! Extract variables from this implied do section
                                call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                   loop_vars, n_vars)
                            end if
                            pos = start_pos + 6  ! Move past "= (/ ("
                        end if
                    end do
                    
                    ! If we found loop variables, add declarations
                    if (n_vars > 0 .or. (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0)) then
                        ! Check if implicit none is in body_code
                        impl_pos = index(body_code, "implicit none")
                        if (impl_pos > 0) then
                            ! Find the end of the implicit none line
                            insert_pos = impl_pos + 13  ! Length of "implicit none"
                            do while (insert_pos <= len(body_code))
                                if (body_code(insert_pos:insert_pos) == new_line('A')) then
                                    insert_pos = insert_pos + 1
                                    exit
                                end if
                                insert_pos = insert_pos + 1
                            end do
                            
                            ! Build declarations for loop variables
                            before_code = body_code(1:insert_pos-1)
                            after_code = body_code(insert_pos:)
                            
                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    ! Skip if already declared
                                    already_declared = .false.
                                    if (index(body_code, "integer :: " // trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if
                                    
                                    if (.not. already_declared) then
                                        before_code = before_code // "    integer :: " // &
                                                     trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                ! Check for implied do with default i
                                if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0) then
                                        before_code = before_code // "    integer :: i" // new_line('A')
                                    end if
                                end if
                            end if
                            
                            body_code = before_code // after_code
                        else
                            ! No implicit none in body, add to code as before
                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    already_declared = .false.
                                    if (index(body_code, "integer :: " // trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if
                                    if (index(code, "integer :: " // trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if
                                    
                                    if (.not. already_declared) then
                                        code = code // "    integer :: " // trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0 .and. index(code, "integer :: i") == 0) then
                                        code = code // "    integer :: i" // new_line('A')
                                    end if
                                end if
                            end if
                        end if
                    end if
                end block
            end if
                
            code = code // body_code
        end if
        
        if (allocated(non_use_indices)) then
            deallocate(non_use_indices)
        end if

        ! Program end
        code = code // "end program " // node%name
    end function generate_code_program

    logical function program_is_trivial_wrapper(arena, prog_index, name) result(is_trivial)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: name
        integer :: j, child_idx

        is_trivial = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. (trim(name) == 'main' .or. trim(name) == '__IMPLICIT_MAIN__')) return
            if (.not. allocated(prog%body_indices) .or. size(prog%body_indices) == 0) then
                is_trivial = .true.
                return
            end if

            is_trivial = .true.
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    cycle
                type is (blank_line_node)
                    cycle
                type is (implicit_statement_node)
                    if (body%is_none) cycle
                    is_trivial = .false.
                    return
                class default
                    is_trivial = .false.
                    return
                end select
            end do
        class default
            return
        end select
    end function program_is_trivial_wrapper

    function collect_trivial_program_trivia(arena, prog_index) result(trivia_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable :: trivia_code
        integer :: j, child_idx
        character(len=:), allocatable :: snippet

        trivia_code = ""
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                type is (blank_line_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                class default
                    cycle
                end select

                if (len(snippet) > 0) then
                    if (len(trivia_code) > 0) trivia_code = trivia_code // new_line('A')
                    trivia_code = trivia_code // snippet
                end if
            end do
        end select
    end function collect_trivial_program_trivia


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

    ! Helper subroutine to extract loop variables from an implied do section
    subroutine extract_loop_vars_from_section(section, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, eq_pos, comma_pos
        character(len=32) :: var_name
        logical :: already_added
        integer :: i
        
        ! Look for patterns like "i=1," or "j=1," or "k=1,"
        pos = 1
        do while (pos < len_trim(section))
            eq_pos = index(section(pos:), "=")
            if (eq_pos == 0) exit
            eq_pos = pos + eq_pos - 1
            
            ! Look backwards from = to find variable name
            if (eq_pos > 1) then
                ! Find the start of the variable name
                i = eq_pos - 1
                do while (i > 0)
                    if (section(i:i) == ' ' .or. section(i:i) == ',' .or. &
                        section(i:i) == '(') then
                        exit
                    end if
                    i = i - 1
                end do
                
                ! Extract variable name
                var_name = adjustl(trim(section(i+1:eq_pos-1)))
                
                ! Check if it looks like a loop variable (single letter or simple name)
                if (len_trim(var_name) > 0 .and. len_trim(var_name) <= 8) then
                    ! Check if it's a number after =
                    comma_pos = index(section(eq_pos+1:), ",")
                    if (comma_pos > 0) then
                        ! This looks like a loop variable
                        ! Check if already in list
                        already_added = .false.
                        do i = 1, n_vars
                            if (trim(loop_vars(i)) == trim(var_name)) then
                                already_added = .true.
                                exit
                            end if
                        end do
                        
                        if (.not. already_added .and. n_vars < size(loop_vars)) then
                            n_vars = n_vars + 1
                            loop_vars(n_vars) = trim(var_name)
                        end if
                    end if
                end if
            end if
            
            pos = eq_pos + 1
        end do
    end subroutine extract_loop_vars_from_section

end module codegen_declarations
