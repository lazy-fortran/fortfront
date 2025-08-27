module standardizer_declarations
    ! Variable declaration generation module
    ! Handles implicit none insertion, variable declaration creation, and type analysis
    
    use ast_core
    use ast_factory
    use type_system_unified
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result
    use standardizer_types
    implicit none
    private

    ! Type standardization configuration (local copy)
    logical, save :: standardizer_type_standardization_enabled = .true.

    public :: insert_variable_declarations
    public :: has_implicit_none
    public :: program_has_variable_declarations
    public :: find_declaration_insertion_point
    public :: generate_and_insert_declarations
    public :: has_explicit_declaration
    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: collect_identifier_var
    public :: collect_identifier_var_with_type
    public :: add_variable
    public :: mark_variable_declared
    public :: standardize_declarations

contains

    ! Local implementation of get_standardizer_type_standardization
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    ! Insert variable declarations and implicit none for a program
    subroutine insert_variable_declarations(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index
        integer, allocatable :: declaration_indices(:)
        integer :: i, j, insert_pos, n_declarations

        if (.not. allocated(prog%body_indices)) return

        ! Find insertion point (after use statements, before executable statements)
        insert_pos = find_declaration_insertion_point(arena, prog)
        if (insert_pos == 0) insert_pos = 1  ! Default to beginning if no use statements

        ! Check if implicit none already exists
        if (.not. has_implicit_none(arena, prog)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                         line=1, column=1, parent_index=prog_index)
        else
            implicit_none_index = 0  ! Don't add duplicate
        end if

        ! Check if program already has variable declarations (is already standardized)
        if (program_has_variable_declarations(arena, prog)) then
            ! Program already standardized, don't add more declarations
            allocate(declaration_indices(0))
        else
            ! Collect and generate variable declarations
            call generate_and_insert_declarations(arena, prog, prog_index, declaration_indices)
        end if
        n_declarations = 0
        if (allocated(declaration_indices)) n_declarations = size(declaration_indices)

        ! Create new body indices with optional implicit none and declarations
        if (implicit_none_index > 0) then
            allocate (new_body_indices(size(prog%body_indices) + 1 + n_declarations))
        else
            allocate (new_body_indices(size(prog%body_indices) + n_declarations))
        end if

        ! Copy use statements
        j = 1
        do i = 1, insert_pos - 1
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Insert implicit none if we created one
        if (implicit_none_index > 0) then
            new_body_indices(j) = implicit_none_index
            j = j + 1
        end if

        ! Insert declarations
        do i = 1, n_declarations
            new_body_indices(j) = declaration_indices(i)
            j = j + 1
        end do

        ! Copy remaining statements
        do i = insert_pos, size(prog%body_indices)
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Update program body
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    end subroutine insert_variable_declarations

    ! Check if a program already has implicit none
    function has_implicit_none(arena, prog) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: found
        integer :: i
        
        found = .false.
        if (.not. allocated(prog%body_indices)) return
        
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (literal_node)
                        if (stmt%literal_kind == LITERAL_STRING .and. &
                            index(stmt%value, "implicit none") > 0) then
                            found = .true.
                            return
                        end if
                    type is (implicit_statement_node)
                        if (stmt%is_none) then
                            found = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function has_implicit_none

    ! Check if program already has variable declarations (indicating it's been standardized)
    function program_has_variable_declarations(arena, prog) result(has_decls)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: has_decls
        integer :: i
        
        has_decls = .false.
        if (.not. allocated(prog%body_indices)) return
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        ! Found a variable declaration - program is already standardized
                        has_decls = .true.
                        return
                    type is (parameter_declaration_node)
                        ! Found a parameter declaration - also indicates standardization
                        has_decls = .true.
                        return
                    end select
                end if
            end if
        end do
    end function program_has_variable_declarations

    ! Find where to insert declarations (after use statements)
    function find_declaration_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos
        integer :: i

        pos = 1  ! Default to beginning
        if (.not. allocated(prog%body_indices)) return

        ! Find the last use statement
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (use_statement_node)
                        pos = i + 1  ! Insert after this use statement
                    class default
                        ! First non-use statement, stop looking
                        exit
                    end select
                end if
            end if
        end do

    end function find_declaration_insertion_point

    ! Standardize existing declarations (e.g., real -> real(8))
    subroutine standardize_declarations(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i
        logical :: standardizer_type_standardization_enabled

        if (.not. allocated(prog%body_indices)) return
        
        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        ! Standardize the type name (if enabled)
                        if (stmt%type_name == "real" .and. &
                            standardizer_type_standardization_enabled) then
                            stmt%type_name = "real"
                            stmt%has_kind = .true.
                            stmt%kind_value = 8
                        end if
                        ! Update the node in the arena
                        arena%entries(prog%body_indices(i))%node = stmt
                    end select
                end if
            end if
        end do
    end subroutine standardize_declarations

    ! Generate and insert variable declarations from inferred types
    subroutine generate_and_insert_declarations(arena, prog, prog_index, &
                                                 declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable, intent(out) :: declaration_indices(:)
        character(len=64), allocatable :: var_names(:)
        character(len=64), allocatable :: var_types(:)
        logical, allocatable :: var_declared(:)
        character(len=64), allocatable :: function_names(:)
        integer :: i, var_count, func_count
        type(declaration_node) :: decl_node

        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        allocate (function_names(100))
        var_declared = .false.
        var_count = 0
        func_count = 0

        ! First pass: collect function names
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (function_def_node)
                            if (func_count < size(function_names)) then
                                func_count = func_count + 1
                                function_names(func_count) = stmt%name
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Collect all variables that need declarations
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
             if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        call collect_statement_vars(arena, prog%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                    function_names, func_count)
                    end if
                end if
            end do
        end if

        ! Create declaration nodes
        if (var_count > 0) then
            call create_declaration_nodes(arena, prog, prog_index, var_names, var_types, &
                                        var_declared, var_count, declaration_indices)
        else
            allocate (declaration_indices(0))
        end if

    end subroutine generate_and_insert_declarations

    ! Create declaration nodes from collected variables
    subroutine create_declaration_nodes(arena, prog, prog_index, var_names, var_types, &
                                      var_declared, var_count, declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        character(len=64), intent(in) :: var_names(:)
        character(len=64), intent(in) :: var_types(:)
        logical, intent(in) :: var_declared(:)
        integer, intent(in) :: var_count
        integer, allocatable, intent(out) :: declaration_indices(:)
        type(declaration_node) :: decl_node
        integer :: i, decl_idx, actual_count

        ! First count how many declarations we'll actually create
        actual_count = 0
        do i = 1, var_count
            if (var_declared(i)) then
                ! Check if this variable already has an explicit declaration
                if (.not. has_explicit_declaration(arena, prog, var_names(i))) then
                    actual_count = actual_count + 1
                end if
            end if
        end do
        
        if (actual_count == 0) then
            allocate (declaration_indices(0))
            return
        end if
        
        allocate (declaration_indices(actual_count))
        
        ! Now create the declaration nodes
        decl_idx = 0
        do i = 1, var_count
            if (var_declared(i)) then
                ! Check if this variable already has an explicit declaration
                if (.not. has_explicit_declaration(arena, prog, var_names(i))) then
                    decl_idx = decl_idx + 1
                    call create_single_declaration(arena, prog_index, var_names(i), &
                                                 var_types(i), decl_node)
                    call arena%push(decl_node, "declaration", prog_index)
                    declaration_indices(decl_idx) = arena%size
                end if
            end if
        end do

    end subroutine create_declaration_nodes

    ! Create a single declaration node
    subroutine create_single_declaration(arena, prog_index, var_name, var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(out) :: decl_node
        integer :: comma_pos, dim_pos, paren_pos, iostat
        logical :: has_dimension_attr
        character(len=20) :: dim_str
        integer :: dim_size

        ! Initialize declaration node
        decl_node%var_name = trim(var_name)
        decl_node%has_kind = .false.
        decl_node%initializer_index = 0
        decl_node%line = 1
        decl_node%column = 1
        decl_node%is_array = .false.
        decl_node%is_allocatable = .false.
        
        ! Parse the type string - it might contain dimension info
        has_dimension_attr = .false.
        comma_pos = index(var_type, ',')
        if (comma_pos > 0) then
            ! Has attributes like dimension - extract just the base type
            decl_node%type_name = trim(var_type(1:comma_pos-1))
            
            ! Check for dimension attribute
            dim_pos = index(var_type, 'dimension(')
            if (dim_pos > 0) then
                has_dimension_attr = .true.
                call parse_dimension_attribute(arena, prog_index, var_type, dim_pos, &
                                             decl_node)
            end if
            
            ! Check for explicit allocatable attribute
            if (index(var_type, 'allocatable') > 0) then
                decl_node%is_allocatable = .true.
            end if
        else
            decl_node%type_name = trim(var_type)
        end if
        
        ! Set array properties based on inferred type if not already set
        if (.not. has_dimension_attr) then
            call set_array_properties_from_type(arena, var_name, prog_index, decl_node)
        end if
        
        ! If array with deferred shape, mark as allocatable
        if (decl_node%is_array .and. allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                if (decl_node%dimension_indices(1) == 0) then
                    decl_node%is_allocatable = .true.
                end if
            end if
        end if

    end subroutine create_single_declaration

    ! Parse dimension attribute from type string
    subroutine parse_dimension_attribute(arena, prog_index, var_type, dim_pos, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, dim_pos
        character(len=*), intent(in) :: var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: paren_pos, iostat, dim_size
        character(len=20) :: dim_str
        type(literal_node) :: size_literal
        character(len=20) :: size_str

        ! Extract dimension value
        paren_pos = index(var_type(dim_pos:), ')')
        if (paren_pos > 10) then  ! Must have at least 1 character after dimension(
            dim_str = var_type(dim_pos+10:dim_pos+paren_pos-2)
            
            ! Check if it's a deferred shape (:) - indicates allocatable
            if (trim(dim_str) == ':') then
                decl_node%is_array = .true.
                decl_node%is_allocatable = .true.
                if (allocated(decl_node%dimension_indices)) &
                    deallocate(decl_node%dimension_indices)
                allocate(decl_node%dimension_indices(1))
                decl_node%dimension_indices(1) = 0  ! 0 indicates deferred shape
            else
                ! Try to parse as integer
                read(dim_str, *, iostat=iostat) dim_size
                if (iostat == 0) then
                    ! Successfully parsed dimension
                    decl_node%is_array = .true.
                    if (allocated(decl_node%dimension_indices)) &
                        deallocate(decl_node%dimension_indices)
                    allocate(decl_node%dimension_indices(1))
                    ! Create literal node for the size
                    write(size_str, '(i0)') dim_size
                    size_literal = create_literal(trim(size_str), LITERAL_INTEGER, 1, 1)
                    call arena%push(size_literal, "literal", prog_index)
                    decl_node%dimension_indices(1) = arena%size
                end if
            end if
        end if

    end subroutine parse_dimension_attribute

    ! Set array properties from inferred type information
    subroutine set_array_properties_from_type(arena, var_name, prog_index, decl_node)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: prog_index
        type(declaration_node), intent(inout) :: decl_node
        integer :: j
        type(literal_node) :: size_literal
        character(len=20) :: size_str

        ! Search for the identifier node with this name to check its inferred type
        do j = 1, arena%size
            if (allocated(arena%entries(j)%node)) then
                select type (node => arena%entries(j)%node)
                type is (identifier_node)
                    if (trim(node%name) == trim(var_name)) then
                        if (node%inferred_type%kind > 0) then
                            if (node%inferred_type%kind == TARRAY) then
                                decl_node%is_array = .true.
                                ! Use fixed-size array if size is known
                                if (allocated(decl_node%dimension_indices)) &
                                    deallocate(decl_node%dimension_indices)
                                allocate(decl_node%dimension_indices(1))
                                if (node%inferred_type%size > 0 .and. &
                                    .not. node%inferred_type%alloc_info%is_allocatable) then
                                    ! Create literal node for the size
                                    write(size_str, '(i0)') node%inferred_type%size
                                    size_literal = create_literal(trim(size_str), &
                                                                 LITERAL_INTEGER, 1, 1)
                                    ! Note: can't modify arena in this context, so defer
                                    decl_node%dimension_indices(1) = node%inferred_type%size
                                else
                                    ! Allocatable dimension
                                    decl_node%dimension_indices(1) = 0
                                end if
                                exit
                            end if
                        end if
                    end if
                end select
            end if
        end do

    end subroutine set_array_properties_from_type

    ! The remaining procedures would be implemented here with similar patterns
    ! For brevity, I'll include stubs for the remaining interface

    ! Check if variable has explicit declaration
    function has_explicit_declaration(arena, prog, var_name) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=*), intent(in) :: var_name
        logical :: has_decl
        integer :: i, j
        
        has_decl = .false.
        
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check single variable declaration
                            if (trim(stmt%var_name) == trim(var_name)) then
                                has_decl = .true.
                                return
                            end if
                            ! Check multi-variable declaration
                            if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                                do j = 1, size(stmt%var_names)
                                    if (trim(stmt%var_names(j)) == trim(var_name)) then
                                        has_decl = .true.
                                        return
                                    end if
                                end do
                            end if
                        end select
                    end if
                end if
            end do
        end if
    end function has_explicit_declaration

    ! Collect variables from statement
    recursive subroutine collect_statement_vars(arena, stmt_index, var_names, &
                                                var_types, var_declared, var_count, &
                                                function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i

        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return

        select type (stmt => arena%entries(stmt_index)%node)
        type is (declaration_node)
            ! Mark variables as already declared - don't generate implicit declarations
            if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                ! Handle multi-variable declaration
                do i = 1, size(stmt%var_names)
                    call mark_variable_declared(stmt%var_names(i), var_names, &
                                               var_declared, var_count)
                end do
            else
                ! Handle single variable declaration
                call mark_variable_declared(stmt%var_name, var_names, &
                                           var_declared, var_count)
            end if
        type is (assignment_node)
            call collect_assignment_vars(arena, stmt_index, var_names, &
                                          var_types, var_declared, var_count, &
                                         function_names, func_count)
        type is (do_loop_node)
            ! Collect loop variable
            call add_variable(stmt%var_name, "integer", var_names, var_types, &
                              var_declared, var_count, &
                              function_names, func_count)
            ! Collect variables from body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call collect_statement_vars(arena, stmt%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (do_while_node)
            ! Collect variables from body
            if (allocated(stmt%body_indices)) then
                do i = 1, size(stmt%body_indices)
                    call collect_statement_vars(arena, stmt%body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (if_node)
            ! Collect variables from then and else branches
            if (allocated(stmt%then_body_indices)) then
                do i = 1, size(stmt%then_body_indices)
                    call collect_statement_vars(arena, stmt%then_body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
            if (allocated(stmt%else_body_indices)) then
                do i = 1, size(stmt%else_body_indices)
                    call collect_statement_vars(arena, stmt%else_body_indices(i), &
                                        var_names, var_types, var_declared, var_count, &
                                                function_names, func_count)
                end do
            end if
        type is (select_case_node)
            ! TODO: Handle select case when implemented
        end select
    end subroutine collect_statement_vars

    ! Collect variables from assignment
    subroutine collect_assignment_vars(arena, assign_index, var_names, &
                                        var_types, var_declared, var_count, &
                                       function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        type(mono_type_t), pointer :: value_type
        character(len=64) :: var_type

        if (assign_index <= 0 .or. assign_index > arena%size) return
        if (.not. allocated(arena%entries(assign_index)%node)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            ! Get target node
            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
                        ! Try to get type from the value expression
                        var_type = ""  ! Empty default indicates type not determined
                        
                        if (assign%value_index > 0 .and. &
                            assign%value_index <= arena%size) then
                            if (allocated(arena%entries(assign%value_index)%node)) then
                                ! Check if it's an array expression by structure
                                if (is_array_expression(arena, assign%value_index)) then
                                    ! Try to determine array size if possible
                                    var_type = &
                                        get_array_var_type(arena, assign%value_index)
                                else
                                    value_type => &
                                        get_expression_type(arena, assign%value_index)
                                    if (associated(value_type)) then
                                        block
                                            type(string_result_t) :: type_result
                                            type_result = get_fortran_type_string(value_type)
                                            if (type_result%is_success()) then
                                                var_type = type_result%get_value()
                                            end if
                                        end block
                                    end if
                                end if
                            end if
                        end if
                        
                        ! Now collect the variable with the determined type
                        call collect_identifier_var_with_type(target, var_type, &
                            var_names, var_types, var_declared, var_count, &
                            function_names, func_count)
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

    ! Collect identifier variable
    subroutine collect_identifier_var(identifier, var_names, var_types, &
                                       var_declared, var_count, &
                                      function_names, func_count)
        type(identifier_node), intent(in) :: identifier
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this identifier is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(identifier%name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(identifier%name)) then
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = identifier%name

                ! Determine type from inferred_type if available
                if (identifier%inferred_type%kind > 0) then
                    block
                        type(string_result_t) :: type_result
                        type_result = get_fortran_type_string(identifier%inferred_type)
                        if (type_result%is_success()) then
                            var_types(var_count) = type_result%get_value()
                            var_declared(var_count) = .true.
                        else
                            ! Skip variables with unknown types instead of defaulting
                            var_count = var_count - 1
                        end if
                    end block
                else
                    ! Skip variables with no type information instead of defaulting
                    var_count = var_count - 1
                end if
            end if
        end if
    end subroutine collect_identifier_var

    ! Collect identifier variable with type
    subroutine collect_identifier_var_with_type(identifier, var_type, &
        var_names, var_types, var_declared, var_count, &
        function_names, func_count)
        type(identifier_node), intent(in) :: identifier
        character(len=*), intent(in) :: var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this identifier is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(identifier%name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(identifier%name)) then
                found = .true.
                ! Update type if it's an array and wasn't before
                if (index(var_type, "(") > 0 .and. index(var_types(i), "(") == 0) then
                    var_types(i) = var_type
                end if
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = identifier%name
                var_types(var_count) = var_type
                var_declared(var_count) = .true.
            end if
        end if
    end subroutine collect_identifier_var_with_type

    ! Add variable to collections
    subroutine add_variable(var_name, var_type, var_names, var_types, &
                              var_declared, var_count, &
                              function_names, func_count)
        character(len=*), intent(in) :: var_name, var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(var_name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = var_name
                var_types(var_count) = var_type
                var_declared(var_count) = .true.
            end if
        end if
    end subroutine add_variable

    ! Mark variable as declared
    subroutine mark_variable_declared(var_name, var_names, var_declared, var_count)
        character(len=*), intent(in) :: var_name
        character(len=64), intent(in) :: var_names(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(in) :: var_count
        integer :: i
        
        ! Find the variable if it exists and mark it as declared
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                var_declared(i) = .false.  ! Mark as already declared - don't generate implicit declaration
                return
            end if
        end do
    end subroutine mark_variable_declared

end module standardizer_declarations