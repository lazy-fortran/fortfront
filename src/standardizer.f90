module standardizer
    ! AST Standardization Stage - transforms lazy fortran AST to standard Fortran AST
    ! This stage:
    ! 1. Decides program vs module based on content
    ! 2. Inserts 'contains' statements where needed
    ! 3. Handles implicit program wrapping
    ! 4. Transforms dialect-specific constructs to standard Fortran
    ! 5. Generates variable declarations from inferred types

    use ast_core
    use ast_factory
    use type_system_hm
    use json_module, only: json_core, json_value, json_file
    implicit none
    private
    
    ! Type standardization configuration for standardizer
    logical, save :: standardizer_type_standardization_enabled = .true.

    public :: standardize_ast
    public :: standardize_ast_json
    public :: set_standardizer_type_standardization, &
              get_standardizer_type_standardization

contains

    ! Main standardization entry point
    recursive subroutine standardize_ast(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: i

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (node => arena%entries(root_index)%node)
        type is (program_node)
            ! Skip standardization for multi-unit containers
            if (node%name == "__MULTI_UNIT__") then
                ! Standardize each unit individually
                if (allocated(node%body_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%body_indices)
                        if (node%body_indices(j) > 0 .and. &
                            node%body_indices(j) <= arena%size) then
                            call standardize_ast(arena, node%body_indices(j))
                        end if
                    end do
                    end block
                end if
            else
                call standardize_program(arena, node, root_index)
            end if
        type is (function_def_node)
            ! Wrap standalone function in a program
            call wrap_function_in_program(arena, root_index)
        type is (subroutine_def_node)
            ! Wrap standalone subroutine in a program
            call wrap_subroutine_in_program(arena, root_index)
        type is (module_node)
            ! Modules don't need wrapping - standardize their contents
            call standardize_module(arena, node, root_index)
        class default
            ! For other node types, no standardization needed yet
        end select

    end subroutine standardize_ast

    ! Standardize a program node
    subroutine standardize_program(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        logical :: has_functions, has_subroutines, has_use_statements
        logical :: has_executable_statements
        logical :: should_be_module
        integer :: contains_index
        integer, allocatable :: new_body_indices(:)
        integer :: i, n_statements, insert_pos

        ! Analyze the program to determine if it should be a module
        call analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)

        if (should_be_module) then
            ! TODO: Transform to module_node
            ! For now, just handle contains insertion
        end if

        ! Standardize existing declarations (e.g., real -> real(8))
        call standardize_declarations(arena, prog)

        ! Always insert implicit none and variable declarations for programs
        call insert_variable_declarations(arena, prog, prog_index)

        ! Check if we need to insert a contains statement
        if (has_functions .or. has_subroutines) then
            ! Find where to insert contains (before first function/subroutine)
            insert_pos = find_contains_insertion_point(arena, prog)

            if (insert_pos > 0) then
                ! Create new body with contains statement
                call insert_contains_statement(arena, prog, prog_index, insert_pos)
            end if
        end if

        ! Standardize function and subroutine definitions
        call standardize_subprograms(arena, prog)

    end subroutine standardize_program

    ! Standardize a module node
    subroutine standardize_module(arena, mod, mod_index)
        type(ast_arena_t), intent(inout) :: arena
        type(module_node), intent(inout) :: mod
        integer, intent(in) :: mod_index
        integer :: i
        
        ! Standardize declarations in the module
        if (allocated(mod%declaration_indices)) then
            do i = 1, size(mod%declaration_indices)
                if (mod%declaration_indices(i) > 0 .and. &
                    mod%declaration_indices(i) <= arena%size) then
                    call standardize_ast(arena, mod%declaration_indices(i))
                end if
            end do
        end if
        
        ! Standardize procedures in the module
        if (allocated(mod%procedure_indices)) then
            do i = 1, size(mod%procedure_indices)
                if (mod%procedure_indices(i) > 0 .and. &
                    mod%procedure_indices(i) <= arena%size) then
                    call standardize_ast(arena, mod%procedure_indices(i))
                end if
            end do
        end if
    end subroutine standardize_module

    ! Analyze program content to determine its nature
    subroutine analyze_program_content(arena, prog, has_functions, has_subroutines, &
                        has_use_statements, has_executable_statements, should_be_module)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical, intent(out) :: has_functions, has_subroutines, has_use_statements
        logical, intent(out) :: has_executable_statements, should_be_module
        integer :: i

        has_functions = .false.
        has_subroutines = .false.
        has_use_statements = .false.
        has_executable_statements = .false.
        should_be_module = .false.

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    type is (use_statement_node)
                        has_use_statements = .true.
                    type is (assignment_node)
                        has_executable_statements = .true.
                    type is (call_or_subscript_node)
                        has_executable_statements = .true.
                    type is (subroutine_call_node)
                        has_executable_statements = .true.
                    type is (print_statement_node)
                        has_executable_statements = .true.
                    type is (if_node)
                        has_executable_statements = .true.
                    type is (do_loop_node)
                        has_executable_statements = .true.
                    type is (do_while_node)
                        has_executable_statements = .true.
                    type is (select_case_node)
                        has_executable_statements = .true.
                    end select
                end if
            end if
        end do

        ! Decision logic: if only functions/subroutines and use statements, &
        ! it's likely a module
        ! For now, keep it simple - presence of multiple procedures suggests module
        should_be_module = (has_functions .or. has_subroutines) .and. &
                   (count([has_functions, has_subroutines]) > 1 .or. has_use_statements)

    end subroutine analyze_program_content

    ! Find where to insert the contains statement
    function find_contains_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos
        integer :: i

        pos = 0
        if (.not. allocated(prog%body_indices)) return

        ! Find the first function or subroutine
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        pos = i
                        return
                    type is (subroutine_def_node)
                        pos = i
                        return
                    end select
                end if
            end if
        end do

    end function find_contains_insertion_point

    ! Insert a contains statement at the specified position
    subroutine insert_contains_statement(arena, prog, prog_index, insert_pos)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index, insert_pos
        integer, allocatable :: new_body_indices(:)
        integer :: contains_index, i, j
        type(contains_node) :: contains_stmt

        if (.not. allocated(prog%body_indices)) return

        ! Create contains node
        contains_stmt%line = 1  ! Line number will be adjusted later
        contains_stmt%column = 1

        ! Add contains node to arena
        call arena%push(contains_stmt, "contains", prog_index)
        contains_index = arena%size

        ! Create new body indices array with contains inserted
        allocate (new_body_indices(size(prog%body_indices) + 1))

        ! Copy statements before the insertion point
        j = 1
        do i = 1, insert_pos - 1
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Insert contains
        new_body_indices(j) = contains_index
        j = j + 1

        ! Copy remaining statements
        do i = insert_pos, size(prog%body_indices)
            new_body_indices(j) = prog%body_indices(i)
            j = j + 1
        end do

        ! Replace body indices
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    end subroutine insert_contains_statement


    ! JSON interface for standardization
    subroutine standardize_ast_json(json_file_path, output_file, error_msg)
        character(len=*), intent(in) :: json_file_path
        character(len=*), intent(in) :: output_file
        character(len=*), intent(out) :: error_msg
        ! TODO: Implement JSON standardization
        ! type(json_file) :: json
        ! type(json_value), pointer :: root
        ! type(ast_arena_t) :: arena
        ! integer :: root_index
        ! character(len=:), allocatable :: output_json

        error_msg = "JSON to arena conversion not yet implemented for standardizer"
        return

    end subroutine standardize_ast_json

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
            ! First count how many declarations we'll actually create
            block
                integer :: actual_count
                actual_count = 0
                do i = 1, var_count
                    if (var_declared(i)) then
                        ! Check if this variable already has an explicit declaration
                        if (.not. has_explicit_declaration(arena, prog, &
                                                            var_names(i))) then
                            actual_count = actual_count + 1
                        end if
                    end if
                end do
                
                if (actual_count == 0) then
                    allocate (declaration_indices(0))
                    return
                end if
                
                allocate (declaration_indices(actual_count))
            end block
            
            ! Now create the declaration nodes
            block
                integer :: decl_idx
                decl_idx = 0
                do i = 1, var_count
                    if (var_declared(i)) then
                        ! Check if this variable already has an explicit declaration
                        if (.not. has_explicit_declaration(arena, prog, &
                                                            var_names(i))) then
                            decl_idx = decl_idx + 1
                    ! Create declaration node
                    decl_node%type_name = trim(var_types(i))
                    decl_node%var_name = trim(var_names(i))
                    
                    ! Check if this variable is an array by looking it up in the arena
                    block
                        logical :: found_array_type
                        integer :: j
                        found_array_type = .false.
                        
                        ! Search for the identifier node with this name to check &
                        ! its inferred type
                        do j = 1, arena%size
                            if (allocated(arena%entries(j)%node)) then
                                select type (node => arena%entries(j)%node)
                                type is (identifier_node)
                                    if (trim(node%name) == trim(var_names(i))) then
                                        if (allocated(node%inferred_type)) then
                                            if (node%inferred_type%kind == TARRAY) then
                                                found_array_type = .true.
                                                decl_node%is_array = .true.
                                                ! Use fixed-size array if size is known
                                                if (allocated(&
                                                    decl_node%dimension_indices)) &
                                                    deallocate(decl_node%dimension_indices)
                                                allocate(decl_node%dimension_indices(1))
                                                if (node%inferred_type%size > 0) then
                                                    ! Create literal node for the size
                                                    block
                                                        type(literal_node) :: size_literal
                                                        character(len=20) :: size_str
                                                        write(size_str, '(i0)') node%inferred_type%size
                                                        size_literal = &
                                                            create_literal(trim(size_str), &
                                                            LITERAL_INTEGER, 1, 1)
                                                        call arena%push(&
                                                            size_literal, &
                                                            "literal", prog_index)
                                                        decl_node%dimension_indices(1) = arena%size
                                                    end block
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
                        
                        if (.not. found_array_type) then
                            decl_node%is_array = .false.
                        end if
                    end block
                    
                    ! If array with deferred shape, mark as allocatable
                    if (decl_node%is_array .and. &
                        allocated(decl_node%dimension_indices)) then
                        if (size(decl_node%dimension_indices) > 0) then
                            if (decl_node%dimension_indices(1) == 0) then
                                ! This is a deferred shape array, needs allocatable
                                decl_node%is_allocatable = .true.
                            end if
                        end if
                    end if
                    
                    decl_node%has_kind = .false.
                    decl_node%initializer_index = 0
                    decl_node%line = 1
                    decl_node%column = 1

                    call arena%push(decl_node, "declaration", prog_index)
                    declaration_indices(decl_idx) = arena%size
                        end if
                    end if
                end do
            end block
        else
            allocate (declaration_indices(0))
        end if

    end subroutine generate_and_insert_declarations

    ! Collect variables from any statement type
    recursive subroutine collect_statement_vars(arena, stmt_index, var_names, &
                                                var_types, var_declared, &
                                                var_count, &
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
            ! Mark this variable as already declared - don't generate implicit &
            ! declaration
            call mark_variable_declared(stmt%var_name, var_names, &
                                         var_declared, var_count)
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

    ! Collect variables from assignment node
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
                        ! Check if the value is an array expression
                        var_type = "real(8)"  ! Default type
                        
                        ! Try to get type from the value expression
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
                                        var_type = get_fortran_type_string(value_type)
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

    ! Collect variable from identifier node
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
                if (allocated(identifier%inferred_type)) then
                    var_types(var_count) = &
                        get_fortran_type_string(identifier%inferred_type)
                else
                    var_types(var_count) = "real(8)"  ! Default type
                end if

                var_declared(var_count) = .true.
            end if
        end if
    end subroutine collect_identifier_var
    
    ! Collect variable from identifier node with explicit type
    subroutine collect_identifier_var_with_type(identifier, var_type, &
                                                var_names, var_types, &
                                                var_declared, &
                                                 var_count, function_names, func_count)
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

    ! Add a variable to the collection list
    subroutine add_variable(var_name, var_type, var_names, var_types, &
                            var_declared, var_count, &
                            function_names, func_count)
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: var_type
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
    
    ! Mark a variable as already declared
    subroutine mark_variable_declared(var_name, var_names, var_declared, var_count)
        character(len=*), intent(in) :: var_name
        character(len=64), intent(in) :: var_names(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(in) :: var_count
        integer :: i
        
        ! Find the variable if it exists and mark it as declared
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                var_declared(i) = .false.  ! Mark as already declared - don't &
                                            ! generate implicit declaration
                return
            end if
        end do
    end subroutine mark_variable_declared
    
    ! Check if a variable already has an explicit declaration
    function has_explicit_declaration(arena, prog, var_name) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=*), intent(in) :: var_name
        logical :: has_decl
        integer :: i
        
        has_decl = .false.
        
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            if (trim(stmt%var_name) == trim(var_name)) then
                                has_decl = .true.
                                return
                            end if
                        end select
                    end if
                end if
            end do
        end if
    end function has_explicit_declaration

    ! Convert mono_type_t to Fortran type string
    recursive function get_fortran_type_string(mono_type) result(type_str)
        type(mono_type_t), intent(in) :: mono_type
        character(len=:), allocatable :: type_str

        select case (mono_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            if (standardizer_type_standardization_enabled) then
                type_str = "real(8)"
            else
                type_str = "real"
            end if
        case (TLOGICAL)
            type_str = "logical"
        case (TCHAR)
            if (mono_type%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') mono_type%size
                    type_str = "character(len="//trim(size_str)//")"
                end block
            else
                type_str = "character(*)"
            end if
        case (TARRAY)
            ! For arrays, get the element type
            if (allocated(mono_type%args) .and. size(mono_type%args) > 0) then
                type_str = get_fortran_type_string(mono_type%args(1))
            else
                type_str = "real(8)"  ! Default array element type
            end if
        case default
            type_str = "real(8)"  ! Default fallback
        end select
    end function get_fortran_type_string

    ! Check if a mono_type_t is an array type
    function is_array_type(mono_type) result(is_array)
        type(mono_type_t), intent(in) :: mono_type
        logical :: is_array
        
        is_array = (mono_type%kind == TARRAY)
    end function is_array_type
    
    ! Get the type of an expression from the AST
    function get_expression_type(arena, expr_index) result(expr_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t), pointer :: expr_type
        
        expr_type => null()
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (identifier_node)
            if (allocated(node%inferred_type)) then
                expr_type => node%inferred_type
            end if
        type is (array_literal_node)
            if (allocated(node%inferred_type)) then
                expr_type => node%inferred_type
            end if
        type is (call_or_subscript_node)
            ! Check if this is an array subscript
            if (allocated(node%inferred_type)) then
                expr_type => node%inferred_type
            else
                ! If it's a subscript of an array, the result should be the &
                ! element type or a subarray
                ! For now, we'll check if it has a colon operator (array slice)
                if (has_array_slice_args(arena, node)) then
                    ! This is an array slice, so result is an array
                    ! We need to get the base array type
                    allocate(expr_type)
                    expr_type%kind = TARRAY
                    ! TODO: Set proper element type
                end if
            end if
        type is (binary_op_node)
            if (allocated(node%inferred_type)) then
                expr_type => node%inferred_type
            end if
        type is (literal_node)
            if (allocated(node%inferred_type)) then
                expr_type => node%inferred_type
            end if
        end select
    end function get_expression_type
    
    ! Check if a call_or_subscript node has array slice arguments
    function has_array_slice_args(arena, node) result(has_slice)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        logical :: has_slice
        integer :: i
        
        has_slice = .false.
        
        if (.not. allocated(node%arg_indices)) return
        
        do i = 1, size(node%arg_indices)
            if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                if (allocated(arena%entries(node%arg_indices(i))%node)) then
                    select type (arg => arena%entries(node%arg_indices(i))%node)
                    type is (binary_op_node)
                        if (trim(arg%operator) == ":") then
                            has_slice = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function has_array_slice_args
    
    ! Check if an expression is an array expression by structure
    function is_array_expression(arena, expr_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        logical :: is_array
        
        is_array = .false.
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            is_array = .true.
        type is (call_or_subscript_node)
            ! Check if this is an array slice (has colon operator in args)
            if (has_array_slice_args(arena, node)) then
                is_array = .true.
            end if
        end select
    end function is_array_expression
    
    ! Get array variable type declaration from an array expression
    function get_array_var_type(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type
        
        var_type = "real(8), dimension(:), allocatable"  ! Default
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            ! For array literals, we know the exact size
            if (allocated(node%element_indices)) then
                write(var_type, '(a,i0,a)') "real(8), dimension(", &
                    size(node%element_indices), ")"
            end if
        type is (call_or_subscript_node)
            ! For array slices, try to calculate the size
            if (has_array_slice_args(arena, node)) then
                ! For now, use allocatable. TODO: Calculate slice size
                var_type = "real(8), dimension(:), allocatable"
            end if
        end select
    end function get_array_var_type

    ! Standardize existing declaration nodes (e.g., real -> real(8))
    subroutine standardize_declarations(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

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

    ! Standardize function and subroutine definitions
    subroutine standardize_subprograms(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (function_def_node)
                        call standardize_function_def(arena, stmt, prog%body_indices(i))
                    type is (subroutine_def_node)
                        call standardize_subroutine_def(arena, stmt, prog%body_indices(i))
                    end select
                end if
            end if
        end do
    end subroutine standardize_subprograms

    ! Standardize a function definition
    subroutine standardize_function_def(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j
        character(len=:), allocatable :: return_type_str

        ! Standardize return type
        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "real") then
                func_def%return_type = "real(8)"
            end if
        else
            ! Default to real(8) if no return type specified
            func_def%return_type = "real(8)"
        end if

        ! Add implicit none at the beginning of function body
        if (allocated(func_def%body_indices)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                         line=1, column=1, parent_index=func_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(func_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(func_def%body_indices)
                new_body_indices(i + 1) = func_def%body_indices(i)
            end do
            func_def%body_indices = new_body_indices
        end if

        ! Standardize parameter declarations
        call standardize_function_parameters(arena, func_def, func_index)

        ! Update the arena entry
        arena%entries(func_index)%node = func_def
    end subroutine standardize_function_def

    ! Standardize function parameters by updating existing declarations or &
    ! adding new ones
    subroutine standardize_function_parameters(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer, allocatable :: param_names_found(:)
        integer :: i, j, n_params, n_body, param_idx
        character(len=64) :: param_name
        character(len=64), allocatable :: param_names(:)
        logical :: is_param_decl, param_updated

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        param_names_found = 0

        do i = 1, n_params
   if (func_def%param_indices(i) > 0 .and. func_def%param_indices(i) <= arena%size) then
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        param_names(i) = param%name
                    end select
                end if
            end if
        end do

        ! Update existing parameter declarations and track what we find
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
     if (func_def%body_indices(i) > 0 .and. func_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                      select type (stmt => arena%entries(func_def%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if this declaration is for a parameter
                            is_param_decl = .false.
                            param_idx = 0
                            do j = 1, n_params
                                if (stmt%var_name == param_names(j)) then
                                    is_param_decl = .true.
                                    param_idx = j
                                    exit
                                end if
                            end do

                            if (is_param_decl) then
                                ! Update the declaration to have intent(in) and &
                                ! preserve/enhance type
                                if (stmt%type_name == "real") then
                                    stmt%type_name = "real"
                                    stmt%has_kind = .true.
                                    stmt%kind_value = 8
                                ! Keep integer, logical, character as-is
                                end if
                                stmt%intent = "in"
                                stmt%has_intent = .true.
                                param_names_found(param_idx) = func_def%body_indices(i)
                                ! Update in arena
                                arena%entries(func_def%body_indices(i))%node = stmt
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Add declarations for parameters not found
        n_body = 0
        if (allocated(func_def%body_indices)) n_body = size(func_def%body_indices)

        ! Count how many new declarations we need
        j = 0
        do i = 1, n_params
            if (param_names_found(i) == 0) j = j + 1
        end do

        if (j > 0) then
            ! We need to add some parameter declarations
            allocate (new_body_indices(n_body + j))

            ! Copy implicit none (should be first) if it exists
            if (n_body > 0) then
                new_body_indices(1) = func_def%body_indices(1)
                j = 2
            else
                j = 1
            end if

            ! Add missing parameter declarations
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    ! Create declaration node with intent(in)
                    param_decl%type_name = "real"
                    param_decl%var_name = param_names(i)
                    param_decl%has_kind = .true.
                    param_decl%kind_value = 8
                    param_decl%intent = "in"
                    param_decl%has_intent = .true.
                    param_decl%line = 1
                    param_decl%column = 1
                    call arena%push(param_decl, "param_decl", func_index)
                    new_body_indices(j) = arena%size
                    j = j + 1
                end if
            end do

            ! Copy rest of body (skip first if it was implicit none)
            if (n_body > 1) then
                do i = 2, n_body
                    new_body_indices(j) = func_def%body_indices(i)
                    j = j + 1
                end do
            end if

            func_def%body_indices = new_body_indices
        end if
    end subroutine standardize_function_parameters

    ! Standardize subroutine definitions similar to function definitions
    subroutine standardize_subroutine_def(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j
        logical :: has_implicit_none

        ! Check if implicit none already exists
        has_implicit_none = .false.
        if (allocated(sub_def%body_indices)) then
            do i = 1, size(sub_def%body_indices)
                if (sub_def%body_indices(i) > 0 .and. sub_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(sub_def%body_indices(i))%node)) then
                        select type (node => arena%entries(sub_def%body_indices(i))%node)
                        type is (literal_node)
                            if (allocated(node%value)) then
                                if (index(node%value, "implicit none") > 0) then
                                    has_implicit_none = .true.
                                    exit
                                end if
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Add implicit none at the beginning of subroutine body if not present
        if (allocated(sub_def%body_indices) .and. .not. has_implicit_none) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                         line=1, column=1, parent_index=sub_index)

            ! Create new body with implicit none at the beginning
            allocate (new_body_indices(size(sub_def%body_indices) + 1))
            new_body_indices(1) = implicit_none_index
            do i = 1, size(sub_def%body_indices)
                new_body_indices(i + 1) = sub_def%body_indices(i)
            end do
            sub_def%body_indices = new_body_indices

            ! Update parent references
            arena%entries(implicit_none_index)%parent_index = sub_index
        else if (.not. allocated(sub_def%body_indices)) then
            ! For empty body, just add implicit none
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                         line=1, column=1, parent_index=sub_index)
            
            allocate(sub_def%body_indices(1))
            sub_def%body_indices(1) = implicit_none_index
            
            ! Update parent references
            arena%entries(implicit_none_index)%parent_index = sub_index
        end if

        ! Standardize parameters if present
        if (allocated(sub_def%param_indices)) then
            call standardize_subroutine_parameters(arena, sub_def, sub_index)
        end if

        ! Update the arena entry
        arena%entries(sub_index)%node = sub_def
    end subroutine standardize_subroutine_def

    ! Standardize subroutine parameters by updating existing declarations or
    ! adding new ones
    subroutine standardize_subroutine_parameters(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer, allocatable :: param_names_found(:)
        integer :: i, j, n_params, n_body, param_idx
        character(len=64) :: param_name
        character(len=64), allocatable :: param_names(:)
        logical :: is_param_decl, param_updated

        if (.not. allocated(sub_def%param_indices)) return
        n_params = size(sub_def%param_indices)
        if (n_params == 0) return

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        param_names_found = 0

        do i = 1, n_params
            if (sub_def%param_indices(i) > 0 .and. sub_def%param_indices(i) <= arena%size) then
                if (allocated(arena%entries(sub_def%param_indices(i))%node)) then
                    select type (param => arena%entries(sub_def%param_indices(i))%node)
                    type is (identifier_node)
                        param_names(i) = param%name
                    type is (parameter_declaration_node)
                        param_names(i) = param%name
                    type is (declaration_node)
                        param_names(i) = param%var_name
                    class default
                        param_names(i) = ""  ! Set default for unknown node types
                    end select
                else
                    param_names(i) = ""  ! Set default for unallocated nodes
                end if
            else
                param_names(i) = ""  ! Set default for invalid indices
            end if
        end do

        ! Check existing body for parameter declarations and update them
        if (allocated(sub_def%body_indices)) then
            n_body = size(sub_def%body_indices)

            do i = 1, n_body
                if (sub_def%body_indices(i) > 0 .and. sub_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(sub_def%body_indices(i))%node)) then
                        select type (stmt => arena%entries(sub_def%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if this declares a parameter
                            is_param_decl = .false.
                            param_idx = 0
                            do j = 1, n_params
                                if (trim(param_names(j)) == trim(stmt%var_name)) then
                                    is_param_decl = .true.
                                    param_idx = j
                                    exit
                                end if
                            end do

                            if (is_param_decl) then
                                param_names_found(param_idx) = i

                                ! Standardize the type in a block to allow modification
                                block
                                    type(declaration_node) :: updated_decl
                                    updated_decl = stmt
                                    
                                    if (allocated(updated_decl%type_name)) then
                                        if (updated_decl%type_name == "real") then
                                            updated_decl%type_name = "real"
                                            updated_decl%has_kind = .true.
                                            updated_decl%kind_value = 8
                                        end if
                                    else
                                        ! Infer type from name convention
                                        param_name = param_names(param_idx)
                                        call infer_parameter_type(param_name, updated_decl%type_name, &
                                                                 updated_decl%has_kind, updated_decl%kind_value)
                                    end if
                                    
                                    ! Update the declaration in arena
                                    arena%entries(sub_def%body_indices(i))%node = updated_decl
                                end block
                            end if
                        end select
                    end if
                end if
            end do

            ! Add declarations for parameters that weren't found
            n_body = size(sub_def%body_indices)
            j = 0
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    j = j + 1
                end if
            end do

            if (j > 0) then
                ! Need to add declarations
                allocate (new_body_indices(n_body + j))

                ! Copy first statement (should be implicit none)
                new_body_indices(1) = sub_def%body_indices(1)

                ! Add new parameter declarations
                j = 1
                do i = 1, n_params
                    if (param_names_found(i) == 0) then
                        param_decl%var_name = param_names(i)

                        ! Infer type from name convention
                        call infer_parameter_type(param_names(i), param_decl%type_name, &
                                                 param_decl%has_kind, param_decl%kind_value)

                        param_decl%line = 1
                        param_decl%column = 1

                        call arena%push(param_decl, "parameter_decl", sub_index)
                        j = j + 1
                        new_body_indices(j) = arena%size
                    end if
                end do

                ! Copy rest of body
                do i = 2, n_body
                    j = j + 1
                    new_body_indices(j) = sub_def%body_indices(i)
                end do

                sub_def%body_indices = new_body_indices
            end if
        end if
    end subroutine standardize_subroutine_parameters

    ! Infer parameter type from naming convention
    subroutine infer_parameter_type(param_name, type_name, has_kind, kind_value)
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable, intent(out) :: type_name
        logical, intent(out) :: has_kind
        integer, intent(out) :: kind_value
        
        has_kind = .false.
        kind_value = 0
        
        if (len_trim(param_name) > 0) then
            ! Apply Fortran implicit typing convention (i-n for integers)
            if (param_name(1:1) >= 'i' .and. param_name(1:1) <= 'n') then
                type_name = "integer"
            else
                type_name = "real"
                has_kind = .true.
                kind_value = 8
            end if
        else
            ! Default to real for empty names
            type_name = "real"
            has_kind = .true.
            kind_value = 8
        end if
    end subroutine infer_parameter_type

    ! Wrap a standalone function in a program
    subroutine wrap_function_in_program(arena, func_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: func_index
        type(program_node) :: prog
        type(contains_node) :: contains_stmt
        integer :: prog_index, implicit_none_index, contains_index
        integer, allocatable :: body_indices(:)

        ! Create program node
        prog%name = "main"
        prog%line = 1
        prog%column = 1

        ! Create implicit none
        implicit_none_index = push_implicit_statement(arena, .true., &
                                                     line=1, column=1, parent_index=0)

        ! Create contains statement
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        ! Standardize the function first
        select type (func => arena%entries(func_index)%node)
        type is (function_def_node)
            call standardize_function_def(arena, func, func_index)
        end select

        ! Build program body: implicit none, contains, function
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = func_index
        prog%body_indices = body_indices

        ! Add program to arena
        call arena%push(prog, "program", 0)
        prog_index = arena%size

        ! Update parent references
        arena%entries(implicit_none_index)%parent_index = prog_index
        arena%entries(contains_index)%parent_index = prog_index
        arena%entries(func_index)%parent_index = prog_index

        ! Update root index to point to the program
        func_index = prog_index
    end subroutine wrap_function_in_program

    ! Wrap a standalone subroutine in a program
    subroutine wrap_subroutine_in_program(arena, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: sub_index
        type(program_node) :: prog
        type(contains_node) :: contains_stmt
        integer :: prog_index, implicit_none_index, contains_index
        integer, allocatable :: body_indices(:)

        ! Create program node
        prog%name = "main"
        prog%line = 1
        prog%column = 1

        ! Create implicit none
        implicit_none_index = push_implicit_statement(arena, .true., &
                                                     line=1, column=1, parent_index=0)

        ! Create contains statement
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        ! Standardize the subroutine
        if (sub_index > 0 .and. sub_index <= arena%size) then
            if (allocated(arena%entries(sub_index)%node)) then
                select type (sub_node => arena%entries(sub_index)%node)
                type is (subroutine_def_node)
                    call standardize_subroutine_def(arena, sub_node, sub_index)
                end select
            end if
        end if

        ! Build program body: implicit none, contains, subroutine
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = sub_index
        prog%body_indices = body_indices

        ! Add program to arena
        call arena%push(prog, "program", 0)
        prog_index = arena%size

        ! Update parent references
        arena%entries(implicit_none_index)%parent_index = prog_index
        arena%entries(contains_index)%parent_index = prog_index
        arena%entries(sub_index)%parent_index = prog_index

        ! Update root index to point to the program
        sub_index = prog_index
    end subroutine wrap_subroutine_in_program

    ! Set type standardization configuration for standardizer
    subroutine set_standardizer_type_standardization(enabled)
        logical, intent(in) :: enabled
        standardizer_type_standardization_enabled = enabled
    end subroutine set_standardizer_type_standardization

    ! Get current type standardization configuration for standardizer
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

end module standardizer
