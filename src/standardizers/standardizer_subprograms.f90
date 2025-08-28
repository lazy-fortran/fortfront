module standardizer_subprograms
    ! Function/subroutine standardization module
    ! Handles function and subroutine transformations, wrapping, and parameter processing
    
    use ast_core
    use ast_factory
    use type_system_unified
    implicit none
    private

    ! Type standardization configuration (local copy)
    logical, save :: standardizer_type_standardization_enabled = .true.

    public :: standardize_subprograms
    public :: standardize_function_def
    public :: standardize_subroutine_def
    public :: standardize_function_parameters
    public :: standardize_subroutine_parameters
    public :: wrap_function_in_program
    public :: wrap_subroutine_in_program
    public :: infer_parameter_type

contains

    ! Local implementation of get_standardizer_type_standardization
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

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
        logical :: standardizer_type_standardization_enabled

        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        ! Standardize return type
        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "real") then
                if (standardizer_type_standardization_enabled) then
                    func_def%return_type = "real(8)"
                else
                    func_def%return_type = "real"
                end if
            end if
        else
            ! Function return type should be explicitly declared
            ! No default assumption - let it remain unspecified
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

    ! Standardize function parameters by updating existing declarations or adding new ones
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
        logical :: standardizer_type_standardization_enabled

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        param_names_found = 0
        
        ! Initialize all param_names to avoid undefined behavior
        do i = 1, n_params
            param_names(i) = ""
        end do

        do i = 1, n_params
            if (func_def%param_indices(i) > 0 .and. func_def%param_indices(i) <= arena%size) then
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        param_names(i) = param%name
                    type is (parameter_declaration_node)
                        param_names(i) = param%name
                    type is (declaration_node)
                        param_names(i) = param%var_name
                    class default
                        ! Try to get a reasonable default name
                        write(param_names(i), '(a,i0)') "param", i
                    end select
                else
                    ! Node not allocated, create default name
                    write(param_names(i), '(a,i0)') "param", i
                end if
            else
                ! Invalid index, create default name
                write(param_names(i), '(a,i0)') "param", i
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
                                ! Update the declaration to have intent(in) and preserve/enhance type
                                if (stmt%type_name == "real") then
                                    if (standardizer_type_standardization_enabled) then
                                        stmt%type_name = "real"
                                        stmt%has_kind = .true.
                                        stmt%kind_value = 8
                                    end if
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
        call add_missing_parameter_declarations(arena, func_def, func_index, &
                                              param_names, param_names_found, n_params)

    end subroutine standardize_function_parameters

    ! Add missing parameter declarations
    subroutine add_missing_parameter_declarations(arena, func_def, func_index, &
                                                param_names, param_names_found, n_params)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index, n_params
        character(len=64), intent(in) :: param_names(:)
        integer, intent(in) :: param_names_found(:)
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer :: i, j, n_body, new_decl_count

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

            ! Copy existing body statements
            do i = 1, n_body
                new_body_indices(i) = func_def%body_indices(i)
            end do

            ! Add new parameter declarations after implicit none (position 2)
            new_decl_count = 0
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    ! Create declaration for this parameter
                    call infer_parameter_type(param_names(i), param_decl%type_name, &
                                            param_decl%has_kind, param_decl%kind_value)
                    param_decl%var_name = param_names(i)
                    param_decl%intent = "in"
                    param_decl%has_intent = .true.
                    param_decl%is_array = .false.
                    param_decl%is_allocatable = .false.
                    param_decl%initializer_index = 0
                    param_decl%line = 1
                    param_decl%column = 1

                    call arena%push(param_decl, "declaration", func_index)
                    new_decl_count = new_decl_count + 1
                    
                    ! Insert after implicit none, shift other statements down
                    ! Move all statements from position 2 onwards down by one
                    do j = n_body + new_decl_count, 3, -1
                        new_body_indices(j) = new_body_indices(j-1)
                    end do
                    new_body_indices(1 + new_decl_count) = arena%size
                end if
            end do

            func_def%body_indices = new_body_indices
        end if

    end subroutine add_missing_parameter_declarations

    ! Standardize a subroutine definition
    subroutine standardize_subroutine_def(arena, sub_def, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index, i, j

        ! Add implicit none at the beginning of subroutine body
        if (allocated(sub_def%body_indices)) then
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
        end if

        ! Standardize parameter declarations
        call standardize_subroutine_parameters(arena, sub_def, sub_index)

        ! Update the arena entry
        arena%entries(sub_index)%node = sub_def
    end subroutine standardize_subroutine_def

    ! Standardize subroutine parameters by updating existing declarations or adding new ones
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
        logical :: standardizer_type_standardization_enabled

        if (.not. allocated(sub_def%param_indices)) return
        n_params = size(sub_def%param_indices)
        if (n_params == 0) return

        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        param_names_found = 0
        
        ! Initialize all param_names to avoid undefined behavior
        do i = 1, n_params
            param_names(i) = ""
        end do

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
                        ! Try to get a reasonable default name
                        write(param_names(i), '(a,i0)') "param", i
                    end select
                else
                    ! Node not allocated, create default name
                    write(param_names(i), '(a,i0)') "param", i
                end if
            else
                ! Invalid index, create default name
                write(param_names(i), '(a,i0)') "param", i
            end if
        end do

        ! Update existing parameter declarations and track what we find
        if (allocated(sub_def%body_indices)) then
            do i = 1, size(sub_def%body_indices)
                if (sub_def%body_indices(i) > 0 .and. sub_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(sub_def%body_indices(i))%node)) then
                      select type (stmt => arena%entries(sub_def%body_indices(i))%node)
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
                                ! Update the declaration to have appropriate intent and preserve/enhance type
                                if (stmt%type_name == "real") then
                                    if (standardizer_type_standardization_enabled) then
                                        stmt%type_name = "real"
                                        stmt%has_kind = .true.
                                        stmt%kind_value = 8
                                    end if
                                ! Keep integer, logical, character as-is
                                end if
                                ! Default to intent(inout) for subroutine parameters
                                if (.not. stmt%has_intent) then
                                    stmt%intent = "inout"
                                    stmt%has_intent = .true.
                                end if
                                param_names_found(param_idx) = sub_def%body_indices(i)
                                ! Update in arena
                                arena%entries(sub_def%body_indices(i))%node = stmt
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Add declarations for parameters not found
        call add_missing_subroutine_parameter_declarations(arena, sub_def, sub_index, &
                                                          param_names, param_names_found, n_params)

    end subroutine standardize_subroutine_parameters

    ! Add missing subroutine parameter declarations
    subroutine add_missing_subroutine_parameter_declarations(arena, sub_def, sub_index, &
                                                           param_names, param_names_found, n_params)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index, n_params
        character(len=64), intent(in) :: param_names(:)
        integer, intent(in) :: param_names_found(:)
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer :: i, j, n_body, new_decl_count

        n_body = 0
        if (allocated(sub_def%body_indices)) n_body = size(sub_def%body_indices)

        ! Count how many new declarations we need
        j = 0
        do i = 1, n_params
            if (param_names_found(i) == 0) j = j + 1
        end do

        if (j > 0) then
            ! We need to add some parameter declarations
            allocate (new_body_indices(n_body + j))

            ! Copy existing body statements
            do i = 1, n_body
                new_body_indices(i) = sub_def%body_indices(i)
            end do

            ! Add new parameter declarations after implicit none (position 2)
            new_decl_count = 0
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    ! Create declaration for this parameter
                    call infer_parameter_type(param_names(i), param_decl%type_name, &
                                            param_decl%has_kind, param_decl%kind_value)
                    param_decl%var_name = param_names(i)
                    param_decl%intent = "inout"  ! Default for subroutines
                    param_decl%has_intent = .true.
                    param_decl%is_array = .false.
                    param_decl%is_allocatable = .false.
                    param_decl%initializer_index = 0
                    param_decl%line = 1
                    param_decl%column = 1

                    call arena%push(param_decl, "declaration", sub_index)
                    new_decl_count = new_decl_count + 1
                    
                    ! Insert after implicit none, shift other statements down
                    do j = n_body + new_decl_count, 3, -1
                        new_body_indices(j) = new_body_indices(j-1)
                    end do
                    new_body_indices(1 + new_decl_count) = arena%size
                end if
            end do

            sub_def%body_indices = new_body_indices
        end if

    end subroutine add_missing_subroutine_parameter_declarations

    ! Infer parameter type from name patterns
    subroutine infer_parameter_type(param_name, type_name, has_kind, kind_value)
        character(len=*), intent(in) :: param_name
        character(len=*), intent(out) :: type_name
        logical, intent(out) :: has_kind
        integer, intent(out) :: kind_value
        character :: first_char
        
        has_kind = .false.
        kind_value = 0
        
        if (len_trim(param_name) > 0) then
            first_char = param_name(1:1)
            select case (first_char)
            case ('i', 'j', 'k', 'l', 'm', 'n')
                type_name = "integer"
            case ('x', 'y', 'z', 'r', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'o', 'p', 'q', 's', 't', 'u', 'v', 'w')
                type_name = "real"
            case default
                type_name = "real"  ! Default to real
            end select
        else
            type_name = "real"  ! Default fallback
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

end module standardizer_subprograms