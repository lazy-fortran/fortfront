module standardizer_subprograms
    ! Function/subroutine standardization module
    ! Handles function and subroutine transformations, wrapping, and parameter processing
    
    use ast_core
    use ast_factory
    use type_system_unified
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
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
    public :: standardize_function_result

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

        ! Ensure function result variable is standardized and declared
        call standardize_function_result(arena, func_def, func_index)

        ! Update the arena entry
        arena%entries(func_index)%node = func_def
    end subroutine standardize_function_def

    ! Ensure a function has a proper result(...) clause and a declared result variable
    subroutine standardize_function_result(arena, func_def, func_index)
        use ast_nodes_core, only: assignment_node, identifier_node
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        integer :: i
        character(len=:), allocatable :: res_name
        logical :: has_decl
        integer, allocatable :: new_body_indices(:)
        type(declaration_node) :: decl
        logical :: type_std_enabled

        call get_standardizer_type_standardization(type_std_enabled)

        ! If result variable not set, try to infer from first assignment target
        if ((.not. allocated(func_def%result_variable)) .or. len_trim(func_def%result_variable) == 0) then
            if (allocated(func_def%body_indices)) then
                do i = 1, size(func_def%body_indices)
                    if (func_def%body_indices(i) > 0 .and. func_def%body_indices(i) <= arena%size) then
                        if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                            select type (stmt => arena%entries(func_def%body_indices(i))%node)
                            type is (assignment_node)
                                if (stmt%target_index > 0 .and. stmt%target_index <= arena%size) then
                                    if (allocated(arena%entries(stmt%target_index)%node)) then
                                        select type (t => arena%entries(stmt%target_index)%node)
                                        type is (identifier_node)
                                            res_name = t%name
                                            exit
                                        end select
                                    end if
                                end if
                            end select
                        end if
                    end if
                end do
            end if
            if (allocated(res_name)) then
                if (len_trim(res_name) > 0) then
                    func_def%result_variable = trim(res_name)
                end if
            end if
        end if

        ! If still no result variable, nothing to do
        if ((.not. allocated(func_def%result_variable)) .or. len_trim(func_def%result_variable) == 0) return

        ! Check if a declaration exists for the result variable
        has_decl = .false.
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                if (func_def%body_indices(i) > 0 .and. func_def%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                        select type (stmt => arena%entries(func_def%body_indices(i))%node)
                        type is (declaration_node)
                            if (trim(stmt%var_name) == trim(func_def%result_variable)) then
                                has_decl = .true.
                                exit
                            end if
                        end select
                    end if
                end if
            end do
        end if

        if (has_decl) return

        ! Insert a declaration for the result variable after implicit none
        call infer_parameter_type(func_def%result_variable, decl%type_name, decl%has_kind, decl%kind_value)
        if (decl%type_name == "real" .and. type_std_enabled) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if
        decl%var_name = trim(func_def%result_variable)
        decl%intent = ""
        decl%has_intent = .false.
        decl%is_optional = .false.
        decl%is_array = .false.
        decl%is_allocatable = .false.
        decl%initializer_index = 0
        decl%line = 1
        decl%column = 1

        ! Push declaration into arena under the function node
        call arena%push(decl, "declaration", func_index)

        if (.not. allocated(func_def%body_indices)) then
            allocate(func_def%body_indices(1))
            func_def%body_indices(1) = arena%size
        else
            ! Insert as the second statement (after implicit none which we added earlier)
            allocate(new_body_indices(size(func_def%body_indices) + 1))
            if (size(func_def%body_indices) == 0) then
                new_body_indices(1) = arena%size
            else
                new_body_indices(1) = func_def%body_indices(1)
                new_body_indices(2) = arena%size
                if (size(func_def%body_indices) > 1) then
                    new_body_indices(3:) = func_def%body_indices(2:)
                end if
            end if
            func_def%body_indices = new_body_indices
        end if

        ! Update arena entry with modified function
        arena%entries(func_index)%node = func_def
    end subroutine standardize_function_result

    ! Standardize function parameters by updating existing declarations or adding new ones
    subroutine standardize_function_parameters(arena, func_def, func_index)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer, allocatable :: param_names_found(:)
        
        logical, allocatable :: fn_param_optional(:)
        character(len=8), allocatable :: fn_param_intent(:)
        
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
        allocate (fn_param_optional(n_params))
        allocate (fn_param_intent(n_params))
        param_names_found = 0
        fn_param_optional = .false.
        fn_param_intent = ""
        
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
                        fn_param_optional(i) = param%is_optional
                        select case (param%intent_type)
                        case (INTENT_IN)
                            fn_param_intent(i) = "in"
                        case (INTENT_OUT)
                            fn_param_intent(i) = "out"
                        case (INTENT_INOUT)
                            fn_param_intent(i) = "inout"
                        case default
                            fn_param_intent(i) = ""
                        end select
                        ! captured above into param_optional/param_intent
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

                            if (stmt%is_multi_declaration .and. &
                                allocated(stmt%var_names)) then
                                block
                                    logical :: matched_multi
                                    integer :: pidx, k
                                    character(len=8) :: stmt_intent

                                    matched_multi = .false.
                                    stmt_intent = ""
                                    do j = 1, size(stmt%var_names)
                                        pidx = 0
                                        do k = 1, n_params
                                            if (trim(stmt%var_names(j)) == trim(param_names(k))) then
                                                pidx = k
                                                exit
                                            end if
                                        end do
                                        if (pidx > 0) then
                                            matched_multi = .true.
                                            if (len_trim(fn_param_intent(pidx)) == 0) then
                                                fn_param_intent(pidx) = "in"
                                            end if
                                            if (fn_param_optional(pidx)) stmt%is_optional = .true.
                                            if (len_trim(stmt_intent) == 0) stmt_intent = fn_param_intent(pidx)
                                            param_names_found(pidx) = func_def%body_indices(i)
                                        end if
                                    end do
                                    if (matched_multi) then
                                        if (stmt%type_name == "real") then
                                            if (standardizer_type_standardization_enabled) then
                                                stmt%type_name = "real"
                                                stmt%has_kind = .true.
                                                stmt%kind_value = 8
                                            end if
                                        end if
                                        if (len_trim(stmt_intent) == 0) stmt_intent = "in"
                                        stmt%intent = stmt_intent
                                        stmt%has_intent = .true.
                                        arena%entries(func_def%body_indices(i))%node = stmt
                                    end if
                                end block
                            else if (is_param_decl) then
                                ! Update the declaration to have intent(in) and preserve/enhance type
                                if (stmt%type_name == "real") then
                                    if (standardizer_type_standardization_enabled) then
                                        stmt%type_name = "real"
                                        stmt%has_kind = .true.
                                        stmt%kind_value = 8
                                    end if
                                ! Keep integer, logical, character as-is
                                end if
                                if (len_trim(fn_param_intent(param_idx)) > 0) then
                                    stmt%intent = fn_param_intent(param_idx)
                                    stmt%has_intent = .true.
                                else
                                    stmt%intent = "in"
                                    stmt%has_intent = .true.
                                end if
                                if (fn_param_optional(param_idx)) stmt%is_optional = .true.
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
        call add_missing_parameter_declarations_ext(arena, func_def, func_index, &
                                              param_names, param_names_found, n_params, &
                                              fn_param_optional, fn_param_intent, &
                                              standardizer_type_standardization_enabled)

    end subroutine standardize_function_parameters

    ! Add missing parameter declarations
    subroutine add_missing_parameter_declarations_ext(arena, func_def, func_index, &
                                                param_names, param_names_found, n_params, &
                                                param_optional, param_intent, type_std_enabled)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index, n_params
        character(len=64), intent(in) :: param_names(:)
        integer, intent(in) :: param_names_found(:)
        logical, intent(in) :: param_optional(:)
        character(len=8), intent(in) :: param_intent(:)
        logical, intent(in) :: type_std_enabled
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
                    if (param_decl%type_name == "real" .and. type_std_enabled) then
                        param_decl%has_kind = .true.
                        param_decl%kind_value = 8
                    end if
                    param_decl%var_name = param_names(i)
                    if (len_trim(param_intent(i)) > 0) then
                        param_decl%intent = param_intent(i)
                        param_decl%has_intent = .true.
                    else
                        param_decl%intent = "in"
                        param_decl%has_intent = .true.
                    end if
                    if (param_optional(i)) param_decl%is_optional = .true.
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

    end subroutine add_missing_parameter_declarations_ext

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
        logical, allocatable :: sb_param_optional(:)
        character(len=8), allocatable :: sb_param_intent(:)
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
        allocate (sb_param_optional(n_params))
        allocate (sb_param_intent(n_params))
        param_names_found = 0
        sb_param_optional = .false.
        sb_param_intent = ""
        
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
                        sb_param_optional(i) = param%is_optional
                        select case (param%intent_type)
                        case (INTENT_IN)
                            sb_param_intent(i) = "in"
                        case (INTENT_OUT)
                            sb_param_intent(i) = "out"
                        case (INTENT_INOUT)
                            sb_param_intent(i) = "inout"
                        case default
                            sb_param_intent(i) = ""
                        end select
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

                            if (stmt%is_multi_declaration .and. &
                                allocated(stmt%var_names)) then
                                block
                                    logical :: matched_multi
                                    integer :: pidx, k
                                    character(len=8) :: stmt_intent

                                    matched_multi = .false.
                                    stmt_intent = ""
                                    do j = 1, size(stmt%var_names)
                                        pidx = 0
                                        do k = 1, n_params
                                            if (trim(stmt%var_names(j)) == trim(param_names(k))) then
                                                pidx = k
                                                exit
                                            end if
                                        end do
                                        if (pidx > 0) then
                                            matched_multi = .true.
                                            if (len_trim(sb_param_intent(pidx)) == 0) then
                                                sb_param_intent(pidx) = "inout"
                                            end if
                                            if (sb_param_optional(pidx)) stmt%is_optional = .true.
                                            if (len_trim(stmt_intent) == 0) stmt_intent = sb_param_intent(pidx)
                                            param_names_found(pidx) = sub_def%body_indices(i)
                                        end if
                                    end do
                                    if (matched_multi) then
                                        if (stmt%type_name == "real") then
                                            if (standardizer_type_standardization_enabled) then
                                                stmt%type_name = "real"
                                                stmt%has_kind = .true.
                                                stmt%kind_value = 8
                                            end if
                                        end if
                                        if (len_trim(stmt_intent) == 0) stmt_intent = "inout"
                                        stmt%intent = stmt_intent
                                        stmt%has_intent = .true.
                                        arena%entries(sub_def%body_indices(i))%node = stmt
                                    end if
                                end block
                            else if (is_param_decl) then
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
                                if (len_trim(sb_param_intent(param_idx)) > 0) then
                                    stmt%intent = sb_param_intent(param_idx)
                                else
                                    stmt%intent = "inout"
                                end if
                                stmt%has_intent = .true.
                                if (sb_param_optional(param_idx)) stmt%is_optional = .true.
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
        call add_missing_subroutine_parameter_declarations_ext(arena, sub_def, sub_index, &
                                                          param_names, param_names_found, n_params, &
                                                          sb_param_optional, sb_param_intent, &
                                                          standardizer_type_standardization_enabled)

    end subroutine standardize_subroutine_parameters

    ! Add missing subroutine parameter declarations
    subroutine add_missing_subroutine_parameter_declarations_ext(arena, sub_def, sub_index, &
                                                           param_names, param_names_found, n_params, &
                                                           param_optional, param_intent, type_std_enabled)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(inout) :: sub_def
        integer, intent(in) :: sub_index, n_params
        character(len=64), intent(in) :: param_names(:)
        integer, intent(in) :: param_names_found(:)
        logical, intent(in) :: param_optional(:)
        character(len=8), intent(in) :: param_intent(:)
        logical, intent(in) :: type_std_enabled
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

            ! Add new parameter declarations after implicit none when present; otherwise at start
            new_decl_count = 0
            do i = 1, n_params
                if (param_names_found(i) == 0) then
                    ! Create declaration for this parameter
                    call infer_parameter_type(param_names(i), param_decl%type_name, &
                                            param_decl%has_kind, param_decl%kind_value)
                    if (param_decl%type_name == "real" .and. type_std_enabled) then
                        param_decl%has_kind = .true.
                        param_decl%kind_value = 8
                    end if
                    param_decl%var_name = param_names(i)
                    if (len_trim(param_intent(i)) > 0) then
                        param_decl%intent = param_intent(i)
                        param_decl%has_intent = .true.
                    else
                        param_decl%intent = "inout"
                        param_decl%has_intent = .true.
                    end if
                    if (param_optional(i)) param_decl%is_optional = .true.
                    param_decl%is_array = .false.
                    param_decl%is_allocatable = .false.
                    param_decl%initializer_index = 0
                    param_decl%line = 1
                    param_decl%column = 1

                    call arena%push(param_decl, "declaration", sub_index)
                    new_decl_count = new_decl_count + 1

                    ! Determine insertion base: 2 if implicit none exists (n_body>=1), else 1
                    if (n_body >= 1) then
                        ! Shift existing statements down to make room after implicit none
                        do j = n_body + new_decl_count, 3, -1
                            new_body_indices(j) = new_body_indices(j-1)
                        end do
                        new_body_indices(1 + new_decl_count) = arena%size
                    else
                        ! No existing body (no implicit none yet); append sequentially starting at 1
                        new_body_indices(new_decl_count) = arena%size
                    end if
                end if
            end do

            sub_def%body_indices = new_body_indices
        end if

    end subroutine add_missing_subroutine_parameter_declarations_ext

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
