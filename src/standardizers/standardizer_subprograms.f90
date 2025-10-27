module standardizer_subprograms
    ! Function/subroutine standardization module
    ! Handles function and subroutine transformations, wrapping, and parameter processing

    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_data
    use ast_nodes_misc
    use ast_factory
    use type_system_unified
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use lexer_core, only: to_lower
    use type_string_utils, only: is_character_type_string
    use semantic_validation_utils, only: rename_identifier_in_arena
    implicit none
    private

    ! Type standardization configuration (local copy)
    ! DISABLED: Converting real -> real(8) breaks generic interfaces that
    ! depend on exact type matching. Users should explicitly use real(8) or
    ! kind parameters if they want double precision.
    logical, save :: standardizer_type_standardization_enabled = .false.

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

    ! Shared helper to synchronize parameter declarations in function/subroutine bodies
    subroutine synchronize_parameter_declarations(arena, body_indices, param_names, &
                                                  param_names_found, param_optional, &
                                                  param_intent, default_intent, &
                                                  standardize_types, param_type, &
                                                  param_type_inferred, &
                                                  param_has_kind, &
                                                  param_kind_value, param_is_array, &
                                                  param_is_allocatable)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: param_names(:)
        integer, intent(inout) :: param_names_found(:)
        logical, intent(in) :: param_optional(:)
        character(len=*), intent(inout) :: param_intent(:)
        character(len=*), intent(in) :: default_intent
        logical, intent(in) :: standardize_types
        character(len=*), intent(inout), optional :: param_type(:)
        logical, intent(inout), optional :: param_type_inferred(:)
        logical, intent(inout), optional :: param_has_kind(:)
        integer, intent(inout), optional :: param_kind_value(:)
        logical, intent(inout), optional :: param_is_array(:)
        logical, intent(inout), optional :: param_is_allocatable(:)

        integer :: i, current_index, n_params, param_idx, j, pidx
        logical :: is_param_decl, matched_multi
        character(len=16) :: stmt_intent

        n_params = size(param_names)
        if (n_params == 0) return

        do i = 1, size(body_indices)
            current_index = body_indices(i)
            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (declaration_node)
                is_param_decl = .false.
                param_idx = 0
                do j = 1, n_params
                    if (trim(param_names(j)) == trim(stmt%var_name)) then
                        is_param_decl = .true.
                        param_idx = j
                        exit
                    end if
                end do

                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    matched_multi = .false.
                    if (stmt%has_intent .and. allocated(stmt%intent)) then
                        stmt_intent = stmt%intent
                    else
                        stmt_intent = ""
                    end if
                    do j = 1, size(stmt%var_names)
                        pidx = find_param_index(trim(stmt%var_names(j)))
                        if (pidx > 0) then
                            call record_match(pidx, stmt, stmt_intent, current_index)
                            matched_multi = .true.
                        end if
                    end do
                    if (matched_multi) then
                        call apply_type_standardization(stmt)
                        if (len_trim(stmt_intent) == 0) stmt_intent = default_intent
                        if (len_trim(stmt_intent) > 0) then
                            stmt%intent = stmt_intent
                            stmt%has_intent = .true.
                        end if
                        arena%entries(current_index)%node = stmt
                    end if
                else if (is_param_decl) then
                    if (stmt%has_intent .and. allocated(stmt%intent)) then
                        stmt_intent = stmt%intent
                    else
                        stmt_intent = ""
                    end if
                    call record_match(param_idx, stmt, stmt_intent, current_index)
                    ! If declaration has type_variable, infer the type
                    if (allocated(stmt%type_name) .and. &
                        trim(stmt%type_name) == "type_variable") then
                        block
                            character(len=32) :: inferred_type
                            logical :: has_kind_local
                            integer :: kind_value_local
                            call infer_parameter_type(param_names(param_idx), &
                                                      inferred_type, &
                                                      has_kind_local, kind_value_local)
                            stmt%type_name = trim(inferred_type)
                            if (has_kind_local) then
                                stmt%has_kind = .true.
                                stmt%kind_value = kind_value_local
                            end if
                        end block
                    end if
                    call apply_type_standardization(stmt)
                    if (len_trim(stmt_intent) == 0) stmt_intent = default_intent
                    if (len_trim(stmt_intent) > 0) then
                        stmt%intent = stmt_intent
                        stmt%has_intent = .true.
                    end if
                    arena%entries(current_index)%node = stmt
                end if
            end select
        end do

    contains

        integer function find_param_index(name) result(index)
            character(len=*), intent(in) :: name
            integer :: k
            index = 0
            do k = 1, n_params
                if (trim(param_names(k)) == trim(name)) then
                    index = k
                    return
                end if
            end do
        end function find_param_index

        subroutine record_match(pidx, stmt, stmt_intent, arena_index)
            integer, intent(in) :: pidx
            type(declaration_node), intent(inout) :: stmt
            character(len=*), intent(inout) :: stmt_intent
            integer, intent(in) :: arena_index

            if (len_trim(stmt_intent) == 0) then
                if (len_trim(param_intent(pidx)) > 0) then
                    stmt_intent = param_intent(pidx)
                else
                    stmt_intent = default_intent
                    param_intent(pidx) = default_intent
                end if
            else
                if (len_trim(param_intent(pidx)) == 0) then
                    param_intent(pidx) = stmt_intent
                end if
            end if

            if (param_optional(pidx)) stmt%is_optional = .true.
            param_names_found(pidx) = arena_index

            if (present(param_type)) then
                if (allocated(stmt%type_name)) then
                    ! Skip type_variable - treat as if type is not present
                    if (trim(stmt%type_name) /= "type_variable") then
                        param_type(pidx) = trim(stmt%type_name)
                        if (present(param_type_inferred)) then
                            if (len_trim(param_type(pidx)) > 0) &
                                param_type_inferred(pidx) = .false.
                        end if
                    end if
                end if
            end if
            if (present(param_has_kind)) param_has_kind(pidx) = stmt%has_kind
            if (present(param_kind_value)) param_kind_value(pidx) = stmt%kind_value
            if (present(param_is_array)) param_is_array(pidx) = stmt%is_array
            if (present(param_is_allocatable)) &
                param_is_allocatable(pidx) = stmt%is_allocatable
        end subroutine record_match

        subroutine apply_type_standardization(stmt)
            type(declaration_node), intent(inout) :: stmt
            if (.not. standardize_types) return
            if (allocated(stmt%type_name)) then
                if (trim(stmt%type_name) == "real") then
                    stmt%type_name = "real"
                    stmt%has_kind = .true.
                    stmt%kind_value = 8
                end if
            end if
        end subroutine apply_type_standardization

    end subroutine synchronize_parameter_declarations

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
                        call standardize_function_def(arena, stmt, &
                                                      prog%body_indices(i))
                    type is (subroutine_def_node)
                        call standardize_subroutine_def(arena, stmt, &
                                                        prog%body_indices(i))
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
                                                          line=1, column=1, &
                                                          parent_index=func_index)

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
        integer :: decl_index
        integer :: name_pos
        integer, allocatable :: new_body_indices(:)
        type(declaration_node) :: decl
        type(declaration_node) :: existing_decl
        logical :: type_std_enabled
        logical :: result_inferred

        call get_standardizer_type_standardization(type_std_enabled)

        ! If result variable not set, try to infer from first assignment target
        if ((.not. allocated(func_def%result_variable)) .or. &
            len_trim(func_def%result_variable) == 0) then
            if (allocated(func_def%body_indices)) then
                do i = 1, size(func_def%body_indices)
                    if (func_def%body_indices(i) > 0 .and. &
                        func_def%body_indices(i) <= &
                        arena%size) then
                       if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                            select type (stmt => &
                                         arena%entries(func_def%body_indices(i))%node)
                            type is (assignment_node)
                                if (stmt%target_index > 0 .and. stmt%target_index <= &
                                    arena%size) then
                              if (allocated(arena%entries(stmt%target_index)%node)) then
                                        select type (t => &
                                                  arena%entries(stmt%target_index)%node)
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
                    ! If result variable is not named result rename all occurrences
                    ! within the function body only
                    if (trim(res_name) /= 'result') then
                        if (allocated(func_def%body_indices)) then
                            call rename_identifier_in_arena(arena, 'result', &
                                                trim(res_name), func_def%body_indices, &
                                                            func_index)
                        end if
                    end if
                end if
            end if
        end if

        ! If still no result variable, nothing to do
        if ((.not. allocated(func_def%result_variable)) .or. &
            len_trim(func_def%result_variable) == 0) return

        ! Check if a declaration exists for the result variable
        has_decl = .false.
        decl_index = 0
        name_pos = -1
        if (allocated(func_def%body_indices)) then
            do i = 1, size(func_def%body_indices)
                if (func_def%body_indices(i) > 0 .and. func_def%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(func_def%body_indices(i))%node)) then
                        select type (stmt => &
                                     arena%entries(func_def%body_indices(i))%node)
                        type is (declaration_node)
                            if (trim(stmt%var_name) == &
                                trim(func_def%result_variable)) then
                                has_decl = .true.
                                decl_index = func_def%body_indices(i)
                                existing_decl = stmt
                                exit
                            end if
                            if (stmt%is_multi_declaration .and. &
                                allocated(stmt%var_names)) then
                                do name_pos = 1, size(stmt%var_names)
                                    if (trim(stmt%var_names(name_pos)) == &
                                        trim(func_def%result_variable)) then
                                        has_decl = .true.
                                        decl_index = func_def%body_indices(i)
                                        existing_decl = stmt
                                        exit
                                    end if
                                end do
                                if (has_decl) exit
                            end if
                        end select
                    end if
                end if
            end do
        end if

        if (has_decl) then
            if (allocated(func_def%name)) then
                if (trim(func_def%result_variable) == trim(func_def%name)) then
                    if (is_character_length_decl(existing_decl%type_name)) then
                        func_def%return_type = ''
                        arena%entries(func_index)%node = func_def
                        return
                    end if
                end if
            end if
            if (existing_decl%is_array) then
                func_def%return_type = ''
                arena%entries(func_index)%node = func_def
                return
            end if
            if (.not. allocated(func_def%return_type) .or. &
                len_trim(func_def%return_type) == 0) then
                if (len_trim(existing_decl%type_name) > 0) then
                    if (existing_decl%has_kind .and. existing_decl%kind_value &
                        > 0 .and. &
                        existing_decl%type_name /= "character") then
                        block
                            character(len=64) :: buffer
                            write (buffer, '(A,"(",I0,")")') &
                                trim(existing_decl%type_name), &
                                existing_decl%kind_value
                            func_def%return_type = trim(buffer)
                        end block
                    else
                        func_def%return_type = trim(existing_decl%type_name)
                    end if
                    arena%entries(func_index)%node = func_def
                end if
            end if
            return
        end if

        decl%type_name = ""
        decl%has_kind = .false.
        decl%kind_value = 0
        decl%is_array = .false.
        decl%is_allocatable = .false.
        result_inferred = .false.

        if (allocated(func_def%return_type)) then
            if (func_def%return_type == "type_variable" .or. &
                func_def%return_type == "function" .or. &
                func_def%return_type == "derived_type") then
                func_def%return_type = ""
            end if
        end if

        if (allocated(func_def%return_type)) then
            if (len_trim(func_def%return_type) > 0) then
                block
                    character(len=:), allocatable :: rt
                    character(len=64) :: base_text
                    character(len=64) :: attr_text
                    integer :: open_pos, close_pos, read_stat, inner_close
                    integer :: kind_val

                    rt = trim(func_def%return_type)
                    open_pos = index(rt, "(")
                    if (open_pos > 0) then
                        inner_close = index(rt(open_pos + 1:), ")")
                        if (inner_close > 0) then
                            close_pos = open_pos + inner_close
                        else
                            close_pos = 0
                        end if
                    else
                        close_pos = 0
                    end if

                    if (open_pos > 0 .and. close_pos > open_pos) then
                        base_text = trim(rt(1:open_pos - 1))
                        attr_text = trim(rt(open_pos + 1:close_pos - 1))
                        read_stat = 0
                        read (attr_text, *, iostat=read_stat) kind_val
                        if (read_stat == 0) then
                            decl%type_name = trim(base_text)
                            decl%has_kind = .true.
                            decl%kind_value = kind_val
                        else
                            decl%type_name = trim(rt)
                        end if
                    else
                        decl%type_name = trim(rt)
                    end if
                end block
            end if
        end if

        if (len_trim(decl%type_name) == 0) then
            block
                character(len=32) :: inferred_type
                call infer_parameter_type(func_def%result_variable, inferred_type, &
                                          decl%has_kind, decl%kind_value)
                decl%type_name = trim(inferred_type)
            end block
            result_inferred = .true.
        end if

        if (decl%type_name == "real" .and. type_std_enabled .and. result_inferred) then
            decl%has_kind = .true.
            decl%kind_value = 8
        end if

        if (.not. allocated(func_def%return_type) .or. &
            len_trim(func_def%return_type) == 0) then
            if (decl%has_kind .and. decl%kind_value > 0 .and. &
                decl%type_name /= "character") then
                block
                    character(len=64) :: buffer
                    write (buffer, '(A,"(",I0,")")') trim(decl%type_name), &
                        decl%kind_value
                    func_def%return_type = trim(buffer)
                end block
            else
                func_def%return_type = trim(decl%type_name)
            end if
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
            allocate (func_def%body_indices(1))
            func_def%body_indices(1) = arena%size
        else
            ! Insert as the second statement (after implicit none which we added earlier)
            allocate (new_body_indices(size(func_def%body_indices) + 1))
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

    logical function is_character_length_decl(type_name) result(is_match)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(type_name)
        if (len_trim(lowered) == 0) then
            is_match = .false.
            return
        end if
        if (.not. is_character_type_string(type_name)) then
            is_match = .false.
            return
        end if
        is_match = (index(lowered, 'len=') > 0) .and. &
                   (index(lowered, 'len=*') == 0) .and. &
                   (index(lowered, 'len=:') == 0)
    end function is_character_length_decl

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
        character(len=64), allocatable :: fn_param_type(:)
        logical, allocatable :: fn_param_has_kind(:)
        integer, allocatable :: fn_param_kind_value(:)
        logical, allocatable :: fn_param_is_array(:)
        logical, allocatable :: fn_param_is_allocatable(:)
        logical, allocatable :: fn_param_type_inferred(:)

        integer :: i, j, n_params, n_body
        character(len=64), allocatable :: param_names(:)
        logical :: standardizer_type_standardization_enabled
        logical :: requires_intent_in

        if (.not. allocated(func_def%param_indices)) return
        n_params = size(func_def%param_indices)
        if (n_params == 0) return

   call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        ! Get parameter names
        allocate (param_names(n_params))
        allocate (param_names_found(n_params))
        allocate (fn_param_optional(n_params))
        allocate (fn_param_intent(n_params))
        allocate (fn_param_type(n_params))
        allocate (fn_param_has_kind(n_params))
        allocate (fn_param_kind_value(n_params))
        allocate (fn_param_is_array(n_params))
        allocate (fn_param_is_allocatable(n_params))
        allocate (fn_param_type_inferred(n_params))
        param_names_found = 0
        fn_param_optional = .false.
        fn_param_intent = ""
        fn_param_type = ""
        fn_param_has_kind = .false.
        fn_param_kind_value = 0
        fn_param_is_array = .false.
        fn_param_is_allocatable = .false.
        fn_param_type_inferred = .true.

        requires_intent_in = .false.
        if (allocated(func_def%prefix_keywords)) then
            do i = 1, size(func_def%prefix_keywords)
                select case (trim(func_def%prefix_keywords(i)))
                case ("pure", "elemental")
                    requires_intent_in = .true.
                end select
            end do
        end if
        if (requires_intent_in) fn_param_intent = "in"
        ! Initialize all param_names to avoid undefined behavior
        do i = 1, n_params
            param_names(i) = ""
        end do

        do i = 1, n_params
            if (func_def%param_indices(i) > 0 .and. func_def%param_indices(i) <= &
                arena%size) then
                if (allocated(arena%entries(func_def%param_indices(i))%node)) then
                    select type (param => &
                                 arena%entries(func_def%param_indices(i))%node)
                    type is (identifier_node)
                        param_names(i) = param%name
                        call apply_function_type(i, .false., "", .false., 0, &
                                                 param%inferred_type%kind > 0, &
                                                 param%inferred_type%to_string(), &
                                                 .false., .false.)
                    type is (parameter_declaration_node)
                        param_names(i) = param%name
                        fn_param_optional(i) = param%is_optional
                        if (allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0) then
                            call apply_function_type(i, .true., &
                                                     trim(param%type_name), &
                                                     param%has_kind, &
                                                     param%kind_value, &
                                                     param%inferred_type%kind > 0, &
                                                     param%inferred_type%to_string(), &
                                                     param%is_array, .false.)
                        else
                            call apply_function_type(i, .false., "", param%has_kind, &
                                                     param%kind_value, &
                                                     param%inferred_type%kind > 0, &
                                                     param%inferred_type%to_string(), &
                                                     param%is_array, .false.)
                        end if
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
                        if (allocated(param%type_name) .and. &
                            len_trim(param%type_name) > 0) then
                            call apply_function_type(i, .true., &
                                                     trim(param%type_name), &
                                                     param%has_kind, &
                                                     param%kind_value, &
                                                     param%inferred_type%kind > 0, &
                                                     param%inferred_type%to_string(), &
                                                     param%is_array, &
                                                     param%is_allocatable)
                        else
                            call apply_function_type(i, .false., "", param%has_kind, &
                                                     param%kind_value, &
                                                     param%inferred_type%kind > 0, &
                                                     param%inferred_type%to_string(), &
                                                     param%is_array, &
                                                     param%is_allocatable)
                        end if
                        if (param%has_intent .and. allocated(param%intent)) then
                            fn_param_intent(i) = param%intent
                        end if
                    class default
                        ! Try to get a reasonable default name
                        write (param_names(i), '(a,i0)') "param", i
                    end select
                else
                    ! Node not allocated, create default name
                    write (param_names(i), '(a,i0)') "param", i
                end if
            else
                ! Invalid index, create default name
                write (param_names(i), '(a,i0)') "param", i
            end if
        end do

        if (allocated(func_def%body_indices)) then
            call synchronize_parameter_declarations(arena, func_def%body_indices, &
                                                    param_names, param_names_found, &
                                                    fn_param_optional, &
                                                    fn_param_intent, &
                                                    "in", &
                                            standardizer_type_standardization_enabled, &
                                                    param_type=fn_param_type, &
                                           param_type_inferred=fn_param_type_inferred, &
                                                    param_has_kind=fn_param_has_kind, &
                                                 param_kind_value=fn_param_kind_value, &
                                                    param_is_array=fn_param_is_array, &
                                           param_is_allocatable=fn_param_is_allocatable)
        end if

        ! Add declarations for parameters not found
        call add_missing_parameter_declarations_ext(arena, func_def, func_index, &
                                                    param_names, param_names_found, &
                                                    n_params, fn_param_optional, &
                                                    fn_param_intent, fn_param_type, &
                                                    fn_param_has_kind, &
                                                    fn_param_kind_value, &
                                                    fn_param_is_array, &
                                                    fn_param_is_allocatable, &
                                                    fn_param_type_inferred, &
                                              standardizer_type_standardization_enabled)

        if (requires_intent_in) then
            call rebuild_parameter_declarations(arena, func_def, func_index, &
                                                param_names, &
                                                fn_param_optional, fn_param_type, &
                                                fn_param_has_kind, &
                                                fn_param_kind_value, &
                                                fn_param_is_array, &
                                                fn_param_is_allocatable, &
                                                fn_param_type_inferred, &
                                              standardizer_type_standardization_enabled)
        end if

        if (allocated(func_def%param_intents)) deallocate (func_def%param_intents)
        if (n_params > 0) then
            allocate (character(len=8) :: func_def%param_intents(n_params))
            func_def%param_intents = fn_param_intent
        end if

    contains
        subroutine apply_function_type(idx, type_present, type_text, has_kind_flag, &
                                       kind_value, inferred_present, inferred_text, &
                                       is_array_flag, is_alloc_flag)
            integer, intent(in) :: idx
            logical, intent(in) :: type_present
            character(len=*), intent(in) :: type_text
            logical, intent(in) :: has_kind_flag
            integer, intent(in) :: kind_value
            logical, intent(in) :: inferred_present
            character(len=*), intent(in) :: inferred_text
            logical, intent(in) :: is_array_flag
            logical, intent(in) :: is_alloc_flag

            ! Check for valid types (not empty and not type_variable)
            if (type_present .and. len_trim(type_text) > 0 .and. &
                trim(type_text) /= "type_variable") then
                fn_param_type(idx) = trim(type_text)
                fn_param_type_inferred(idx) = .false.
            else if (inferred_present .and. len_trim(inferred_text) > 0 .and. &
                     trim(inferred_text) /= "type_variable") then
                fn_param_type(idx) = trim(inferred_text)
                fn_param_type_inferred(idx) = .false.
            end if

            fn_param_has_kind(idx) = has_kind_flag
            if (has_kind_flag) then
                fn_param_kind_value(idx) = kind_value
            else
                fn_param_kind_value(idx) = 0
            end if

            fn_param_is_array(idx) = is_array_flag
            fn_param_is_allocatable(idx) = is_alloc_flag
        end subroutine apply_function_type
    end subroutine standardize_function_parameters

    ! Add missing parameter declarations
    subroutine add_missing_parameter_declarations_ext(arena, func_def, func_index, &
                                                      param_names, param_names_found, &
                                                      n_params, param_optional, &
                                                      param_intent, &
                                                      param_type, param_has_kind, &
                                                      param_kind_value, &
                                                      param_is_array, &
                                                      param_is_allocatable, &
                                                      param_type_inferred, &
                                                      type_std_enabled)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index, n_params
        character(len=64), intent(in) :: param_names(:)
        integer, intent(in) :: param_names_found(:)
        logical, intent(in) :: param_optional(:)
        character(len=8), intent(in) :: param_intent(:)
        character(len=64), intent(in) :: param_type(:)
        logical, intent(in) :: param_has_kind(:)
        integer, intent(in) :: param_kind_value(:)
        logical, intent(in) :: param_is_array(:)
        logical, intent(in) :: param_is_allocatable(:)
        logical, intent(in) :: param_type_inferred(:)
        logical, intent(in) :: type_std_enabled
        type(declaration_node) :: param_decl
        integer, allocatable :: new_body_indices(:)
        integer :: i, j, n_body, new_decl_count
        logical :: inferred_local

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
                    param_decl%type_name = ""
                    param_decl%has_kind = .false.
                    param_decl%kind_value = 0
                    param_decl%is_array = .false.
                    param_decl%is_allocatable = .false.
                    inferred_local = param_type_inferred(i)
                    ! Check if we have a valid type (not empty and not type_variable)
                    if (len_trim(param_type(i)) > 0 .and. &
                        trim(param_type(i)) /= "type_variable") then
                        param_decl%type_name = trim(param_type(i))
                        param_decl%has_kind = param_has_kind(i)
                        param_decl%kind_value = param_kind_value(i)
                    else
                        block
                            character(len=32) :: inferred_type
                            call infer_parameter_type(param_names(i), inferred_type, &
                                                      param_decl%has_kind, &
                                                      param_decl%kind_value)
                            param_decl%type_name = trim(inferred_type)
                        end block
                        inferred_local = .true.
                    end if
                    if (param_decl%type_name == "real") then
                        if (type_std_enabled .and. inferred_local) then
                            param_decl%has_kind = .true.
                            param_decl%kind_value = 8
                        end if
                    end if
                    param_decl%is_array = param_is_array(i)
                    param_decl%is_allocatable = param_is_allocatable(i)
                    param_decl%var_name = param_names(i)
                    param_decl%is_multi_declaration = .false.
                    if (allocated(param_decl%var_names)) deallocate &
                        (param_decl%var_names)
                    if (len_trim(param_intent(i)) > 0) then
                        param_decl%intent = param_intent(i)
                        param_decl%has_intent = .true.
                    else
                        param_decl%intent = "in"
                        param_decl%has_intent = .true.
                    end if
                    param_decl%is_optional = param_optional(i)
                    param_decl%initializer_index = 0
                    param_decl%line = 1
                    param_decl%column = 1

                    call arena%push(param_decl, "declaration", func_index)
                    new_decl_count = new_decl_count + 1

                    ! Insert after implicit none, shift other statements down
                    ! Move all statements from position 2 onwards down by one
                    do j = n_body + new_decl_count, 3, -1
                        new_body_indices(j) = new_body_indices(j - 1)
                    end do
                    new_body_indices(1 + new_decl_count) = arena%size
                end if
            end do

            func_def%body_indices = new_body_indices
        end if

    end subroutine add_missing_parameter_declarations_ext

    subroutine rebuild_parameter_declarations(arena, func_def, func_index, &
                                              param_names, &
                                              param_optional, param_type, &
                                              param_has_kind, param_kind_value, &
                                              param_is_array, &
                                              param_is_allocatable, &
                                              param_type_inferred, &
                                              type_std_enabled)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(inout) :: func_def
        integer, intent(in) :: func_index
        character(len=64), intent(in) :: param_names(:)
        logical, intent(in) :: param_optional(:)
        character(len=64), intent(in) :: param_type(:)
        logical, intent(in) :: param_has_kind(:)
        integer, intent(in) :: param_kind_value(:)
        logical, intent(in) :: param_is_array(:)
        logical, intent(in) :: param_is_allocatable(:)
        logical, intent(in) :: param_type_inferred(:)
        logical, intent(in) :: type_std_enabled
        integer :: i, n_params, idx, pname_idx, name_idx
        type(declaration_node) :: param_decl
        integer, allocatable :: new_indices(:)
        character(len=32) :: inferred_type
        logical :: is_param
        integer, allocatable :: existing(:)
        logical :: inferred_local

        if (.not. allocated(func_def%body_indices)) return
        n_params = size(param_names)
        if (n_params == 0) return

        ! Remove existing parameter declarations
        do i = 1, size(func_def%body_indices)
            idx = func_def%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (declaration_node)
                is_param = .false.
                if (trim(stmt%var_name) /= "") then
                    do pname_idx = 1, n_params
                        if (trim(stmt%var_name) == trim(param_names(pname_idx))) then
                            is_param = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. is_param) then
                    if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                        do pname_idx = 1, n_params
                            do name_idx = 1, size(stmt%var_names)
                                if (trim(stmt%var_names(name_idx)) == &
                                    trim(param_names(pname_idx))) then
                                    is_param = .true.
                                    exit
                                end if
                            end do
                            if (is_param) exit
                        end do
                    end if
                end if
                if (is_param) func_def%body_indices(i) = 0
            type is (parameter_declaration_node)
                do pname_idx = 1, n_params
                    if (trim(stmt%name) == trim(param_names(pname_idx))) then
                        func_def%body_indices(i) = 0
                        exit
                    end if
                end do
            end select
        end do

        ! Create new declarations for each parameter
        allocate (new_indices(n_params))
        do i = 1, n_params
            param_decl%type_name = ""
            param_decl%has_kind = .false.
            param_decl%kind_value = 0
            param_decl%is_array = .false.
            param_decl%is_allocatable = .false.
            inferred_local = param_type_inferred(i)
            ! Check if we have a valid type (not empty and not type_variable)
            if (len_trim(param_type(i)) > 0 .and. &
                trim(param_type(i)) /= "type_variable") then
                param_decl%type_name = trim(param_type(i))
                param_decl%has_kind = param_has_kind(i)
                param_decl%kind_value = param_kind_value(i)
            else
                call infer_parameter_type(param_names(i), inferred_type, &
                                          param_decl%has_kind, param_decl%kind_value)
                param_decl%type_name = trim(inferred_type)
                inferred_local = .true.
            end if
            if (param_decl%type_name == "real") then
                if (type_std_enabled .and. inferred_local) then
                    param_decl%has_kind = .true.
                    param_decl%kind_value = 8
                end if
            end if
            param_decl%var_name = trim(param_names(i))
            param_decl%is_multi_declaration = .false.
            if (allocated(param_decl%var_names)) deallocate (param_decl%var_names)
            param_decl%intent = "in"
            param_decl%has_intent = .true.
            param_decl%is_optional = param_optional(i)
            param_decl%is_array = param_is_array(i)
            param_decl%is_allocatable = param_is_allocatable(i)
            param_decl%initializer_index = 0
            param_decl%line = 1
            param_decl%column = 1
            call arena%push(param_decl, "declaration", func_index)
            new_indices(i) = arena%size
        end do

        ! Collect existing non-removed statements
        allocate (existing(0))
        do i = 1, size(func_def%body_indices)
            if (func_def%body_indices(i) /= 0) then
                existing = [existing, func_def%body_indices(i)]
            end if
        end do

        if (size(existing) == 0) then
            if (allocated(func_def%body_indices)) deallocate (func_def%body_indices)
            allocate (func_def%body_indices(n_params))
            func_def%body_indices = new_indices
        else
            if (allocated(func_def%body_indices)) deallocate (func_def%body_indices)
            allocate (func_def%body_indices(size(existing) + n_params))
            if (size(existing) >= 1) then
                func_def%body_indices(1) = existing(1)
                do i = 1, n_params
                    func_def%body_indices(1 + i) = new_indices(i)
                end do
                do i = 2, size(existing)
                    func_def%body_indices(n_params + i) = existing(i)
                end do
            else
                do i = 1, n_params
                    func_def%body_indices(i) = new_indices(i)
                end do
            end if
        end if

    end subroutine rebuild_parameter_declarations

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
                                                          line=1, column=1, &
                                                          parent_index=sub_index)

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
        integer :: i, j, n_params, n_body
        character(len=64), allocatable :: param_names(:)
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
            if (sub_def%param_indices(i) > 0 .and. sub_def%param_indices(i) <= &
                arena%size) then
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
                        if (param%has_intent .and. allocated(param%intent)) then
                            sb_param_intent(i) = param%intent
                        end if
                    class default
                        ! Try to get a reasonable default name
                        write (param_names(i), '(a,i0)') "param", i
                    end select
                else
                    ! Node not allocated, create default name
                    write (param_names(i), '(a,i0)') "param", i
                end if
            else
                ! Invalid index, create default name
                write (param_names(i), '(a,i0)') "param", i
            end if
        end do

        if (allocated(sub_def%body_indices)) then
            call synchronize_parameter_declarations(arena, sub_def%body_indices, &
                                                    param_names, param_names_found, &
                                                    sb_param_optional, &
                                                    sb_param_intent, &
                                                    "", &
                                              standardizer_type_standardization_enabled)
        end if

        ! Add declarations for parameters not found
        call add_missing_subroutine_parameter_declarations_ext(arena, sub_def, &
                                                               sub_index, &
                                                               param_names, &
                                                               param_names_found, &
                                                               n_params, &
                                                               sb_param_optional, &
                                                               sb_param_intent, &
                                              standardizer_type_standardization_enabled)

        if (allocated(sub_def%param_intents)) deallocate (sub_def%param_intents)
        if (n_params > 0) then
            allocate (character(len=8) :: sub_def%param_intents(n_params))
            sub_def%param_intents = sb_param_intent
        end if

    end subroutine standardize_subroutine_parameters

    ! Add missing subroutine parameter declarations
    subroutine add_missing_subroutine_parameter_declarations_ext(arena, sub_def, &
                                                                 sub_index, &
                                                                 param_names, &
                                                                 param_names_found, &
                                                                 n_params, &
                                                                 param_optional, &
                                                                 param_intent, &
                                                                 type_std_enabled)
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
                                              param_decl%has_kind, &
                                              param_decl%kind_value)
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
                            new_body_indices(j) = new_body_indices(j - 1)
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
            case ('x', 'y', 'z', 'r', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', &
                  'o', 'p', &
                  'q', 's', 't', 'u', 'v', 'w')
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
