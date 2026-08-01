module parser_statement_placement_module
    ! Token-level validation of statement placement (issue #2896).
    !
    ! Fortran confines several statements to one kind of program section:
    ! BLOCK DATA admits only a small subset of specification statements,
    ! an interface body admits no statement function, ENTRY, DATA or FORMAT
    ! statement, an INTERFACE statement may not open inside another interface
    ! block, a statement function may not appear in a module specification
    ! part, IMPLICIT may not follow a data declaration, and a declaration may
    ! not follow the executable part.
    !
    ! The check runs on the raw token stream, before the recursive-descent
    ! parser drops the misplaced statements, because that is the earliest
    ! layer that still knows the source order of every statement.
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, &
        TK_NUMBER, TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_construct_terminators_module, only: &
        placement_diagnostic => diagnostic
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    implicit none
    private

    public :: validate_statement_placement
    public :: set_statement_placement_input_mode

    integer, parameter :: SCOPE_MAX_KIND_LEN = 16

    type :: placement_scope_t
        character(len=SCOPE_MAX_KIND_LEN) :: kind = ""
        logical :: seen_declaration = .false.
        logical :: seen_executable = .false.
        logical :: in_interface_body = .false.
        logical :: in_contains = .false.
    end type placement_scope_t

    ! Lazy Fortran allows declarations to follow executable statements, so the
    ! ordering rules only apply once the caller says the source is standard
    ! Fortran. The default stays lazy so that an unset caller never rejects.
    integer, save :: placement_input_mode = INPUT_MODE_LAZY

contains

    ! Token walking. Statements are delimited by newline tokens and
    ! semicolons: the lexer joins continuation lines by dropping the
    ! ampersand together with its newline, so line numbers no longer delimit
    ! a statement while newline tokens still do.
    integer function placement_next_significant(tokens, start) result(k)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start

        integer :: m

        k = start
        do while (k >= 1 .and. k <= size(tokens))
            if (placement_is_skippable(tokens(k))) then
                k = k + 1
                cycle
            end if
            if (tokens(k)%kind == TK_NEWLINE) then
                m = k + 1
                do while (m <= size(tokens))
                    if (.not. placement_is_skippable(tokens(m))) exit
                    m = m + 1
                end do
                if (m > size(tokens)) return
                ! A fixed-form continuation marker in column 6 carries the
                ! statement across the line break.
                if (.not. is_fixed_form_marker(tokens, m)) return
                k = m + 1
                cycle
            end if
            return
        end do
        k = 0
    end function placement_next_significant

    logical function is_fixed_form_marker(tokens, k) result(is_marker)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: k

        is_marker = .false.
        if (k < 1 .or. k > size(tokens)) return
        if (tokens(k)%column /= 6) return
        if (tokens(k)%kind /= TK_OPERATOR .and. tokens(k)%kind /= TK_NUMBER) return
        is_marker = .true.
    end function is_fixed_form_marker

    integer function placement_previous_significant(tokens, start) result(k)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start

        k = start
        do while (k >= 1)
            if (.not. placement_is_skippable(tokens(k))) return
            k = k - 1
        end do
        k = 0
    end function placement_previous_significant

    ! Whitespace, comments and a surviving continuation ampersand carry no
    ! statement structure.
    logical function placement_is_skippable(token) result(skippable)
        type(token_t), intent(in) :: token

        skippable = token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT
        if (skippable) return
        if (token%kind /= TK_OPERATOR) return
        if (.not. allocated(token%text)) return
        skippable = trim(token%text) == "&"
    end function placement_is_skippable

    logical function placement_statement_continues(tokens, prev, k) result(continues)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: prev
        integer, intent(in) :: k

        integer :: unused

        continues = .false.
        if (k <= 0) return
        if (tokens(k)%kind == TK_EOF) return
        if (tokens(k)%kind == TK_NEWLINE) return
        if (tokens(k)%kind == TK_OPERATOR) then
            if (trim(tokens(k)%text) == ";") return
        end if
        unused = prev
        continues = .true.
    end function placement_statement_continues

    logical function previous_token_is_newline(tokens, i) result(is_newline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i

        integer :: prev

        is_newline = .false.
        prev = i - 1
        do while (prev >= 1)
            if (.not. placement_is_skippable(tokens(prev))) exit
            prev = prev - 1
        end do
        if (prev < 1) return
        is_newline = tokens(prev)%kind == TK_NEWLINE
    end function previous_token_is_newline

    logical function placement_is_statement_start(tokens, i) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i

        integer :: prev

        is_start = .true.
        prev = placement_previous_significant(tokens, i - 1)
        if (prev == 0) return
        if (tokens(prev)%kind == TK_NEWLINE) return
        if (tokens(prev)%kind == TK_OPERATOR) then
            if (trim(tokens(prev)%text) == ";") return
        end if
        is_start = .false.
    end function placement_is_statement_start

    ! A numeric label or a construct name may precede the statement keyword.
    integer function skip_statement_label(tokens, i) result(j)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i

        integer :: colon, after

        j = i
        if (tokens(i)%kind == TK_NUMBER) then
            after = placement_next_significant(tokens, i + 1)
            if (.not. placement_statement_continues(tokens, i, after)) return
            j = after
            return
        end if
        if (tokens(i)%kind /= TK_IDENTIFIER) return
        colon = placement_next_significant(tokens, i + 1)
        if (colon == 0) return
        if (tokens(colon)%kind /= TK_OPERATOR) return
        if (trim(tokens(colon)%text) /= ":") return
        after = placement_next_significant(tokens, colon + 1)
        if (.not. placement_statement_continues(tokens, colon, after)) return
        j = after
    end function skip_statement_label

    subroutine set_statement_placement_input_mode(mode)
        integer, intent(in) :: mode

        placement_input_mode = mode
    end subroutine set_statement_placement_input_mode

    subroutine validate_statement_placement(tokens, error_msg)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable, intent(out) :: error_msg

        type(placement_scope_t), allocatable :: stack(:)
        integer :: depth
        integer :: i, j
        character(len=:), allocatable :: lowered

        error_msg = ""
        allocate (stack(0))
        depth = 0

        i = 1
        do while (i <= size(tokens))
            if (placement_is_skippable(tokens(i)) .or. &
                tokens(i)%kind == TK_NEWLINE) then
                i = i + 1
                cycle
            end if
            if (previous_token_is_newline(tokens, i)) then
                if (is_fixed_form_marker(tokens, i)) then
                    i = i + 1
                    cycle
                end if
            end if
            if (tokens(i)%kind == TK_EOF) exit
            if (.not. placement_is_statement_start(tokens, i)) then
                i = i + 1
                cycle
            end if

            j = skip_statement_label(tokens, i)
            if (tokens(j)%kind /= TK_IDENTIFIER .and. &
                tokens(j)%kind /= TK_KEYWORD) then
                ! Not a statement keyword or name: a continuation artefact or
                ! a form this validator does not classify.
                i = i + 1
                cycle
            end if
            lowered = to_lower(trim(tokens(j)%text))
            call handle_statement(tokens, j, lowered, stack, depth, error_msg)
            if (len(error_msg) > 0) return

            i = i + 1
        end do
    end subroutine validate_statement_placement

    subroutine handle_statement(tokens, j, lowered, stack, depth, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: lowered
        type(placement_scope_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        character(len=:), allocatable, intent(inout) :: error_msg

        character(len=:), allocatable :: word

        word = trim(lowered)

        if (is_end_statement(tokens, j, word)) then
            call pop_scope(stack, depth)
            return
        end if

        ! A name that happens to spell a keyword is still a variable when the
        ! statement assigns to it, and an assignment is executable.
        if (is_assignment_statement(tokens, j)) then
            if (.not. section_forbids_assignment(stack, depth)) then
                if (depth > 0) stack(depth)%seen_executable = .true.
                return
            end if
        end if

        call check_statement_in_scope(tokens, j, word, stack, depth, error_msg)
        if (len(error_msg) > 0) return

        call push_or_mark(tokens, j, word, stack, depth)
    end subroutine handle_statement

    ! Rejection rules. Each rule names the section that forbids the statement.
    subroutine check_statement_in_scope(tokens, j, word, stack, depth, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word
        type(placement_scope_t), allocatable, intent(in) :: stack(:)
        integer, intent(in) :: depth
        character(len=:), allocatable, intent(inout) :: error_msg

        character(len=:), allocatable :: scope_kind
        logical :: statement_function

        scope_kind = current_kind(stack, depth)

        ! A PROGRAM or BLOCK DATA statement opens a program unit and therefore
        ! cannot appear inside another one.
        if (word == "program") then
            if (depth > 0) then
                error_msg = placement_diagnostic( &
                    "Unexpected PROGRAM statement", tokens(j))
                return
            end if
        end if
        if (starts_block_data(tokens, j, word)) then
            if (depth > 0) then
                error_msg = placement_diagnostic( &
                    "Unexpected BLOCK DATA statement", tokens(j))
                return
            end if
        end if

        ! SEQUENCE belongs to a derived-type definition, ELSE to an IF
        ! construct.
        if (word == "sequence" .and. depth > 0) then
            if (scope_kind /= "type") then
                error_msg = placement_diagnostic( &
                    "Unexpected SEQUENCE statement", tokens(j))
                return
            end if
        end if
        if (word == "else" .or. word == "elseif") then
            if (scope_kind /= "if") then
                error_msg = placement_diagnostic( &
                    "Unexpected ELSE statement", tokens(j))
                return
            end if
        end if

        statement_function = is_statement_function(tokens, j)

        ! F2018 C1116: BLOCK DATA holds only specification statements.
        if (scope_kind == "blockdata") then
            if (statement_function) then
                error_msg = placement_diagnostic( &
                    "STATEMENT FUNCTION statement is not allowed inside of "// &
                    "BLOCK DATA", tokens(j))
                return
            end if
            if (word == "interface") then
                error_msg = placement_diagnostic( &
                    "INTERFACE statement is not allowed inside of BLOCK DATA", &
                    tokens(j))
                return
            end if
            if (word == "format") then
                error_msg = placement_diagnostic( &
                    "FORMAT statement is not allowed inside of BLOCK DATA", &
                    tokens(j))
                return
            end if
        end if

        ! An interface block holds interface bodies only, never another
        ! INTERFACE statement.
        if (scope_kind == "interface" .and. word == "interface") then
            error_msg = placement_diagnostic("Unexpected INTERFACE statement", &
                tokens(j))
            return
        end if

        ! F2018 C1519: an interface body has no executable part, so a
        ! statement function, ENTRY, DATA or FORMAT statement cannot appear
        ! within it.
        if (depth > 0) then
            if (stack(depth)%in_interface_body) then
                if (statement_function) then
                    error_msg = placement_diagnostic( &
                        "Statement function cannot appear within an "// &
                        "INTERFACE body", tokens(j))
                    return
                end if
                if (word == "entry" .or. word == "data" .or. word == "format") &
                    then
                    error_msg = placement_diagnostic( &
                        upper_case(word)//" statement cannot appear within an "// &
                        "INTERFACE body", tokens(j))
                    return
                end if
            end if
        end if

        ! A module specification part has no executable part either.
        if (scope_kind == "module" .or. scope_kind == "submodule") then
            if (statement_function) then
                error_msg = placement_diagnostic( &
                    "Unexpected STATEMENT FUNCTION statement", tokens(j))
                return
            end if
        end if

        ! F2018 C1102: IMPLICIT must precede every data declaration statement
        ! of its scoping unit.
        if (word == "implicit" .and. depth > 0) then
            if (stack(depth)%seen_declaration) then
                error_msg = placement_diagnostic( &
                    "IMPLICIT statement at (1) cannot follow data "// &
                    "declaration statement at (2)", tokens(j))
                return
            end if
        end if

        if (placement_input_mode /= INPUT_MODE_STANDARD) return
        if (depth <= 0) return
        if (.not. stack(depth)%seen_executable) return

        if (starts_derived_type_definition(tokens, j, word)) then
            error_msg = placement_diagnostic( &
                "Unexpected derived type declaration", tokens(j))
            return
        end if
        if (is_type_declaration(tokens, j, word)) then
            error_msg = placement_diagnostic( &
                "data declaration statement at (1) cannot appear after "// &
                "executable statements", tokens(j))
            return
        end if
    end subroutine check_statement_in_scope

    ! Scope bookkeeping: opens a new scope for every construct that has its own
    ! END statement, and records what the current scope has seen so far.
    subroutine push_or_mark(tokens, j, word, stack, depth)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word
        type(placement_scope_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth

        logical :: parent_is_interface

        if (word == "contains") then
            if (depth > 0) then
                stack(depth)%seen_executable = .false.
                stack(depth)%seen_declaration = .false.
                stack(depth)%in_contains = .true.
            end if
            return
        end if

        ! A separate module procedure after CONTAINS opens a procedure body.
        if (opens_separate_module_procedure(tokens, j, word, stack, depth)) then
            call push_scope(stack, depth, "procedure", .false.)
            return
        end if

        if (starts_block_data(tokens, j, word)) then
            call push_scope(stack, depth, "blockdata", .false.)
            return
        end if
        if (word == "program" .or. word == "module" .or. word == "submodule") then
            if (opens_program_unit(tokens, j, word)) then
                call push_scope(stack, depth, word, .false.)
                return
            end if
        end if
        if (word == "interface" .or. word == "abstract") then
            if (opens_interface_block(tokens, j, word)) then
                call push_scope(stack, depth, "interface", .false.)
                return
            end if
        end if
        if (starts_procedure_definition(tokens, j)) then
            parent_is_interface = .false.
            if (depth > 0) parent_is_interface = &
                trim(stack(depth)%kind) == "interface"
            call push_scope(stack, depth, "procedure", parent_is_interface)
            return
        end if
        if (starts_derived_type_definition(tokens, j, word)) then
            call push_scope(stack, depth, "type", .false.)
            return
        end if
        if (opens_if_construct(tokens, j, word)) then
            call push_scope(stack, depth, "if", .false.)
            return
        end if
        if (opens_named_construct(tokens, j, word)) then
            call push_scope(stack, depth, "construct", .false.)
            return
        end if

        if (depth <= 0) return
        if (is_type_declaration(tokens, j, word)) then
            stack(depth)%seen_declaration = .true.
            return
        end if
        if (is_executable_statement(word)) stack(depth)%seen_executable = .true.
    end subroutine push_or_mark

    subroutine push_scope(stack, depth, kind, in_interface_body)
        type(placement_scope_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        character(len=*), intent(in) :: kind
        logical, intent(in) :: in_interface_body

        type(placement_scope_t), allocatable :: grown(:)
        type(placement_scope_t) :: entry

        entry%kind = kind
        entry%in_interface_body = in_interface_body
        allocate (grown(depth + 1))
        if (depth > 0) grown(1:depth) = stack(1:depth)
        grown(depth + 1) = entry
        call move_alloc(grown, stack)
        depth = depth + 1
    end subroutine push_scope

    subroutine pop_scope(stack, depth)
        type(placement_scope_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth

        if (.not. allocated(stack)) return
        if (depth <= 0) return
        depth = depth - 1
    end subroutine pop_scope

    function current_kind(stack, depth) result(kind)
        type(placement_scope_t), allocatable, intent(in) :: stack(:)
        integer, intent(in) :: depth
        character(len=:), allocatable :: kind

        kind = ""
        if (depth <= 0) return
        if (.not. allocated(stack)) return
        kind = trim(stack(depth)%kind)
    end function current_kind

    ! Statement recognisers.

    logical function is_end_statement(tokens, j, word) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        is_end = .false.
        if (word == "end") then
            is_end = .true.
            return
        end if
        if (len(word) <= 3) return
        if (word(1:3) /= "end") return
        select case (word(4:))
        case ("if", "do", "select", "where", "forall", "associate", "block", &
                "critical", "type", "interface", "function", "subroutine", &
                "module", "submodule", "program", "enum", "blockdata", "team")
            is_end = .true.
        end select
    end function is_end_statement

    logical function starts_block_data(tokens, j, word) result(is_block_data)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k

        is_block_data = .false.
        if (word == "blockdata") then
            is_block_data = .true.
            return
        end if
        if (word /= "block") return
        k = placement_next_significant(tokens, j + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, j, k)) return
        is_block_data = to_lower(trim(tokens(k)%text)) == "data"
    end function starts_block_data

    logical function opens_program_unit(tokens, j, word) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k
        character(len=:), allocatable :: next_word

        opens = .false.
        k = placement_next_significant(tokens, j + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, j, k)) return
        if (tokens(k)%kind /= TK_IDENTIFIER .and. tokens(k)%kind /= TK_KEYWORD) &
            return
        next_word = to_lower(trim(tokens(k)%text))
        if (word == "module") then
            ! MODULE PROCEDURE, MODULE FUNCTION and MODULE SUBROUTINE are not
            ! module openings.
            if (next_word == "procedure") return
            if (next_word == "function") return
            if (next_word == "subroutine") return
            if (next_word == "pure") return
            if (next_word == "elemental") return
            if (next_word == "impure") return
            if (next_word == "recursive") return
        end if
        opens = .true.
    end function opens_program_unit

    logical function opens_interface_block(tokens, j, word) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k

        opens = .false.
        if (word == "interface") then
            opens = .true.
            return
        end if
        k = placement_next_significant(tokens, j + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, j, k)) return
        opens = to_lower(trim(tokens(k)%text)) == "interface"
    end function opens_interface_block

    logical function starts_derived_type_definition(tokens, j, word) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k
        character(len=:), allocatable :: next_text

        opens = .false.
        if (word /= "type") return
        k = placement_next_significant(tokens, j + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, j, k)) return
        next_text = to_lower(trim(tokens(k)%text))
        ! TYPE(x) declares a variable and TYPE IS selects a class branch.
        if (next_text == "(") return
        if (next_text == "is") return
        opens = .true.
    end function starts_derived_type_definition

    logical function opens_if_construct(tokens, j, word) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        opens = .false.
        if (word /= "if") return
        opens = last_statement_word(tokens, j) == "then"
    end function opens_if_construct

    logical function opens_named_construct(tokens, j, word) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k

        opens = .false.
        select case (word)
        case ("select", "associate", "critical", "enum", "block")
            opens = .true.
        case ("do")
            ! A labelled DO ends at its label, not at an END DO.
            k = placement_next_significant(tokens, j + 1)
            if (k > 0) then
                if (placement_statement_continues(tokens, j, k)) then
                    if (tokens(k)%kind == TK_NUMBER) return
                end if
            end if
            opens = .true.
        case ("where", "forall")
            ! A one-line WHERE or FORALL carries its statement after the mask
            ! and has no END.
            opens = statement_ends_after_paren_group(tokens, j)
        end select
    end function opens_named_construct

    ! True when the parenthesised group that follows the keyword is the whole
    ! statement, as in a WHERE or FORALL construct header.
    logical function statement_ends_after_paren_group(tokens, j) result(ends)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k, prev, paren_depth

        ends = .false.
        prev = j
        k = placement_next_significant(tokens, j + 1)
        if (.not. placement_statement_continues(tokens, prev, k)) return
        if (trim(tokens(k)%text) /= "(") return

        paren_depth = 0
        do while (k > 0)
            if (.not. placement_statement_continues(tokens, prev, k)) return
            if (trim(tokens(k)%text) == "(") paren_depth = paren_depth + 1
            if (trim(tokens(k)%text) == ")") then
                paren_depth = paren_depth - 1
                if (paren_depth == 0) exit
            end if
            prev = k
            k = placement_next_significant(tokens, k + 1)
        end do
        if (k <= 0) return

        prev = k
        k = placement_next_significant(tokens, k + 1)
        ends = .not. placement_statement_continues(tokens, prev, k)
    end function statement_ends_after_paren_group

    logical function starts_procedure_definition(tokens, j) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k, prev, paren_depth
        character(len=:), allocatable :: word

        opens = .false.
        paren_depth = 0
        prev = j
        k = j
        do while (k > 0)
            if (k > j) then
                if (.not. placement_statement_continues(tokens, prev, k)) return
            end if
            word = to_lower(trim(tokens(k)%text))
            if (word == "(") then
                paren_depth = paren_depth + 1
            else if (word == ")") then
                paren_depth = paren_depth - 1
            else if (paren_depth == 0) then
                if (word == "function" .or. word == "subroutine") then
                    opens = .true.
                    return
                end if
                if (.not. is_procedure_prefix_word(word)) return
            end if
            prev = k
            k = placement_next_significant(tokens, k + 1)
        end do
    end function starts_procedure_definition

    logical function is_procedure_prefix_word(word) result(is_prefix)
        character(len=*), intent(in) :: word

        select case (word)
        case ("pure", "impure", "elemental", "recursive", "non_recursive", &
                "module", "real", "integer", "complex", "logical", "character", &
                "double", "precision", "type", "class", "*", ",", "::", "(", ")")
            is_prefix = .true.
        case default
            is_prefix = .false.
        end select
    end function is_procedure_prefix_word

    ! A statement function is an assignment whose target is a name applied to a
    ! parenthesised list of plain names. It is only recognised in sections that
    ! have no executable part, so an ordinary assignment is never mistaken for
    ! one.
    logical function is_statement_function(tokens, j) result(is_stmt_fn)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k, prev, paren_depth
        character(len=:), allocatable :: word

        is_stmt_fn = .false.
        if (tokens(j)%kind /= TK_IDENTIFIER) return
        if (is_specification_statement(to_lower(trim(tokens(j)%text)))) return

        prev = j
        k = placement_next_significant(tokens, j + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, prev, k)) return
        if (trim(tokens(k)%text) /= "(") return

        paren_depth = 0
        do while (k > 0)
            if (.not. placement_statement_continues(tokens, prev, k)) return
            word = trim(tokens(k)%text)
            if (word == "(") then
                paren_depth = paren_depth + 1
            else if (word == ")") then
                paren_depth = paren_depth - 1
                if (paren_depth == 0) exit
            else if (paren_depth == 1) then
                if (tokens(k)%kind /= TK_IDENTIFIER .and. word /= ",") return
            else
                return
            end if
            prev = k
            k = placement_next_significant(tokens, k + 1)
        end do
        if (k <= 0) return

        prev = k
        k = placement_next_significant(tokens, k + 1)
        if (k <= 0) return
        if (.not. placement_statement_continues(tokens, prev, k)) return
        is_stmt_fn = trim(tokens(k)%text) == "="
    end function is_statement_function

    ! True in the sections that have no executable part, where an assignment
    ! form can only be a statement function.
    logical function section_forbids_assignment(stack, depth) result(forbids)
        type(placement_scope_t), allocatable, intent(in) :: stack(:)
        integer, intent(in) :: depth

        character(len=:), allocatable :: kind

        forbids = .false.
        if (depth <= 0) return
        if (stack(depth)%in_interface_body) then
            forbids = .true.
            return
        end if
        kind = trim(stack(depth)%kind)
        forbids = kind == "blockdata" .or. kind == "module" .or. &
            kind == "submodule" .or. kind == "interface" .or. kind == "type"
    end function section_forbids_assignment

    ! An assignment names a variable, optionally with subscripts, component
    ! references or a substring, and then an equals sign. Nothing else may
    ! stand between the name and the equals sign, so a DO or FORALL statement
    ! is never taken for an assignment.
    logical function is_assignment_statement(tokens, j) result(is_assign)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k, prev, paren_depth
        logical :: expect_component
        character(len=:), allocatable :: text

        is_assign = .false.
        if (tokens(j)%kind /= TK_IDENTIFIER .and. tokens(j)%kind /= TK_KEYWORD) &
            return

        paren_depth = 0
        expect_component = .false.
        prev = j
        k = placement_next_significant(tokens, j + 1)
        do while (k > 0)
            if (.not. placement_statement_continues(tokens, prev, k)) return
            text = trim(tokens(k)%text)
            if (paren_depth > 0) then
                if (text == "(") paren_depth = paren_depth + 1
                if (text == ")") paren_depth = paren_depth - 1
            else if (expect_component) then
                if (tokens(k)%kind /= TK_IDENTIFIER .and. &
                    tokens(k)%kind /= TK_KEYWORD) return
                expect_component = .false.
            else if (text == "=") then
                is_assign = .true.
                return
            else if (text == "(") then
                paren_depth = 1
            else if (text == "%") then
                expect_component = .true.
            else
                return
            end if
            prev = k
            k = placement_next_significant(tokens, k + 1)
        end do
    end function is_assignment_statement

    logical function is_type_declaration(tokens, j, word) result(is_decl)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word

        integer :: k

        is_decl = .false.
        select case (word)
        case ("integer", "real", "complex", "logical", "character", "double", &
                "doubleprecision", "doublecomplex", "byte")
            is_decl = .true.
        case ("type", "class")
            k = placement_next_significant(tokens, j + 1)
            if (k <= 0) return
            if (.not. placement_statement_continues(tokens, j, k)) return
            is_decl = trim(tokens(k)%text) == "("
        end select
        if (.not. is_decl) return
        ! A function definition names its result type first; that is a header,
        ! not a declaration.
        if (starts_procedure_definition(tokens, j)) is_decl = .false.
    end function is_type_declaration

    logical function opens_separate_module_procedure(tokens, j, word, stack, &
            depth) result(opens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: word
        type(placement_scope_t), allocatable, intent(in) :: stack(:)
        integer, intent(in) :: depth
        integer :: k
        character(len=:), allocatable :: kind

        opens = .false.
        if (depth <= 0) return
        if (.not. stack(depth)%in_contains) return
        kind = trim(stack(depth)%kind)
        if (kind /= "module" .and. kind /= "submodule") return
        if (word /= "module" .and. word /= "procedure") return
        if (word == "module") then
            k = placement_next_significant(tokens, j + 1)
            if (.not. placement_statement_continues(tokens, j, k)) return
            if (to_lower(trim(tokens(k)%text)) /= "procedure") return
        end if
        opens = .true.
    end function opens_separate_module_procedure

    ! Only a recognised executable statement closes the specification part.
    ! An unrecognised statement leaves the section state alone so that the
    ! ordering rules never fire on a form this validator does not model.
    logical function is_executable_statement(word) result(is_exec)
        character(len=*), intent(in) :: word

        select case (word)
        case ("call", "print", "write", "read", "open", "close", "inquire", &
                "rewind", "backspace", "endfile", "flush", "wait", "if", "do", &
                "goto", "go", "stop", "return", "allocate", "deallocate", &
                "nullify", "cycle", "exit", "select", "where", "forall", &
                "associate", "pause", "continue", "assign", "sync", "lock", &
                "unlock", "critical", "error", "block")
            is_exec = .true.
        case default
            is_exec = .false.
        end select
    end function is_executable_statement

    logical function is_specification_statement(word) result(is_spec)
        character(len=*), intent(in) :: word

        select case (word)
        case ("use", "implicit", "import", "parameter", "common", "data", &
                "equivalence", "dimension", "allocatable", "pointer", "target", &
                "save", "volatile", "asynchronous", "intent", "optional", &
                "external", "intrinsic", "public", "private", "protected", &
                "bind", "namelist", "sequence", "contains", "interface", &
                "abstract", "procedure", "generic", "enumerator", "value", &
                "codimension", "format", "entry", "include", "structure", &
                "record", "union", "map", "automatic", "static", "final", &
                "deferred", "enum", "elseif", "elsewhere", "case", "type", &
                "class", "end")
            is_spec = .true.
        case default
            is_spec = .false.
        end select
    end function is_specification_statement

    function last_statement_word(tokens, j) result(text)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=:), allocatable :: text

        integer :: k, prev

        text = to_lower(trim(tokens(j)%text))
        prev = j
        k = placement_next_significant(tokens, j + 1)
        do while (k > 0)
            if (.not. placement_statement_continues(tokens, prev, k)) exit
            text = to_lower(trim(tokens(k)%text))
            prev = k
            k = placement_next_significant(tokens, k + 1)
        end do
    end function last_statement_word

    function upper_case(word) result(text)
        character(len=*), intent(in) :: word
        character(len=:), allocatable :: text

        integer :: i, code

        text = word
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('a') .and. code <= iachar('z')) then
                text(i:i) = achar(code - 32)
            end if
        end do
    end function upper_case

end module parser_statement_placement_module
