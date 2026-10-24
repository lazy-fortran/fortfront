program test_long_enumerator_common_names
    ! ENUM enumerator names and COMMON member names of 32 or more characters
    ! must survive parsing intact. GNU Fortran 12 at -O2 miscompiled the
    ! former string_t(trim(token%text)) constructor used to collect them,
    ! which gave wrong lengths and corrupted the heap for the SUNDIALS
    ! bind(c) enums (fsundials_core_mod, farkode_mod).
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_legacy, only: enum_node, common_block_node
    implicit none

    integer, parameter :: n_enum = 8, n_common = 3
    character(len=*), parameter :: nl = new_line('a')
    character(len=64) :: enum_names(n_enum), common_names(n_common)
    character(:), allocatable :: src
    integer :: i, enum_count, common_count

    enum_names = [character(len=64) :: &
        'SUN_ERR_ARKODE_MRISTEP_INNER_STEPPER_FAILED', &
        'ARK_SPLITTINGSTEP_COEFFICIENTS_INVALID_ORDER', &
        'a', &
        'ARKODE_SPLITTINGSTEP_DEFAULT_COEFFICIENTS_ID', &
        'SUN_OUTPUTFORMAT_TABLE_WITH_VERY_LONG_SUFFIX', &
        'ARKODE_MRI_TABLE_IMEX_MRI_GARK_ERK45a_EXTENDED', &
        'SUNDIALS_ENUMERATOR_NAME_EXACTLY_32', &
        'SUN_ERR_OUT_OF_RANGE_IN_SPLITTINGSTEP_SOLVER']
    common_names = [character(len=64) :: &
        'very_long_common_member_name_number_one_abc', &
        'x', &
        'another_long_common_member_name_for_gfortran12']

    src = 'module long_names_mod' // nl // &
        '    use, intrinsic :: iso_c_binding' // nl // &
        '    implicit none' // nl // &
        '    enum, bind(c)' // nl
    do i = 1, n_enum
        src = src // '        enumerator :: ' // trim(enum_names(i)) // nl
    end do
    src = src // '    end enum' // nl // 'end module long_names_mod' // nl
    call count_nodes(src, enum_count, common_count)
    if (enum_count /= 1 .or. common_count /= 0) then
        print *, 'FAIL: expected one enum, found ', enum_count, common_count
        error stop 1
    end if

    src = 'program long_names_common' // nl // &
        '    implicit none' // nl // &
        '    real :: ' // trim(common_names(1)) // ', ' // &
        trim(common_names(2)) // ', ' // trim(common_names(3)) // nl // &
        '    common /blk/ ' // trim(common_names(1)) // ', ' // &
        trim(common_names(2)) // ', ' // trim(common_names(3)) // nl // &
        'end program long_names_common' // nl
    call count_nodes(src, enum_count, common_count)
    if (enum_count /= 0 .or. common_count /= 1) then
        print *, 'FAIL: expected one common block, found ', enum_count, &
            common_count
        error stop 1
    end if

    print *, 'PASS: long enumerator and common member names preserved'

contains

    subroutine count_nodes(text, enum_count, common_count)
        character(len=*), intent(in) :: text
        integer, intent(out) :: enum_count, common_count
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, k

        arena = create_ast_arena()
        call lex_source(text, tokens, error_msg)
        call fail_on_error(error_msg, 'lex')
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call fail_on_error(error_msg, 'parse')

        enum_count = 0
        common_count = 0
        do k = 1, arena%size
            if (.not. allocated(arena%entries(k)%node)) cycle
            select type (n => arena%entries(k)%node)
            type is (enum_node)
                enum_count = enum_count + 1
                call check_names('enumerator', n%enumerator_names, enum_names)
                if (any(n%enumerator_values /= [(i - 1, i = 1, n_enum)])) then
                    print *, 'FAIL: enumerator values ', n%enumerator_values
                    error stop 1
                end if
            type is (common_block_node)
                common_count = common_count + 1
                call check_names('common member', n%member_names, common_names)
            end select
        end do
    end subroutine count_nodes

    subroutine check_names(what, got, want)
        use string_types, only: string_t
        character(len=*), intent(in) :: what
        type(string_t), allocatable, intent(in) :: got(:)
        character(len=*), intent(in) :: want(:)
        integer :: k

        if (.not. allocated(got)) then
            print *, 'FAIL: no ', what, ' names captured'
            error stop 1
        end if
        if (size(got) /= size(want)) then
            print *, 'FAIL: expected ', size(want), ' ', what, ' names, found ', &
                size(got)
            error stop 1
        end if
        do k = 1, size(want)
            if (.not. allocated(got(k)%s)) then
                print *, 'FAIL: ', what, ' name ', k, ' not allocated'
                error stop 1
            end if
            if (len(got(k)%s) /= len_trim(want(k)) .or. &
                got(k)%s /= trim(want(k))) then
                print *, 'FAIL: ', what, ' name ', k, ' is "', got(k)%s, &
                    '" (len ', len(got(k)%s), '), expected "', trim(want(k)), '"'
                error stop 1
            end if
        end do
    end subroutine check_names

    subroutine fail_on_error(msg, stage)
        character(:), allocatable, intent(in) :: msg
        character(len=*), intent(in) :: stage

        if (allocated(msg)) then
            if (len_trim(msg) > 0) then
                print *, 'FAIL: ', stage, ' error: ', msg
                error stop 1
            end if
        end if
    end subroutine fail_on_error

end program test_long_enumerator_common_names
