program test_identifier_hash_modular_arithmetic
    use, intrinsic :: iso_fortran_env, only: int32, error_unit
    use identifier_table, only: identifier_table_t, identifier_table_init, &
        identifier_table_intern, identifier_table_find, identifier_table_get, &
        identifier_table_reset
    implicit none
    ! Reference values use arbitrary-precision FNV-1a arithmetic modulo 2**64,
    ! then retain the existing stored low-31-bit hash (zero maps to one).
    character(len=63), parameter :: names(20) = [character(len=63) :: &
        '', 'a', &
        'alpha', 'Alpha', &
        'beta', 'global', &
        'exp', 'log', &
        'abs', '_name', &
        'one', 'two', &
        'three', 'module_name', &
        'procedure_name', 'field_123', &
        'abcdefghijklmnopqrstuvwxyz', 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx', &
        'Case_Sensitive', 'z9_']
    integer(int32), parameter :: hashes(20) = [ &
        1939669891_int32, 1942853894_int32, 2053269605_int32, 187822597_int32, &
        536001145_int32, 1356340344_int32, 534118058_int32, 1298619683_int32, &
        230210093_int32, 1905747599_int32, 1054926593_int32, 1943501963_int32, &
        557818433_int32, 283979219_int32, 229568006_int32, 1723578530_int32, &
        244413452_int32, 1796599689_int32, 1981655400_int32, 2106957563_int32]
    type(identifier_table_t) :: table
    integer(int32) :: id
    integer :: i

    call identifier_table_init(table, 16)
    do i = 1, size(names)
        id = identifier_table_intern(table, trim(names(i)))
        call require(id == i, 'insertion identity')
        call require(table%entries(id)%hash == hashes(i), 'reference FNV hash')
        call require(identifier_table_get(table, id) == trim(names(i)), 'stored name')
    end do
    do i = 1, size(names)
        id = identifier_table_find(table, trim(names(i)))
        call require(id == i, 'lookup after bucket growth')
        id = identifier_table_intern(table, trim(names(i)))
        call require(id == i, 'repeat insertion identity')
    end do
    call require(identifier_table_find(table, 'absent') == 0, 'missing name')
    call identifier_table_reset(table)
    call require(identifier_table_find(table, 'alpha') == 0, 'reset removes binding')
    id = identifier_table_intern(table, 'alpha')
    call require(id == 1, 'reuse after reset')
    call require(table%entries(id)%hash == hashes(3), 'hash after reset')
    print *, 'PASS: overflow-free identifier hashes and interning behavior'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a)') 'FAIL: '//message
        stop 1
    end subroutine require

end program test_identifier_hash_modular_arithmetic
