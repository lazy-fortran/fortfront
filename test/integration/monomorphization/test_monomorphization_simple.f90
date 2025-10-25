program test_monomorphization_simple
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg

    print *, '=== Monomorphization Phase 1 Test (Future Feature) ==='
    print *, 'This test demonstrates the need for monomorphization.'
    print *, 'See issue #1863 and docs/MONOMORPHIZATION_STATUS.md'
    print *, ''

    input = 'function add(a, b)' // new_line('A') // &
            '    add = a + b' // new_line('A') // &
            'end function' // new_line('A') // &
            '' // new_line('A') // &
            'x = add(5, 3)' // new_line('A') // &
            'y = add(2.5d0, 1.5d0)' // new_line('A')

    call transform_lazy_fortran_string(input, output, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: transformation failed with error:', error_msg
        error stop 1
    end if

    print *, 'Current output (single type inference):'
    print *, output
    print *, ''

    if (index(output, 'add__i32_i32') > 0 .and. &
        index(output, 'add__r64_r64') > 0 .and. &
        index(output, 'interface add') > 0 .and. &
        index(output, 'module procedure') > 0) then
        print *, 'PASS: Monomorphization implemented!'
        print *, 'Generated interface add with module procedures'
        print *, 'Found: add__i32_i32 and add__r64_r64'
    else
        print *, 'EXPECTED: Monomorphization not yet fully implemented'
        print *, 'Infrastructure created:'
        print *, '  - codegen_name_mangling.f90 (name mangling)'
        print *, '  - call_graph_signatures_mod.f90 (signature tracking)'
        print *, 'Remaining work:'
        print *, '  - Integrate signature collection into semantic analysis'
        print *, '  - Modify codegen to generate multiple variants'
        print *, '  - Generate interface blocks for generic dispatch'
        print *, ''
        print *, 'Current behavior: Uses first call site type for all calls'
        print *, 'See docs/MONOMORPHIZATION_STATUS.md for complete roadmap'
    end if

end program test_monomorphization_simple
