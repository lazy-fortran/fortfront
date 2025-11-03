! Issue #1827: submodule containing module subroutine
submodule (parent_module) child_submodule_with_contents
    implicit none
contains
    module subroutine test()
        print *, 'test'
    end subroutine test
end submodule child_submodule_with_contents
