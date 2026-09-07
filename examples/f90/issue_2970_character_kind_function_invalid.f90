module character_kind_function_invalid
    interface choose_kind
        procedure provide_kind
    end interface
contains
    subroutine take(text)
        character(kind=choose_kind()) :: text
    end subroutine take
    integer function provide_kind()
        provide_kind = 1
    end function provide_kind
end module character_kind_function_invalid
