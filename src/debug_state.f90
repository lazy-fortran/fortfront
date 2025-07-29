module debug_state
    implicit none
    private

    public :: set_debug_flags, get_debug_flags

    ! Global debug state
    logical, save :: debug_tokens_flag = .false.
    logical, save :: debug_ast_flag = .false.
    logical, save :: debug_semantic_flag = .false.
    logical, save :: debug_standardize_flag = .false.
    logical, save :: debug_codegen_flag = .false.

contains

    subroutine set_debug_flags(tokens, ast, semantic, standardize, codegen)
        logical, intent(in) :: tokens, ast, semantic, standardize, codegen
        debug_tokens_flag = tokens
        debug_ast_flag = ast
        debug_semantic_flag = semantic
        debug_standardize_flag = standardize
        debug_codegen_flag = codegen
    end subroutine set_debug_flags

    subroutine get_debug_flags(tokens, ast, semantic, standardize, codegen)
        logical, intent(out) :: tokens, ast, semantic, standardize, codegen
        tokens = debug_tokens_flag
        ast = debug_ast_flag
        semantic = debug_semantic_flag
        standardize = debug_standardize_flag
        codegen = debug_codegen_flag
    end subroutine get_debug_flags

end module debug_state
