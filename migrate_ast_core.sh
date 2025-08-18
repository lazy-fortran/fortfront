#!/bin/bash

# AST Core Migration Automation Script
# 
# This script helps automate the migration from ast_core to explicit imports
# Usage: ./migrate_ast_core.sh <source_file>

set -e

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <source_file>"
    echo "Example: $0 src/frontend.f90"
    exit 1
fi

SOURCE_FILE="$1"

if [[ ! -f "$SOURCE_FILE" ]]; then
    echo "Error: File $SOURCE_FILE not found"
    exit 1
fi

echo "=== AST Core Migration for $SOURCE_FILE ==="

# Step 1: Analyze what AST components are actually used
echo "Step 1: Analyzing AST usage..."

# Create backup
cp "$SOURCE_FILE" "$SOURCE_FILE.backup"

# Find AST types used
AST_TYPES=$(grep -o '[a-zA-Z_]*_node\|ast_arena_t\|LITERAL_[A-Z]*' "$SOURCE_FILE" | sort | uniq || true)
echo "Found AST types: $AST_TYPES"

# Map types to modules
declare -A TYPE_TO_MODULE
TYPE_TO_MODULE[ast_arena_t]="ast_arena"
TYPE_TO_MODULE[program_node]="ast_nodes_core"
TYPE_TO_MODULE[assignment_node]="ast_nodes_core"
TYPE_TO_MODULE[identifier_node]="ast_nodes_core"
TYPE_TO_MODULE[literal_node]="ast_nodes_core"
TYPE_TO_MODULE[binary_op_node]="ast_nodes_core"
TYPE_TO_MODULE[array_literal_node]="ast_nodes_core"
TYPE_TO_MODULE[call_or_subscript_node]="ast_nodes_core"
TYPE_TO_MODULE[if_node]="ast_nodes_control"
TYPE_TO_MODULE[do_loop_node]="ast_nodes_control"
TYPE_TO_MODULE[do_while_node]="ast_nodes_control"
TYPE_TO_MODULE[forall_node]="ast_nodes_control"
TYPE_TO_MODULE[where_node]="ast_nodes_control"
TYPE_TO_MODULE[select_case_node]="ast_nodes_control"
TYPE_TO_MODULE[function_def_node]="ast_nodes_procedure"
TYPE_TO_MODULE[subroutine_def_node]="ast_nodes_procedure"
TYPE_TO_MODULE[subroutine_call_node]="ast_nodes_procedure"
TYPE_TO_MODULE[module_node]="ast_nodes_data"
TYPE_TO_MODULE[declaration_node]="ast_nodes_data"
TYPE_TO_MODULE[parameter_declaration_node]="ast_nodes_data"
TYPE_TO_MODULE[print_statement_node]="ast_nodes_io"
TYPE_TO_MODULE[read_statement_node]="ast_nodes_io"
TYPE_TO_MODULE[write_statement_node]="ast_nodes_io"
TYPE_TO_MODULE[LITERAL_INTEGER]="ast_base"
TYPE_TO_MODULE[LITERAL_REAL]="ast_base"
TYPE_TO_MODULE[LITERAL_STRING]="ast_base"
TYPE_TO_MODULE[LITERAL_LOGICAL]="ast_base"
TYPE_TO_MODULE[init_ast_arena]="ast_arena"

# Step 2: Generate specific imports
echo "Step 2: Generating explicit import statements..."

declare -A MODULE_TYPES
for type in $AST_TYPES; do
    module="${TYPE_TO_MODULE[$type]}"
    if [[ -n "$module" ]]; then
        if [[ -n "${MODULE_TYPES[$module]}" ]]; then
            MODULE_TYPES[$module]="${MODULE_TYPES[$module]}, $type"
        else
            MODULE_TYPES[$module]="$type"
        fi
    fi
done

# Step 3: Create replacement import block
REPLACEMENT=""
REPLACEMENT+="\n    ! Migrated from ast_core: use explicit imports for better dependency management"
for module in "${!MODULE_TYPES[@]}"; do
    REPLACEMENT+="\n    use $module, only: ${MODULE_TYPES[$module]}"
done

echo "Step 3: Generated replacement imports:"
echo -e "$REPLACEMENT"

# Step 4: Perform replacement (dry run first)
echo "Step 4: Replacement preview (use --apply to actually apply)..."
if grep -q "use ast_core" "$SOURCE_FILE"; then
    echo "Would replace 'use ast_core' with explicit imports"
else
    echo "No 'use ast_core' found in file"
fi

# If --apply flag is provided, do the actual replacement
if [[ "${2:-}" == "--apply" ]]; then
    echo "Applying migration..."
    sed -i "s|use ast_core.*|$REPLACEMENT|" "$SOURCE_FILE"
    echo "Migration applied to $SOURCE_FILE"
    echo "Backup saved as $SOURCE_FILE.backup"
    echo ""
    echo "Please run 'fpm build' to test compilation and fix any missing imports."
else
    echo ""
    echo "This was a dry run. Use '$0 $SOURCE_FILE --apply' to actually perform the migration."
fi

echo "=== Migration analysis complete ==="