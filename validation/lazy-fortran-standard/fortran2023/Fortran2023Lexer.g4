// F2023 Lexer Grammar for fortfront validation
lexer grammar Fortran2023Lexer;

// Keywords
PROGRAM : 'program' | 'PROGRAM' ;
END : 'end' | 'END' ;
MODULE : 'module' | 'MODULE' ;
SUBROUTINE : 'subroutine' | 'SUBROUTINE' ;
FUNCTION : 'function' | 'FUNCTION' ;
TYPE : 'type' | 'TYPE' ;
CLASS : 'class' | 'CLASS' ;
EXTENDS : 'extends' | 'EXTENDS' ;
ABSTRACT : 'abstract' | 'ABSTRACT' ;
PROCEDURE : 'procedure' | 'PROCEDURE' ;

// Data types
INTEGER : 'integer' | 'INTEGER' ;
REAL : 'real' | 'REAL' ;
COMPLEX : 'complex' | 'COMPLEX' ;
LOGICAL : 'logical' | 'LOGICAL' ;
CHARACTER : 'character' | 'CHARACTER' ;

// F2023 specific keywords
COARRAY : 'codimension' | 'CODIMENSION' ;
SUBMODULE : 'submodule' | 'SUBMODULE' ;
ERROR : 'error' | 'ERROR' ;
STOP : 'stop' | 'STOP' ;
BIND : 'bind' | 'BIND' ;

// Operators
ASSIGN : '=' ;
PLUS : '+' ;
MINUS : '-' ;
MULTIPLY : '*' ;
DIVIDE : '/' ;
POWER : '**' ;

// Delimiters
LPAREN : '(' ;
RPAREN : ')' ;
LBRACKET : '[' ;
RBRACKET : ']' ;
COMMA : ',' ;
COLON : ':' ;
DOUBLE_COLON : '::' ;
SEMICOLON : ';' ;

// Identifiers and literals
IDENTIFIER : [a-zA-Z][a-zA-Z0-9_]* ;
INTEGER_LITERAL : [0-9]+ ;
REAL_LITERAL : [0-9]+ '.' [0-9]* ([eE][+-]?[0-9]+)? ;
STRING_LITERAL : '"' (~["])* '"' | "'" (~['])* "'" ;

// Whitespace and comments
WHITESPACE : [ \t\r\n]+ -> skip ;
COMMENT : '!' ~[\r\n]* -> skip ;
