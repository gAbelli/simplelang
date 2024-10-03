grammar SimpleLang;

program: (functionDecl)* EOF;

functionDecl: 'function' ID '(' paramList? ')' block;

paramList: ID (',' ID)*;

stmtList: (stmt)*;

stmt: letStmt | ifStmt | whileStmt | assignmentStmt | returnStmt | block;

block: '{' stmtList '}';

letStmt: 'let' ID '=' expr ';';

ifStmt: 'if' '(' expr ')' block ('else' block)?;

whileStmt: 'while' '(' expr ')' block;

assignmentStmt: ID '=' expr ';';

returnStmt: 'return' expr ';';

expr: relationalExpr;

relationalExpr: additiveExpr (('>' | '<' | '>=' | '<=' | '==' | '!=') additiveExpr)*;

additiveExpr: multiplicativeExpr (('+' | '-') multiplicativeExpr)*;

multiplicativeExpr: primaryExpr (('*' | '/') primaryExpr)*;

primaryExpr: functionCall | ID | INT | '(' expr ')';

functionCall: ID '(' argList? ')';

argList: expr (',' expr)*;

ID: [a-zA-Z_][a-zA-Z0-9_]*;

INT: [0-9]+;

WS: [ \t\r\n]+ -> skip;
