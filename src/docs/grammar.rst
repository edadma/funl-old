Grammar
=======

Here is the grammar used to parse FunL presented in EBNF.


Syntactic Grammar
-----------------

The syntactic grammar productions::

    source ::= Newline | statements

    declaration ::= imports | natives | constants | variables | data | definitions

    imports ::=
        'import' importModule
      | 'import' Indent importModule+ Dedent Newline

    importModule ::= name

    natives ::=
        'native' name
      | 'native' Indent name+ Dedent Newline
      | 'function' name
      | 'function' Indent name+ Dedent Newline

    dottedName ::= ident ('.' ident)*

    qualifier ::= ident ('=>' ident)?

    name ::=
        dottedName ('=>' ident)? Newline
      | dottedName '.' '{' qualifier (',' qualifier)* '}' Newline
      | dottedName '.' '*' Newline

    identifiers ::= ident (',' ident)*

    constants ::=
        'val' constant
      | 'val' Indent constant+ Dedent Newline

    constant ::=
        pattern '=' expressionOrBlock Newline

    variables ::=
        'var' variable
      | 'var' Indent variable+ Dedent Newline

    variable ::=
        ident ('=' expressionOrBlock)? Newline

    data ::=
        'data' datatype
      | 'data' Indent datatype+ Dedent Newline

    datatype ::=
        ident '=' constructor ('|' constructor)* Newline
      | constructor Newline

    constructor ::=
        ident '(' identifiers ')'
      | ident

    definitions ::=
        'def' definition
      | 'def' Indent definition+ Dedent Newline

    definition ::=
        ident ('(' (pattern (',' pattern)*)? ')')? (optionallyGuardedPart | guardedParts)

    optionallyGuardedPart ::=
        ('|' booleanExpression)? '=' expressionOrBlock Newline

    guardedPart ::=
      '|' ('otherwise' | booleanExpression) '=' expressionOrBlock Newline

    guardedParts ::= Indent guardedPart+ Dedent Newline

    statements ::= statement+

    optionalNewline ::= Newline?

    expressionStatement ::= expression Newline

    statement ::=
        expressionStatement
      | declaration

    blockExpression ::=
        Indent statements Dedent

    assignment ::= '=' | '+=' | '-=' | '*=' | '/=' | '\=' | '^='

    expression ::=
        lvalueExpression (',' lvalueExpression)* assignment nonAssignmentExpression (',' nonAssignmentExpression)*
      | nonAssignmentExpression

    lambdaExpression ::=
      ('(' pattern (',' pattern)* ')' | pattern) ('|' booleanExpression)? '->' expression

    caseFunctionExpression ::=
        Indent lambdaExpression (Newline lambdaExpression)* Dedent

    functionExpression ::=
        lambdaExpression | caseFunctionExpression

    nonAssignmentExpression ::=
        functionExpression | controlExpression

    elif ::=
        optionalNewline 'elif' booleanExpression ('then' expressionOrBlock | blockExpression)

    generator ::=
        pattern '<-' expression ('if' expression)?

    generators ::= generator (',' generator)*

    expressionOrBlock ::= expression | blockExpression

    elif ::= optionalNewline 'elif' booleanExpression ('then' expressionOrBlock | blockExpression)

    elsePart ::= (optionalNewline 'else' expressionOrBlock)?

    controlExpression ::=
        'if' booleanExpression ('then' expressionOrBlock | blockExpression) elif* elsePart
      | 'for' generators ('do' expressionOrBlock | blockExpression) elsePart
      | 'for' expressionOrBlock
      | 'while' expression ('do' expressionOrBlock | blockExpression) elsePart
      | 'do' expression optionalNewline 'while' expression elsePart
      | 'do' expression optionalNewline 'until' expression elsePart
      | 'break'
      | 'continue'
      | 'return' expression?
      | 'case' expression ('of' functionExpression | caseFunctionExpression)
      | orExpression

    booleanExpression ::= orExpression

    orExpression ::=
        orExpression ('or' | 'xor') andExpression
      | andExpression

    andExpression ::=
        andExpression ('and' | 'rotateright' | 'rotateleft' | 'shiftright' | 'shiftleft') notExpression
      | notExpression

    notExpression ::=
        'not' notExpression
      | comparisonExpression

    comparisonExpression ::=
        iteratorExpression ('==' | '!=' | '<' | '>' | '<=' | '>=' | 'in' | 'not' 'in'^ 'notin' | '|' | '/|') iteratorExpression
      | iteratorExpression 'is' ident
      | iteratorExpression

    iteratorExpression ::=
        (consExpression '|') generators
      | consExpression

    consExpression ::=
        rangeExpression (':' consExpression)
      | rangeExpression ('#' consExpression)
      | rangeExpression

    keyExpression ::= rangeExpression

    rangeExpression ::=
        additiveExpression ('..' | 'until') additiveExpression ('by' additiveExpression)?
      | (additiveExpression '..') ('by' additiveExpression)?
      | additiveExpression

    additiveExpression ::=
        additiveExpression ('+' | '-') multiplicativeExpression
      | multiplicativeExpression

    multiplicativeExpression ::=
        multiplicativeExpression ('*' | '/' | '\' | '%') exponentialExpression
      | multiplicativeExpression applyExpression
      | exponentialExpression

    exponentialExpression ::=
        exponentialExpression '^' negationExpression
      | negationExpression

    negationExpression ::=
        '-' incrementExpression
      | incrementExpression

    incrementExpression ::=
        ('++' | '--') applyExpression
      | applyExpression ('++' | '--')
      | applyExpression

    lvalueExpression ::= applyExpression

    applyExpression ::=
        applyExpression ('(' (expression (',' expression)*)? ')')
      | applyExpression ('.' | '.>') ident
      | primaryExpression

    MapEntry ::=
        keyExpression ':' expression

    comprehensionExpression ::=
        consExpression '|' generators

    primaryExpression ::=
        numericLit
      | stringLit
      | '(' infix ')'
      | '(' expression infix ')'
      | '(' infixNoMinus expression ')'
      | '(' expression ')'
      | ident
      | ('true' | 'false')
      | '(' ')'
      | ('(' nonAssignmentExpression ',') nonAssignmentExpression (',' nonAssignmentExpression)* ')'
      | '[' comprehensionExpression ']'
      | '[' (nonAssignmentExpression (',' nonAssignmentExpression)*)? ']'
      | 'null'
      | '{' (keyExpression (',' keyExpression)*)? '}'
      | '{' MapEntry (',' MapEntry)* '}'
      | '$' ident
      | '?' ident

    infixNoMinus ::= '+' | '*' | '/' | '\' | '^' | '%' | '==' | '!=' | '<' | '>' | '<=' | '>=' | ':' | '#' | 'and' | 'or' | 'xor'

    infix ::= infixNoMinus | '-'

    pattern ::=
        ident '@' typePattern
      | typePattern

    typePattern ::=
        consPattern '::' ident
      | consPattern

    consPattern ::=
        primaryPattern ':' consPattern
      | primaryPattern

    primaryPattern ::=
        numericLit
      | stringLit
      | 'true' | 'false'
      | '(' ')'
      | 'null'
      | ident '(' pattern (',' pattern)* ')'
      | ident
      | '(' pattern ',' pattern (',' pattern)* ')'
      | '[' (pattern (',' pattern)*)? ']'
      | '{' '}'
      | '(' pattern '|' pattern ('|' pattern)* ')'
      | '(' pattern ')'

Lexical Grammar
---------------

The reserved words in the language are: ``and``, ``break``, ``by``, ``case``, ``class``, ``continue``, ``data``, ``def``, ``do``, ``elif``, ``else``, ``false``, ``for``, ``forever``, ``function``, ``if``, ``import``, ``in``, ``is``, ``mod``, ``native``, ``not``, ``null``, ``of``, ``or``, ``otherwise``, ``repeat``, ``return``, ``then``, ``true``, ``until``, ``val``, ``var``, ``while``, ``xor``, ``yield``.

The special delimiters are: ``+``, ``*``, ``-``, ``/``, ``^``, ``(``, ``)``, ``[``, ``]``, ``|``, ``{``, ``}``, ``,``, ``=``, ``==``, ``!=``, ``<``, ``$``, ``>``, ``<-``, ``<=``, ``>=``, ``--``, ``++``, ``.``, ``..``, ``<-``, ``->``, ``=>``, ``+=``, ``-=``, ``*=``, ``/=``, ``^=``, ``:``, ``\\``, ``::``, ``@``, ``?``.

The lexical grammar productions::

	decimalParser =
		rep1(digit) ~ optFraction ~ optExponent |
		fraction ~ optExponent

	sign = '+' | '-'

	optSign = opt( sign )

	fraction = '.' ~ rep1(digit)

	optFraction = opt( fraction )

	exponent = ('e' | 'E') ~ optSign ~ rep1(digit)

	optExponent = opt(exponent)