# Especificación del Lenguaje IMP

## Gramática

```
Program ::= Statement

Statement ::= 
    | Identifier := ArithExpr ;
    | skip ;
    | Statement Statement
    | if BoolExpr then Statement else Statement
    | while BoolExpr do Statement
    | ( Statement )

ArithExpr ::=
    | Number
    | Identifier  
    | ArithExpr + ArithExpr
    | ArithExpr - ArithExpr
    | ArithExpr * ArithExpr
    | ( ArithExpr )

BoolExpr ::=
    | ArithExpr = ArithExpr
    | ArithExpr <= ArithExpr
    | ( BoolExpr )

Identifier ::= [a-zA-Z][a-zA-Z0-9]*
Number ::= [0-9]+
```

## Tokens

| Token | Descripción | Ejemplo |
|-------|-------------|---------|
| `TId String` | Identificador | `x`, `variable1` |
| `TNum Int` | Número entero | `42`, `0` |
| `TIf` | Palabra clave if | `if` |
| `TThen` | Palabra clave then | `then` |
| `TElse` | Palabra clave else | `else` |
| `TWhile` | Palabra clave while | `while` |
| `TDo` | Palabra clave do | `do` |
| `TSkip` | Palabra clave skip | `skip` |
| `TPlus` | Operador suma | `+` |
| `TMinus` | Operador resta | `-` |
| `TTimes` | Operador multiplicación | `*` |
| `TEq` | Operador igualdad | `=` |
| `TLeq` | Operador menor o igual | `<=` |
| `TAssign` | Operador asignación | `:=` |
| `TSemi` | Punto y coma | `;` |
| `TLParen` | Paréntesis izquierdo | `(` |
| `TRParen` | Paréntesis derecho | `)` |

## Ejemplos

```imp
x := 5;
y := x + 1;
if x <= 10 then z := y * 2 else z := x - 1
```

```imp
while x <= 10 do (
    x := x + 1;
    if x = 5 then skip else y := y * x
)
```
