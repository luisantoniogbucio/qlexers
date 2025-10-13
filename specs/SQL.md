# Especificación del Lenguaje SQL (Subconjunto)

## Gramática

```
Statement ::= 
    | SELECT SelectList FROM TableName WHERE Condition ;
    | INSERT INTO TableName ( ColumnList ) VALUES ( ValueList ) ;
    | UPDATE TableName SET Assignment WHERE Condition ;
    | DELETE FROM TableName WHERE Condition ;
    | CREATE TABLE TableName ( ColumnDefList ) ;
    | DROP TABLE TableName ;

SelectList ::= * | ColumnList
ColumnList ::= Identifier | ColumnList , Identifier
ValueList ::= Value | ValueList , Value
ColumnDefList ::= ColumnDef | ColumnDefList , ColumnDef

ColumnDef ::= Identifier DataType Constraints
DataType ::= INT | VARCHAR ( Number ) | DECIMAL ( Number , Number )
Constraints ::= PRIMARY KEY | NOT NULL | ε

Condition ::= 
    | Identifier = Value
    | Identifier > Value  
    | Identifier < Value
    | Condition AND Condition
    | Condition OR Condition

Value ::= Number | String | NULL
Identifier ::= [a-zA-Z][a-zA-Z0-9_]*
Number ::= [0-9]+(\.[0-9]+)?
String ::= '[^']*'
```

## Tokens

| Token | Descripción | Ejemplo |
|-------|-------------|---------|
| `TSELECT` | Palabra clave SELECT | `SELECT` |
| `TFROM` | Palabra clave FROM | `FROM` |
| `TWHERE` | Palabra clave WHERE | `WHERE` |
| `TINSERT` | Palabra clave INSERT | `INSERT` |
| `TINTO` | Palabra clave INTO | `INTO` |
| `TVALUES` | Palabra clave VALUES | `VALUES` |
| `TUPDATE` | Palabra clave UPDATE | `UPDATE` |
| `TSET` | Palabra clave SET | `SET` |
| `TDELETE` | Palabra clave DELETE | `DELETE` |
| `TCREATE` | Palabra clave CREATE | `CREATE` |
| `TTABLE` | Palabra clave TABLE | `TABLE` |
| `TDROP` | Palabra clave DROP | `DROP` |
| `TAND` | Operador AND | `AND` |
| `TOR` | Operador OR | `OR` |
| `TNOT` | Operador NOT | `NOT` |
| `TNULL` | Valor NULL | `NULL` |
| `TIdentifier String` | Identificador | `usuarios`, `nombre` |
| `TNumber Int` | Número | `42`, `1500` |
| `TString String` | Cadena de texto | `'Juan'`, `'email@test.com'` |
| `TAsterisk` | Asterisco | `*` |
| `TComma` | Coma | `,` |
| `TSemicolon` | Punto y coma | `;` |
| `TLParen` | Paréntesis izquierdo | `(` |
| `TRParen` | Paréntesis derecho | `)` |
| `TEq` | Operador igualdad | `=` |
| `TGt` | Operador mayor que | `>` |
| `TLt` | Operador menor que | `<` |

## Ejemplos

```sql
SELECT * FROM usuarios WHERE edad > 18;
```

```sql
INSERT INTO productos (nombre, precio) VALUES ('laptop', 1500);
```

```sql
CREATE TABLE empleados (
    id INT PRIMARY KEY,
    nombre VARCHAR(50) NOT NULL,
    salario DECIMAL(10,2)
);
```

```sql
UPDATE clientes SET email = 'nuevo@email.com' WHERE id = 123;
```
