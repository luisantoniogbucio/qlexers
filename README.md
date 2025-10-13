# qlexers

Analizador léxico.

## Lenguajes Soportados

- **IMP**: Lenguaje imperativo simple con asignaciones, condicionales y bucles
- **SQL**: Subconjunto de SQL con operaciones básicas (SELECT, INSERT, UPDATE, DELETE, CREATE TABLE)

## Compilación

### Requisitos
- Stack (Haskell Tool Stack)
- GHC (Glasgow Haskell Compiler)

### Compilar el proyecto
```bash
stack build
```
## Uso

### Sintaxis general
```bash
stack run -- --<lenguaje> --archivo <ruta_archivo>
```

### Ejemplos

#### Tokenizar archivo IMP
```bash
stack run -- --imp --archivo samples/imp/test1.imp
```

#### Tokenizar archivo SQL
```bash
stack run -- --sql --archivo samples/sql/test1.sql
```

### Opciones disponibles
- `--imp`: Analizar archivo usando el lexer IMP
- `--sql`: Analizar archivo usando el lexer SQL
- `--archivo <ruta>`: Especificar el archivo a analizar

## Estructura del proyecto

```
qlexers/
├── src/
│   ├── Main.hs           # Punto de entrada principal
│   ├── MDD.hs            # Motor de Decisión Determinista genérico
│   ├── LexerIMP.hs       # Lexer para lenguaje IMP
│   └── LexerSQL.hs       # Lexer para lenguaje SQL
├── samples/
│   ├── imp/              # Archivos de prueba IMP
│   └── sql/              # Archivos de prueba SQL
├── specs/
│   ├── IMP.md            # Especificación del lenguaje IMP
│   └── SQL.md            # Especificación del lenguaje SQL
└── tests/                # Tests unitarios
```

## Archivos de prueba

### IMP
- `samples/imp/test1.imp`: Asignaciones y condicional
- `samples/imp/test2.imp`: Bucle while con condicional anidado
- `samples/imp/test3.imp`: Cálculo de factorial

### SQL
- `samples/sql/test1.sql`: Consulta SELECT con WHERE
- `samples/sql/test2.sql`: Inserción con VALUES
- `samples/sql/test3.sql`: Creación de tabla con tipos de datos
