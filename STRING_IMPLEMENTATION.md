# Implementação do Tipo `string` do Projeto 1

## Resumo
Implementei o tipo `string` do SmartTS (Projeto 1) com 4 funções built-in de manipulação de string e testes unitários.

## Funções Implementadas

### 1. `string_concat(s1: string, s2: string): string`
Concatena duas strings.
```smartts
return string_concat("hello", "world");  // "helloworld"
```

### 2. `string_length(s: string): int`
Retorna o comprimento de uma string.
```smartts
return string_length("hello");  // 5
```

### 3. `string_uppercase(s: string): string`
Converte uma string para maiúsculas.
```smartts
return string_uppercase("hello");  // "HELLO"
```

### 4. `string_lowercase(s: string): string`
Converte uma string para minúsculas.
```smartts
return string_lowercase("HELLO");  // "hello"
```

## Adicionais

- **Operador `+` para strings**: Além de somar inteiros, o operador `+` agora também concatena strings:
  ```smartts
  return "hello" + "world";  // "helloworld"
  ```

- **Escape sequences**: Suporte a sequências de escape em strings literais:
  ```smartts
  return "hello\nworld";  // com quebra de linha
  return "tab\there";     // com tabulação
  ```

- **Storage com campos string**: Campos de armazenamento podem ser do tipo `string`

## Arquivos Modificados

1. **lib/SmartTS/AST.hs**
   - Adicionado `TString` ao tipo `Type`
   - Adicionado `CString String` ao tipo `Expr`
   - Adicionado `Call Name [Expr]` para chamadas de função

2. **lib/SmartTS/Parser.hs**
   - Adicionado parser para literais de string com escape sequences
   - Adicionado suporte a chamadas de função (sintaxe `funcName(arg1, arg2)`)
   - Adicionado `"string"` à lista de palavras reservadas

3. **lib/SmartTS/TypeCheck.hs**
   - Adicionado `inferCall` para type-check de funções built-in
   - Adicionado `inferStringOrIntBin` para permitir `+` com strings e ints
   - Atualizado `typesEqual` e `prettyType` para suportar `TString`

4. **lib/SmartTS/Interpreter.hs**
   - Adicionado `evalCall` para executar as 4 funções de string
   - Atualizado `exprToJson` para serializar strings
   - Atualizado `jsonToExprByType` para desserializar strings
   - Modificado `evalExpr` para concatenação de strings com `+`

5. **test/Main.hs**
   - Adicionado grupo `stringTests` com 17 testes unitários:
     - Testes de parsing de literais de string
     - Testes de parsing de chamadas de função
     - Testes de type-check para garantir segurança de tipos
     - Testes de escape sequences
     - Testes de igualdade e desigualdade entre strings

## Testes Unitários

Os 17 testes cobrem:
- Parsing de strings literais
- Parsing de chamadas de função
- Type-checking correto das funções
- Detecção de erros de tipo (ex: `string_concat(s, 123)` falha)
- Escape sequences em strings
- Comparação de igualdade entre strings
- Concatenação com operador `+`

## Exemplo de Contrato

Um contrato exemplo está em `samples/StringExample.smartts` que demonstra todas as funcionalidades:
- Armazenamento de strings
- Concatenação de strings
- Conversão para maiúsculas/minúsculas
- Cálculo de comprimento
