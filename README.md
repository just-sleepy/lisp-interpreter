# README - Interpretador LISP em Haskell

## Visão Geral

Este projeto é um interpretador LISP escrito em Haskell. Ele é projetado para interpretar e executar um subconjunto da linguagem de programação LISP, permitindo que os usuários trabalhem com expressões simbólicas e o estilo de programação funcional do LISP. O interpretador é capaz de manipular vários tipos de dados, incluindo átomos, listas, números, strings e valores booleanos, e oferece uma gama de funções integradas para operações matemáticas e lógicas, além de manipulação de listas.

## Componentes

- **Evaluator.hs**: Contém a lógica de avaliação das expressões LISP, incluindo a definição de tipos de dados e a implementação de funções.
- **Parser.hs**: Responsável pela análise sintática das expressões LISP, convertendo-as em estruturas de dados que podem ser avaliadas pelo Evaluator.
- **Main.hs**: Contém a lógica da interface do usuário, permitindo a entrada de expressões LISP para serem interpretadas. Inclui um REPL (Read-Eval-Print Loop) para interação contínua.

## Funcionalidades

- **Tipos de Dados**: Suporta átomos, listas, números, strings e valores booleanos.
- **Funções Integradas**: Inclui uma variedade de funções como operações aritméticas, operações lógicas e funções de processamento de listas (como `cons`, `head`, `tail`).
- **Avaliação de Funções**: Avalia expressões e funções LISP com ênfase nos paradigmas de programação funcional.
- **Suporte a Funções Personalizadas**: Os usuários podem definir e aplicar funções personalizadas no ambiente LISP.
- **Tratamento de Erros**: Tratamento básico de erros para expressões e funções desconhecidas.

## Uso

1. Digite "ghc --make -isrc app/Main.hs -o app/Main" para compilar
2. Vá para o diretório "app" e execute "./Main"
3. Agora você pode inserir expressões LISP para avaliação no REPL ou passar uma expressão como argumento na linha de comando. Caso queira sair, basta digitar "quit".

Exemplo:

```haskell
Lisp>>> (+ 1 2)
```
