Documentação do html-temple
---

> TODO! mover isso para um lugar apropiado
>
> os elementos `link`, `meta`, `title`, `script` e `style` são sempre movidos para o head e instâncias duplicadas são removidas

você código executável começa com `@` e o fim é detectado automaticamente

para incluir um `@` no html, basta colocar dois `@@`

dentro do código executável você pode incluir html com `{{`

scripts sempre começam no modo html

```html
@tabela = [
    { nome: "Fulano", conta: 1052.32 },
    { nome: "Sicrano", conta: 892.51 },
    { nome: "Deltrano", conta: 14252.49 },
]
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Document</title>
</head>
<body>
    <table>
    <thead>
        <tr>
            <th>Nome</th>
            <th>Total</th>
        </tr>
    </thead>
    <tbody>
        @for row in tabela do {{
            <tr>
                <td> @row.nome </td>
                <td> @fmt_currency_br(row.conta) </td>
            </tr>
        }}
        @if tabela == [] then {{
            <tr>
                <td colspan="2">
                    Não há registros
                </td>
            </tr>
        }}
    </tbody>
</table>
</body>
</html>
```

todo código executável é uma expressão e sempre retorna algum valor

atribuições (`=`, `+=` e outros) sempre retornam null

o for retorna um array contendo todos os itens evaluados (o corpo da função também é uma expressão)

o if retorna o valor do corpo que foi executado, se der false e não tiver um corpo no else, o if retorna null

```html
<!-- demonstração dos tipos de dados que existem -->
@nulo = null
@inteiro = 30
@flutuante = 1.0
@booleano = true
@strings = "teste"
@arrays = [1, 2, 3]
@objetos = { a: 1, b: 2, c: 3 }
@funcoes = fn (a) => [a, a]
@html = {{ <div></div> }}
@range = 0..10
```

o tipo de dado html é diferente do tipo string

a diferença é que quando você printa uma string, caracteres perigosos são escapados, quando você printa html, nada é escapado

html é uma string so que já escapada

isso facilita na hora de criar funções que retornam html, já que você não tem que desativar o escapamento na hora de botar o resultado da função no documento
