# spy
# Objetivos
A ideia dessa aplicação feita em haskell é desenvolver o pattern Specification no nivel dos tipos.
Caso esse design pattern não seja familiar a você sugiro que veja sua definição e implementação no link abaixo:
https://en.wikipedia.org/wiki/Specification_pattern

O pattern tem como objetivo seguir o principio DRY (DONT REPEAT YOURSELF) no sentido de evitar duplicações das regras de negócio dentro da sua aplicação,
além disso é possivel que você construa especificações com outras especificações.

# Qual o objetivo das especificações como posso utiliza-las no meu código?

## Construção de Objetos
Esse padrão serve para aquelas situações onde para criar um objeto você precisa que uma série de especificações sejam atendidas.
Por exemplo imagine que você esta criando um pedido e você tem uma lista de items na memória, esses itens tem um campo
quantidade indicado por um (int) e se esse campo for 0 no item que você quer adicionar ele não pode ser adicionado no pedido, provavelmente 
sua classe pedido vai ter um método addItem que vai adicionar esse item somente se a especificação dita acima for atendida.

## Filtrando Coleções e Criando Consultas
O mesmo exemplo acima pode ser utilizado aqui, imagine que a mesma lista de produtos existe na memória e que você quer filtrar somente os items
que tenham quantidade maior que 0, você pode usar a mesma especificação para filtrar a coleção ja que por padrão a avaliação da especifiação
retorna um booleano ela funciona como um Predicate.

Só para exemplificar a criação de consultas imagine que sua linguagem possua um framework que converta expressões booleanas em clausulas 
WHERE no SQL, ORMs como EntityFramework e a biblioteca Linq conseguem integrar esse tipo de funcionalidade bem no C# e criar consultas no 
nivel de banco de dados com as mesmas espeficicações que você criou no C#, ou seja, você ganhou de quebra a possibilidade de não ter que carregar
todos os dados em memória para filtra-los.

# Qual o problema do pattern acima implementado como no link apresentado?
A resposta é a falta da segurança no nivel dos tipos, imagine que você tem um caso de uso no seu software relacionado a alteração de um pedido,
você não quer que um pedido com status fechado possa ter preço e itens alterados imagine se na assinatura da sua função você pudesse dizer que quer que
o tipo de pedido que entra nela tenha obrigatóriamente o status de pedido pendente para que modificações no pedido sejam feitas, a coisa começa a ficar mais complexa quando as exigencias vão sendo incorporadas, preciso que tal propriedade do pedido seja assim ou que outra seja assada e que outras duas sejam verdadeiras ao mesmo tempo... imagine a bagunça e a quantidade de bugs que seu código pode ter se você confiar em implementações que validem somente estruturas no nivel dos valores.
Existe uma forma de fazer seu software não compilar caso alguem passe um parametro que não condiz com a assinatura da função validando em tempo de compilação se todas as especificações da assinatura são validas? Sim e é isso que essa POC feita em Haskell quer mostrar.


# Implementação
A implementação foi uma adaptação com mais operações no nivel de tipos do programa de validação de strings desenvolvido na disciplina "Desenvolvimento guiado por tipos" da UFABC, você encontra a solução completa do validador mencionado no link: https://haskell.pesquisa.ufabc.edu.br/desenvolvimento-orientado-a-tipos/06.typefamily/

A diferença entre a implementação feita nesse trabalho qunado comparada a desenvolvida como exemplo na disciplina é que enquanto no validador de strings se validava se a string continha ou não tal requisito a implementação desse repositório permite criar expressões booleanas que avaliam se o tipo satisfaz todas as restrições impostas a ele por expressões booleanas.

Recomendo que primeiramente seja visitado o arquivo Operators.hs na pasta src/Common, la vão ter todos os operadores no nivel de tipos que precisamos para construir nossa arvore de expressões booleanas e avalia-la, em seguida entre no módulo de espiões, perceba que no nivel dos valores um espião é uma estrutura minima, enquanto ao entrar no módulo de especificações dos espiões você vera funções que avaliam no nivel dos valores um espião e retornam um tipo Maybe de espião com deternimada especificação, assim conseguimos ter uma avaliação que depende de valores com um retorno de um tipo que carrega a informação se o espião avaliado tem ou não a especificação. Finalmente após entender bem o que acontece no módulo dos espiões você pode ir para o módulo de missões e ver como podemos usar o pattern implementado no nivel dos tipos para construir novos tipos de missões e assim concluimos a implementação demonstrando que todos os pontos positivos originais do padrão são mantidos com o aditivo da segurança de tipos.

Após toda essa leitura e roteiro para entender a solução veja os exemplos montados na função main no arquivo Main.hs da pasta app, nele vão estar contidos códigos comentados com exemplos que não compilariam e uma breve explicação e exemplos funcionais de como usar o código.

Se você leu até aqui, muito obrigado pela atenção.
