-- 1. Defina uma função que recebe o salário base de um funcionário e resulta no salário líquido a
-- receber, sabendo-se que o funcionário tem gratificação de 10% sobre o salário base e paga
-- imposto de 7% sobre o salário base. Exemplos:

salario :: Float -> Float
salario s = s * 1.1 - (s * 0.07)

-- 2. A nota final de um estudante é calculada a partir de três notas atribuídas respectivamente a
-- um trabalho de laboratório, a uma avaliação semestral e a um exame final. A média
-- ponderada das três notas mencionadas obedece aos pesos a seguir:
-- Faça uma função que recebe as três notas, calcula a média ponderada e exibe o conceito
-- obtido pelo aluno conforme a tabela abaixo.

media :: Float -> Float -> Float -> Char
media t a e = mediaChar ((t * 2 + a * 3 + e * 5) / 10)

mediaChar :: Float -> Char
mediaChar m
        | m >= 8 = 'A'
        | m >= 7 = 'B'
        | m >= 6 = 'C'
        | m >= 5 = 'D'
        | otherwise = 'E'

-- 3. O estúdio fotográfico Boa Imagem cobra de seus clientes por retratos antigos baseando-se
-- no número de indivíduos incluídos no retrato. As tarifas constam da tabela seguinte.
-- Retratos antigos tirados aos sábados ou aos domingos custam 20% a mais do que o preço
-- base. Defina uma função precoRetrato do tipo Integer -> String -> Double que recebe como
-- argumentos o número de pessoas no retrato e o dia da semana agendado, e calcula o custo do
-- retrato. Exemplos:

precoRetrato :: Integer -> String -> Double
precoRetrato p d = personPrice(p) * dayMultiplier(d)

personPrice :: Integer -> Double
personPrice 1 = 100
personPrice 2 = 130
personPrice 3 = 150
personPrice 4 = 165
personPrice 5 = 175
personPrice 6 = 180
personPrice _ = 185

dayMultiplier :: String -> Double
dayMultiplier "sabado" = 1.2
dayMultiplier "domingo" = 1.2
dayMultiplier _ = 1

-- 4. O fatorial duplo de um número natural n é o produto de todos os números de 1 (ou 2) até n,
-- contados de 2 em 2. Por exemplo, o fatorial duplo de 8 é 8 × 6 × 4 × 2 = 384, e o fatorial
-- duplo de 7 é 7 × 5 × 3 × 1 = 105. Defina uma função para calcular o fatorial duplo usando
-- recursividade.

fat 0 = 1
fat (-1) = 1
fat n = n * fat (n - 2)

-- 5. Defina uma função recursiva para calcular a potência de um número, considerando que o
-- expoente é um número natural. Utilize o método das multiplicações sucessivas:

pow :: Int -> Int -> Int
pow _ 0 = 1
pow n 1 = n
pow n p = n * (pow n (p - 1))

-- 6. Um funcionário de uma empresa recebe aumento salarial anualmente. O primeiro aumento é
-- de 1,5% sobre seu salário inicial. Os aumentos subsequentes sempre correspondem ao dobro
-- do percentual de aumento do ano anterior. Faça uma função onde é informado o salário
-- inicial do funcionário, o ano de contratação e o ano atual, e calcula e exibe o seu salário
-- atual.

currentWage :: Float -> Int -> Int -> Float
currentWage s y cy = currentWage' s (cy - y) 0.015

currentWage' :: Float -> Int -> Float -> Float
currentWage' s 0 _ = s
currentWage' s y x = currentWage' (s * (1 + x)) (y - 1) (x * 2)

-- 7. Defina uma função chamada ultimo que seleciona o último elemento de uma lista não
-- vazia.

ultimo :: [a] -> a
ultimo (h:[]) = h
ultimo (h:tl) = ultimo tl

-- 8. Defina uma função chamada primeiros que seleciona todos os elementos de uma lista não
-- vazia, exceto o último.

primeiros :: [a] -> [a]
primeiros [] = []
primeiros [_] = []
primeiros (h:tl) = h : primeiros tl

-- 9. Faça uma função que receba duas listas e retorne outra lista produto destas duas listas, ou
-- seja, cada posição das listas de entrada devem ser multiplicadas e armazenadas na mesma
-- posição na lista de saída.

prodList :: [Int] -> [Int] -> [Int]
prodList [] [] = []
prodList (xh:xtl) (yh:ytl) = xh * yh : prodList xtl ytl

-- 10. Defina um novo tipo de dado chamado Produto, que permita armazenar informações sobre:
-- a. Produto perecível: código, descrição, ano de validade e se é comestível ou não.
-- b. Produto não perecível: código, descrição, fabricante, ano de fabricação.
-- Faça testes com este novo tipo de dado.

-- data Produto = Perecivel String String Int Bool
--              | NPerecivel String String String Int
--              deriving Show

-- Perecivel "A123" "comida1" 2023 False
-- NPerecivel "B123" "comida2" "MarcaX" 2024

-- 11. Defina um novo tipo de dado para armazenar a forma de comercialização de um produto,
-- com duas opções:
-- a. Unidade
-- b. Peso
-- Adicione este novo tipo de dado às duas opções do produto e refaça os testes anteriores.

data Comerc = Unidade | Peso deriving Show

data Produto = Perecivel String String Int Bool Comerc
             | NPerecivel String String String Int Comerc
             deriving Show

-- Perecivel "A123" "comida1" 2023 False Unidade
-- NPerecivel "B123" "comida2" "MarcaX" 2024 Peso

-- 12. Faça uma função que receba um produto e o ano atual e verifique se ele ainda está válido
-- para uso, retornando um valor booleano. Considere que produtos não perecíveis sempre
-- estão válidos.

productIsValid :: Produto -> Int -> Bool
productIsValid (NPerecivel _ _ _ _ _) _ = True
productIsValid (Perecivel _ _ py _ _) y = y <= py

-- 13. Escreva as funções and e or usando casamento de padrões.

andX :: Bool -> Bool -> Bool
andX True True = True
andX _ _ = False

orX :: Bool -> Bool -> Bool
orX True _ = True
orX _ True = True
orX _ _ = False

-- 14. Usando casamento de padrão, defina uma função que, dada uma lista de números, retorna:
-- a. a soma dos dois primeiros elementos, se a lista tiver pelo menos dois elementos;
-- b. a cabeça da lista, se ela contiver apenas um elemento;
-- c. zero, caso contrário.

func14 :: [Int] -> Int
func14 [x, y] = x + y
func14 [h] = h
func14 _ = 0

-- 15. Utilize uma função de alta ordem para realizar a contagem de elementos de uma lista.

count :: [Int] -> Int
count l = foldl (\acc _ -> acc + 1) 0 l

-- 16. Identifique qual é o resultado da compilação e avaliação das expressões a seguir, podendo
-- ser um determinado valor ou lista de valores, erro de sintaxe ou erro de tipo.

-- a. if 1 == 2 then "abc" else [’d’, ’e’, ’f’]
-- R: Erro pq "abc" = String e [’d’, ’e’, ’f’] = [Char]
--    then e else do if devem retornar os mesmos tipos

-- b. let pot_dois x | x <= 0 = 1
--                   | otherwise = 2 * pot_dois (x-1)
--    in pot_dois 6
-- R: 64

-- c. case not (1 /= 2) of
--          True -> 3
--          False -> "fim”
-- R: "fim"

-- d. filter (not . even . (+3)) [5, 6, 7, 8, 9]
-- R: [5, 6, 7, 8, 9]
--    [8, 9, 10, 11, 12]
--    [True, False, True, False, True]
--    [False, True, False, True, False]
--    [6, 8]

-- e. sum (map (logBase 3) [9, 27, 81])
-- R: 9.0

-- f. foldl (\x y -> x + y) 0 [10,20,30]
-- R: 60