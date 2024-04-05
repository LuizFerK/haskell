module A03 where

data Cliente = OrgGov String
             | Empresa String Integer String String
             | Individuo Pessoa Bool Genero
             deriving Show

data Pessoa = Pessoa String String
             deriving Show

data Genero = Masculino
            | Feminino
            | Outro
            deriving Show

nomeCliente::Cliente -> String
nomeCliente(OrgGov nome) = nome
nomeCliente(Empresa nome _ _ _) = nome
nomeCliente(Individuo (Pessoa n s) _ _) = n ++ " " ++ s

nomeEmpresa::Cliente -> Maybe String
nomeEmpresa(Empresa nome _ _ _) = Just nome
nomeEmpresa _ = Nothing

primeiro::(a, b) -> a
primeiro(a, _) = a

segundo::(a, b) -> b
segundo(_, b) = b

terceiro::(a, b, c) -> c
terceiro(_, _, c) = c

primeiroLista::[a] -> a
primeiroLista(h:t) = h

ultimosLista::[a] -> [a]
ultimosLista(h:t) = t
