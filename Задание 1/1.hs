{-
data Term = IntConstant{ intValue :: Int }
            | Variable{ varName :: String }
            | BinaryTerm{ lhv :: Term, rhv :: Term } deriving(Show,Eq)

Данная структура данных представляет собой ячейку дерева разбора некоторого языка, содержащего в себе числовые константы, переменные и бинарные операторы.

Следует расширить данную структуру данных таким образом,
чтобы она позволяла описывать различные бинарные операторы (сложение, умножение, вычитание), а так же унарный минус.
Помимо этого, нужно описать ряд функций:

  * Операторы <+>, <-> и <*>, которые создают значения соответствующих бинарных операций.
  * Функцию замены replaceVar, которая принимает имя переменной и терм, на который её следует заменить, и производит замену этой переменной на этот терм по всему выражению.
-}

data Operation = Plus | Minus | Mult deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }
            | Variable{ varName :: String }
            | UnaryTerm{ op :: Operation, trm :: Term }
            | BinaryTerm{ lhv :: Term, op :: Operation, rhv :: Term }
            deriving (Show, Eq)

a <+> b = BinaryTerm a Plus b
a <-> b = BinaryTerm a Minus b
a <*> b = BinaryTerm a Mult b
(-) a = UnaryTerm Minus a

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable varName) var term = if varName == var then term else Variable varName
replaceVar (UnaryTerm op trm) var term = UnaryTerm op $ replaceVar trm var term
replaceVar (BinaryTerm lhv op rhv) var term =
  BinaryTerm (replaceVar lhv var term) op (replaceVar rhv var term)
