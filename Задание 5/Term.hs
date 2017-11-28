module Term where
  
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
