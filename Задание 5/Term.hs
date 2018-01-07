module Term where

  import Data.Maybe

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

  {---------------------------------------------------------------------------------}
  {-Функции для получения бинарного терма с оператором, имеющим наивысший приоритет-}
  {---------------------------------------------------------------------------------}
  leftMostMult :: Term -> Maybe Term
  -- Раскручиваем до BinaryTerm с Int'ами. Не берём во внимание остальные типы.
  leftMostMult term@(BinaryTerm (IntConstant _) Mult (IntConstant _)) = Just term
  leftMostMult (BinaryTerm (IntConstant _) _ (IntConstant _)) = Nothing
  leftMostMult term@(BinaryTerm lhv op rhv)
    | isJust leftTree = leftTree
    | isJust rightTree = rightTree
    | otherwise = case op of (Mult) -> Just term
                             _      -> Nothing
    where
      leftTree = leftMostMult lhv
      rightTree = leftMostMult rhv

  leftMostOp :: Term -> Term
  leftMostOp term@(BinaryTerm (IntConstant _) op (IntConstant _)) = term
  leftMostOp (BinaryTerm lhv _ _) = leftMostOp lhv

  leftMostMajorOpGetter :: Term -> Term
  leftMostMajorOpGetter term
    | isJust mult = fromJust mult
    | otherwise = leftMostOp term
    where
      mult = leftMostMult term

  leftMostOpSetter :: Term -> Term -> Term
  leftMostOpSetter (BinaryTerm (IntConstant _) _ (IntConstant _)) newTerm = newTerm
  leftMostOpSetter (BinaryTerm lhv op rhv) newTerm = BinaryTerm (leftMostOpSetter lhv newTerm) op rhv

  leftMostMajorOpSetter :: Term -> Term -> Term
  leftMostMajorOpSetter (BinaryTerm (IntConstant _) Mult (IntConstant _)) newTerm = newTerm
  leftMostMajorOpSetter term@(BinaryTerm lhv op rhv) newTerm
    | isJust $ leftMostMult lhv = BinaryTerm (leftMostMajorOpSetter lhv newTerm) op rhv
    | isJust $ leftMostMult rhv = BinaryTerm lhv op (leftMostMajorOpSetter rhv newTerm)
    | otherwise = leftMostOpSetter term newTerm


  {---------------------------------------------}
  {-Функции для получения самого крайнего терма-}
  {---------------------------------------------}

  rightMostTermGetter :: Term -> Term
  rightMostTermGetter (BinaryTerm _ _ term) = rightMostTermGetter term
  rightMostTermGetter (UnaryTerm _ term) = rightMostTermGetter term
  rightMostTermGetter term = term

  rightMostTermSetter :: Term -> Term -> Term
  rightMostTermSetter (BinaryTerm lhv op rhv) newTerm =
    BinaryTerm lhv op (rightMostTermSetter rhv newTerm)
  rightMostTermSetter (UnaryTerm op term) newTerm =
    UnaryTerm op (rightMostTermSetter term newTerm)
  rightMostTermSetter _ newTerm = newTerm
