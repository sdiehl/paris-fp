{-# LANGUAGE TypeSynonymInstances #-}

module Pretty (
  pprint,
  pptenv,
) where

import Protolude hiding ((<>), empty)
import Text.PrettyPrint.Leijen.Text

import Types
import Syntax

instance Pretty a => Pretty (Program a) where
  pretty (Program decls) = vcat (fmap pretty decls)

instance Pretty a => Pretty (Decl a) where
  pretty x = case x of
    FunDecl fn args body ->
      "let"
      <+> pretty fn 
      <+> hsep (fmap pretty args)
      <+> "="
      <+> pretty body

instance Pretty a => Pretty (Expr a) where
  pretty = \case
    Var x        -> pretty x
    App x1 x2    -> parens (pretty x1 <+> pretty x2)
    Lam x1 x2    -> "\\" <> hcat (fmap pretty x1) <+> "->" <+> pretty x2
    Lit x        -> pretty x
    If x1 x2 x3  -> nest 2 $ 
      "if"
      <+> pretty x1
      <+> pretty x2 <//> "then"
      <+> pretty x3 <//> "else"
    Prim x       -> pretty x

instance Pretty Prim where
  pretty = \case
    Add -> "(+)"
    Sub -> "(-)"
    Mul -> "(*)"
    Eql -> "(==)"

instance Pretty Literal where
  pretty = \case
    LInt x  -> pretty x
    LBool x -> pretty x

instance Pretty PType where
  pretty (Forall [] t) = pretty t
  pretty (Forall xs t) = "forall " <> hsep (fmap pretty xs) <> " . " <> pretty t

instance Pretty MType where
  pretty = go False
    where
      go _ (TVar  i) = pretty i
      go _ (Con n) = pretty n
      go p (TArr t1 t2) = 
        let b = go True t1 <> " -> " <> go False t2
        in if p then "(" <> b <> ")" else b

ltext :: Name -> Doc
ltext = text . toS

pprint :: Pretty a => a -> LText
pprint = displayT . renderPretty 0.4 100 . pretty

pptenv :: Either Text [(Name, PType)] -> LText
pptenv (Left msg) = toS msg
pptenv (Right env) = pprint (vcat ([pretty nm <+> "::" <+> pretty ty | (nm, ty) <- env]))
