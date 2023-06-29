module LiteralTemplate where
import Grammar

literalTransformer :: Literal -> String
literalTransformer lit = case lit of
  (IntegerLiteral il) -> show il
  (BooleanLiteral bl) -> show bl
  (StringLiteral sl) -> show sl
  (CharacterLiteral cl) -> show cl