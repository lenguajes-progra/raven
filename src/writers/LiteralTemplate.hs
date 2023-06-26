module LiteralTemplate where
import Grammar

literalTemplate :: Literal -> String
literalTemplate lit = case lit of
  (IntegerLiteral il) -> show il
  (BooleanLiteral bl) -> show bl
  (StringLiteral sl) -> show sl
  (CharacterLiteral cl) -> show cl