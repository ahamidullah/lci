import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec

main :: IO ()
main = do exprStr <- getLine
          putStrLn $ printExpr $ eval (parseExpr exprStr, Map.fromList [])

type Name = Char
data Expr = Variable Name | Function Name Expr | Application Expr Expr
        deriving (Show)
type Bindings = Map.Map Name Expr

eval :: (Expr, Bindings) -> Expr
eval (expr, b) = go expr
        where go (Variable name)             = substitute name
              go (Application fn arg)        = eval $ beta fn (eval (arg, b))
              go (Function name body)        = Function name (eval (body, Map.delete name b))
              beta (Function name body) arg  = (body, Map.insert name arg b)
              beta (Variable name) arg       = if Map.member name b
                                                  then (Application (substitute name) arg, b)
                                                  else error "applying free variable"
              beta app@(Application _ _) arg = (Application (eval (app, b)) arg, b)
              substitute name                = Map.findWithDefault (Variable name) name b

-- Parsing
parseExpr :: String -> Expr
parseExpr s = case parse expr "(unknown)" s of
                Right a -> a
                Left _ -> error "syntax error"

defSpaces :: Parser Char
defSpaces = char ' '

expr :: Parser Expr
expr = (do char '\\'
           vl <- (sepEndBy1 var defSpaces)
           char '.'
           spaces
           e <- expr
           return $ foldr (\var fn -> Function var fn) e vl)
       <|> (do char '('
               e <- try app <|> expr
               char ')'
               return e)
       <|> do v <- var
              return $ Variable v

app :: Parser Expr
app = do e1 <- expr
         spaces
         e2 <- expr
         spaces
         return $ Application e1 e2

var :: Parser Char
var = noneOf " .\\()"

-- Printing
printExpr :: Expr -> String
printExpr (Application e1 e2)  = "(" ++ (printExpr e1) ++ (printExpr e2) ++ ")"
printExpr (Function name body) = "(" ++ "\\" ++ name:[] ++ "." ++ (printExpr body) ++ ")"
printExpr (Variable name)      = name:[]

