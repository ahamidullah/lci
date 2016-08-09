import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec
import System.Console.Readline (readline)

repl :: IO ()
repl = do maybeLine <- readline "expr?> "
          case maybeLine of
            Nothing     -> return ()
            Just "exit" -> return ()
            Just line   -> case parseExpr line of
                             Just expr -> do printExpr $ eval (expr, Map.fromList [])
                                             repl
                             Nothing   -> putStrLn "syntax error"
--main = do exprStr <- getInputLine "expr?"
--          printExpr $ eval (parseExpr exprStr, Map.fromList [])

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
parseExpr :: String -> Maybe Expr
parseExpr s = case parse expr "(unknown)" s of
                Right a -> Just a
                Left _ -> Nothing

defSpaces :: Parser [Char]
defSpaces = many1 $ char ' '

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
app = do fn <- expr
         spaces
         args <- (sepEndBy1 expr defSpaces)
         spaces
         return $ foldl (\f a -> Application f a) fn args

var :: Parser Char
var = noneOf " .\\()"

-- Printing
printExpr :: Expr -> IO ()
printExpr e = putStrLn $ "beta reduction: " ++ exprToStr e

exprToStr :: Expr -> String
exprToStr (Application e1 e2)  = "(" ++ (exprToStr e1) ++ (exprToStr e2) ++ ")"
exprToStr (Function name body) = "(" ++ "\\" ++ name:[] ++ "." ++ (exprToStr body) ++ ")"
exprToStr (Variable name)      = name:[]

