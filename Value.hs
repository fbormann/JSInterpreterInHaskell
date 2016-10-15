module Value (Value (..)) where
-- Import definition of Id from Syntax
import Language.ECMAScript3.Syntax
data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Nil
    | Continue
    | Break
    | Return Value
    | Double Double
    | Function Id [Id] [Statement]
    | Array [Value]
    | GlobalVar
    deriving (Eq) -- so we don't have to implement an instance of Eq :)

--
-- Pretty Printer
--
--instance Eq Value where
 --(Bool b1) == (Bool b2) = b1 == b2
 --(Continue) == (Continue) = True
 --(Break) == (Break) = True
 --_ == (Continue) = False
 --_ == (Break) = False


instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Nil = "undefined"
  show Break = "break"
  show Continue = "continue"
  show (Return n) = show n
  show (Double d) = show d
  show (Array a) = "[ " ++ (showArray (Array a)) ++ " ]"
  show (Function (Id id) args c) = "Function: " ++ id ++ " Arguments:" ++ showArgs args

--
-- Auxiliary functions
--

showArray (Array []) = ""
showArray (Array (x:xs)) | xs /= [] = (show x) ++ ", " ++ showArray (Array xs)
			 | otherwise = show x

showArgs [] = ""
showArgs ((Id id):xs) | xs /= [] = show id ++ "; " ++ showArgs xs
		      | otherwise = show id

  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)
