module AuxParsers where

import           Data.Functor.Identity                    (Identity)
import           Text.Parsec.Prim                         as Prim (ParsecT,
                                                                   unexpected)
import           Text.ParserCombinators.Parsec.Char       (char, digit, letter,
                                                           string)

import           Text.ParserCombinators.Parsec.Combinator (many1)
import           Control.Applicative

parO :: ParsecT String u Identity String
parO = string "("

parC :: ParsecT String u Identity String
parC = string ")"

sum :: ParsecT String u Identity Char
sum = char '+'

sub :: ParsecT String u Identity Char
sub = char '-'

vig :: ParsecT String u Identity Char
vig = char ','

mul :: ParsecT String u Identity Char
mul = char '*'

div :: ParsecT String u Identity Char
div = char '/'

equal :: ParsecT String u Identity String
equal = string "=="

diff :: ParsecT String u Identity String
diff = string "!="

lt :: ParsecT String u Identity Char
lt = char '<'

gt :: ParsecT String u Identity Char
gt = char '>'

leq :: ParsecT String u Identity String
leq = string "<="

geq :: ParsecT String u Identity String
geq = string ">="

bracketO :: ParsecT String u Identity Char
bracketO = char '{'

bracketC :: ParsecT String u Identity Char
bracketC = char '}'

squareO :: ParsecT String u Identity Char
squareO = char '['

squareC :: ParsecT String u Identity Char
squareC = char ']'

integer :: ParsecT String u Identity Int
integer  = read <$> many1 digit

symbol :: ParsecT String u Identity String
symbol = many1 letter >>= symb'


symb' :: String -> ParsecT String u Identity String
symb' "while" = unexpected "Private Word while!"
symb' "if" = unexpected "Private Word if!"
symb' "else" = unexpected "Private Word else!"
symb' "inv" = unexpected "Private Word inv!"
symb' "pre" = unexpected "Private Word pre!"
symb' "pos" = unexpected "Private Word pos!"
symb' "~" = unexpected "Private Word not!"
symb' val = return  val

pAnd :: ParsecT String u Identity String
pAnd = string "&&"

pOr :: ParsecT String u Identity String
pOr = string "||"

pImp :: ParsecT String u Identity String
pImp = string "imp"

pNot :: ParsecT String u Identity String
pNot = string "~"

eq :: ParsecT String u Identity Char
eq = char '='

true :: ParsecT String u Identity String
true = string "True"

false :: ParsecT String u Identity String
false = string "False"

sep :: ParsecT String u Identity Char
sep = char ';'

while :: ParsecT String u Identity String
while = string "while"

pIf :: ParsecT String u Identity String
pIf = string "if"

pElse :: ParsecT String u Identity String
pElse = string "else"

inv :: ParsecT String u Identity String
inv = string "inv:"

pos :: ParsecT String u Identity String
pos = string "pos:"

pre :: ParsecT String u Identity String
pre = string "pre:"
