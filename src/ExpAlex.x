{ module ExpAlex where}

%wrapper "basic"

$symb = [a-z]
$digit = [0-9]

tokens :-
  $white+ ;
  $digit+ {\s -> Number $ read s}
  "|" {\_ -> Vert}
  "&" | "And" | "∧" {\_ -> And}
  "¬" | "Not" | "~" | "!" {\_ -> Non}
  "Bot" | "⊥" {\_ -> Bot}
  "Top" | "⊤" {\_ -> Top}
  "Or" | "∨" {\_ -> Or}
  "*" {\_ -> Star}
  "+" {\_ -> Somme}
  "."|"·" {\_ -> Conc}
  $symb {\s -> Symb s}
  "Empty" | "∅" {\_ -> Vide}
  "Epsilon" | "ε"  {\_ -> Epsilon}
  "(" {\_ -> ParO}
  ")" {\_ -> ParF}
  "," {\_ -> Virg}
  . {\_ ->  Autre}

{data Token
  =	And
  | Vert
  | Non
  | Bot
  | Top
  | Or
  | Star
  | Somme
  | Conc
  | Symb String
  | Number Integer
  | Vide
  | Epsilon
  | ParO
  | ParF
  | Virg
  | Autre
	deriving (Eq, Show)
}
