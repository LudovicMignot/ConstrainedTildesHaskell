{

module ExpHappy where

import Control.Applicative (liftA2)
import ExpAlex as A
import Exp as E
import BoolForm as B
import Data.Maybe (listToMaybe)
import Control.Monad(join)

}

%name expfromString Exp

%tokentype { Token }
%error { parseError }
%left somme
%left conc
%left star
%left ou
%left et
%left non

%monad {Maybe}

%token
  virg {A.Virg}
  ou {A.Or}
  et {A.And}
  non {A.Non}
  somme {A.Somme}
  conc {A.Conc}
  star {A.Star}
  symb {A.Symb $$}
  atom {A.Number $$}
  vide {A.Vide}
  eps {A.Epsilon}
  po {A.ParO}
  pf {A.ParF}
  bot {A.Bot}
  top {A.Top}
  vert {A.Vert}

%%
Exp :: {Maybe (Exp Char)}
  : Exp somme Exp {
    liftA2 Sum $1 $3
  }
  | Exp conc Exp {
    liftA2 E.Concat $1 $3
  }
  | Exp star { fmap E.Star $1}
  | po Exp pf {$2}
  | vide {Just E.Empty}
  | eps {Just E.Epsilon}
  | symb { fmap E.Symbol (listToMaybe $1)}
  | vert Form vert ExpList {
    join $ liftA2 E.consTilde $2 (sequence $4)
  }

Form :: {Maybe (BoolForm Integer)}
  : Form et Form {liftA2 B.And $1 $3}
  | Form star Form {liftA2 B.And $1 $3}
  | Form ou Form {liftA2 B.Or $1 $3}
  | po Form pf {$2}
  | non Form { fmap B.Not $2}
  | top {Just B.Top}
  | bot {Just B.Bot}
  | atom {Just $ Atom $1}


ExpList :: {[Maybe (Exp Char)]}
  : po pf {[]}
  | po NonEmptyList pf {
    $2
  }

NonEmptyList :: {[Maybe (Exp Char)]}
  : Exp virg NonEmptyList {$1 : $3}
  | Exp {[$1]}


{
parseError _ = Nothing
}
