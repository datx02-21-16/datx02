module GUI.Rules (Rules(..)) where

{-
The other rules we already have in the repository contains some numbers to
indicate what operands to apply a rule to. When we click a button in the GUI we
don't (yet) have that information, so I have added this sum type here to be able
to differentiate between clicks on different buttons.
-}
data Rules
  = AndElim1
  | AndElim2
  | AndIntro
  | OrIntro
  | NotIntro
  | NotElim
