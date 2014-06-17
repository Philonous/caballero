# Caballero

Make cabal files work for you

By default the following keys are defined for haskell-cabal mode:

* C-c s : Sort and indent lines in the current subsection. customize-variable
  caballero/list-comma-position if you prefer the commas to be at the end of
  each line

* C-c f : On a line with a module or a filename: Open the file this line refers
  to in another window.

* C-n, C-p : Go to the next (previous) subsection
* C-M-n, C-M-p : Go to the next (previous) section

* M-g l : Go to the next library section
* M-g e : Go to the next executable section
* M-g t : Go to the next test-suite section
* M-g b : Go to the next benchmark section
