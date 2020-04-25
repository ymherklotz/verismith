{-|
Module      : Verismith.Verilog.Distance
Description : Definition of the distance function for the abstract syntax tree.
Copyright   : (c) 2020, Yann Herklotz
License     : GPL-3
Maintainer  : yann [at] yannherklotz [dot] com
Stability   : experimental
Poratbility : POSIX

Define the distance function for the abstract syntax tree, so that different
Verilog files can be compared.  This allows us to define a metric on how
different two pieces of Verilog are.  Currently, differences in expressions are
ignored, as these are not that interesting.
-}
