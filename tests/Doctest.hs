import           Test.DocTest

main =
  doctest ["-XOverloadedStrings", "-isrc", "src/Test/VeriFuzz/Verilog/Mutate.hs"]
