-- -*- haskell -*-
{
{-# OPTIONS_GHC -w #-}
module Verismith.Verilog.Lex
  ( alexScanTokens
  ) where

import Verismith.Verilog.Token

}

%wrapper "posn"

-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]
@binaryDigit  = [0-1]
@octalDigit   = [0-7]
@hexDigit     = [0-9a-fA-F]

@decimalBase = "'" [dD]
@binaryBase  = "'" [bB]
@octalBase   = "'" [oO]
@hexBase     = "'" [hH]

@binaryValue         = @binaryDigit ("_" | @binaryDigit)*
@octalValue  = @octalDigit  ("_" | @octalDigit)*
@hexValue    = @hexDigit    ("_" | @hexDigit)*

@unsignedNumber = $decimalDigit ("_" | $decimalDigit)*

@size = @unsignedNumber

@decimalNumber
  = @unsignedNumber
  | @size? @decimalBase @unsignedNumber

@binaryNumber = @size? @binaryBase @binaryValue
@octalNumber  = @size? @octalBase  @octalValue
@hexNumber    = @size? @hexBase    @hexValue
  
-- $exp  = [eE]
-- $sign = [\+\-]
-- @realNumber = unsignedNumber "." unsignedNumber | unsignedNumber ( "." unsignedNumber)? exp sign? unsignedNumber
@number = @decimalNumber | @octalNumber | @binaryNumber | @hexNumber

-- Strings

@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  "always"           { tok KWAlways     }
  "assign"           { tok KWAssign     }
  "begin"            { tok KWBegin      }
  "case"             { tok KWCase       }
  "default"          { tok KWDefault    }
  "else"             { tok KWElse       }
  "end"              { tok KWEnd        }
  "endcase"          { tok KWEndcase    }
  "endmodule"        { tok KWEndmodule  }
  "for"              { tok KWFor        }
  "if"               { tok KWIf         }
  "initial"          { tok KWInitial    }
  "inout"            { tok KWInout      }
  "input"            { tok KWInput      }
  "integer"          { tok KWInteger    }
  "localparam"       { tok KWLocalparam }
  "module"           { tok KWModule     }
  "negedge"          { tok KWNegedge    }
  "or"               { tok KWOr         }
  "output"           { tok KWOutput     }
  "parameter"        { tok KWParameter  }
  "posedge"          { tok KWPosedge    }
  "reg"              { tok KWReg        }
  "wire"             { tok KWWire       }
  "signed"           { tok KWSigned     }

  @simpleIdentifier  { tok IdSimple             }
  @escapedIdentifier { tok IdEscaped            }
  @systemIdentifier  { tok IdSystem             }

  @number            { tok LitNumber            }
  @string            { tok LitString            }

  "("                { tok SymParenL            }
  ")"                { tok SymParenR            }
  "["                { tok SymBrackL            }
  "]"                { tok SymBrackR            }
  "{"                { tok SymBraceL            }
  "}"                { tok SymBraceR            }
  "~"                { tok SymTildy             }
  "!"                { tok SymBang              }
  "@"                { tok SymAt                }
  "#"                { tok SymPound             }
  "%"                { tok SymPercent           }
  "^"                { tok SymHat               }
  "&"                { tok SymAmp               }
  "|"                { tok SymBar               }
  "*"                { tok SymAster             }
  "."                { tok SymDot               }
  ","                { tok SymComma             }
  ":"                { tok SymColon             }
  ";"                { tok SymSemi              }
  "="                { tok SymEq                }
  "<"                { tok SymLt                }
  ">"                { tok SymGt                }
  "+"                { tok SymPlus              }
  "-"                { tok SymDash              }
  "?"                { tok SymQuestion          }
  "/"                { tok SymSlash             }
  "$"                { tok SymDollar            }
  "'"                { tok SymSQuote            }

  "~&"               { tok SymTildyAmp          }
  "~|"               { tok SymTildyBar          }
  "~^"               { tok SymTildyHat          }
  "^~"               { tok SymHatTildy          }
  "=="               { tok SymEqEq              }
  "!="               { tok SymBangEq            }
  "&&"               { tok SymAmpAmp            }
  "||"               { tok SymBarBar            }
  "**"               { tok SymAsterAster        }
  "<="               { tok SymLtEq              }
  ">="               { tok SymGtEq              }
  ">>"               { tok SymGtGt              }
  "<<"               { tok SymLtLt              }
  "++"               { tok SymPlusPlus          }
  "--"               { tok SymDashDash          }
  "+="               { tok SymPlusEq            }
  "-="               { tok SymDashEq            }
  "*="               { tok SymAsterEq           }
  "/="               { tok SymSlashEq           }
  "%="               { tok SymPercentEq         }
  "&="               { tok SymAmpEq             }
  "|="               { tok SymBarEq             }
  "^="               { tok SymHatEq             }
  "+:"               { tok SymPlusColon         }
  "-:"               { tok SymDashColon         }
  "::"               { tok SymColonColon        }
  ".*"               { tok SymDotAster          }
  "->"               { tok SymDashGt            }
  ":="               { tok SymColonEq           }
  ":/"               { tok SymColonSlash        }
  "##"               { tok SymPoundPound        }
  "[*"               { tok SymBrackLAster       }
  "[="               { tok SymBrackLEq          }
  "=>"               { tok SymEqGt              }
  "@*"               { tok SymAtAster           }
  "(*"               { tok SymParenLAster       }
  "*)"               { tok SymAsterParenR       }
  "*>"               { tok SymAsterGt           }

  "==="              { tok SymEqEqEq            }
  "!=="              { tok SymBangEqEq          }
  "=?="              { tok SymEqQuestionEq      }
  "!?="              { tok SymBangQuestionEq    }
  ">>>"              { tok SymGtGtGt            }
  "<<<"              { tok SymLtLtLt            }
  "<<="              { tok SymLtLtEq            }
  ">>="              { tok SymGtGtEq            }
  "|->"              { tok SymBarDashGt         }
  "|=>"              { tok SymBarEqGt           }
  "[->"              { tok SymBrackLDashGt      }
  "@@("              { tok SymAtAtParenL        }
  "(*)"              { tok SymParenLAsterParenR }
  "->>"              { tok SymDashGtGt          }
  "&&&"              { tok SymAmpAmpAmp         }

  "<<<="             { tok SymLtLtLtEq          }
  ">>>="             { tok SymGtGtGtEq          }

  $white             ;

  .                  { tok Unknown              }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}
