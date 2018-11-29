module Test.VeriFuzz.VerilogAST where

data ModuleItem = 

-- | 'module' module_identifier [list_of_ports] ';' { module_item } 'end_module'
data ModuleDecl = ModuleDecl { moduleId   :: Text
                             , ports      :: [Port]
                             , moduleItem :: ModuleItem
                             }

type Description = ModuleDecl

type SourceText = [Description]
