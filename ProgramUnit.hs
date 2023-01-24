module ProgramUnit ( ProgramUnit(..) )
where

import Function ( Function(..) )

-- by default everything is public
data ProgramUnit = ProgramUnit { functions :: [Function]
                            --    , structs :: [Struct]
                               }
                               deriving (Show, Read)
