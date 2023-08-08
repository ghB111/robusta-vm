module CompilationUnit ( CompilationUnit(..), MetaData(..) )
where

import Function ( Function(..) )


data MetaData = MetaData { unitName :: String, extras :: [(String, String)] }
    deriving (Show, Read)

-- by default everything is public
data CompilationUnit = CompilationUnit { metaData :: MetaData
                                       , functions :: [Function]
                                       } deriving (Show, Read)
