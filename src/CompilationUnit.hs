module CompilationUnit ( CompilationUnit(..), MetaData(..) )
where

import Function ( Function(..) )


newtype MetaData = MetaData { extras :: [(String, String)] }
    deriving (Show, Read)

-- by default everything is public
data CompilationUnit = CompilationUnit { name :: String
                                       , metaData :: MetaData
                                       , functions :: [Function]
                                       } deriving (Show, Read)
