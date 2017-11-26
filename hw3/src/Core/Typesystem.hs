module Core.Typesystem
    ( VariableName
    , EvalContext
    ) where

import           Data.ByteString (ByteString)
import           Data.Map        (Map)

type VariableName = ByteString

type EvalContext a = Map VariableName a
