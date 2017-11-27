module Core.Typesystem
    ( VariableName
    , EvalContext
    ) where

import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)

type VariableName = ByteString

type EvalContext a = Map VariableName a
