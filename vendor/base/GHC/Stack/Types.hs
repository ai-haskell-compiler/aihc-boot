-- | Call stacks. The boot base keeps no call stacks: 'HasCallStack' is
-- a class without parameters, and every function has an instance.
module GHC.Stack.Types (HasCallStack) where

class HasCallStack

instance HasCallStack
