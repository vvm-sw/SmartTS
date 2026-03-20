module SmartTS.Interpreter where

import Control.Monad.State as S
import Data.Map.Strict as M

import SmartTS.AST

-- | Environment maps contract addresses to their storage values.
-- In Tezos, each contract instance has its own storage state.
type Env = M.Map Name Expr

type Execution a = S.State Env a

-- | Generate a unique contract address.
-- In a real implementation, this would use cryptographic hashing (e.g., Base58 encoding).
-- For now, we generate a simple address based on contract name and instance counter.
generateAddress :: Contract -> Int -> Name
generateAddress c counter = 
  let name = contractName c
  in "KT1" ++ name ++ "_" ++ show counter

-- | Instantiate a contract by executing its @originate method.
-- The @originate method initializes the storage and returns the initial storage value.
originate :: Contract -> Expr -> Execution Name
originate c initialStorage = do
  -- Find the originate method
  let origMethods = findMethods Originate c
  case origMethods of
    [] -> error "Contract must have exactly one @originate method"
    [_] -> do
      -- TODO: Execute methodBody origMethod with initialStorage to compute final storage
      -- For now, we store the initial storage value directly
      -- In Tezos, the @originate method would run and update the storage
      env <- S.get
      let counter = M.size env -- Use env size as counter for address generation
          address = generateAddress c counter
      -- Store the contract instance's initial storage
      S.modify (M.insert address initialStorage)
      return address
    _ -> error "Contract must have exactly one @originate method"
