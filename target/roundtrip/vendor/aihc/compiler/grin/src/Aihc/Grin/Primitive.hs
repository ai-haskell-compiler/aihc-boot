{-# LANGUAGE OverloadedStrings #-}
module Aihc.Grin.Primitive (primitiveAllocates, allocatingPrimitives, primitiveHeapWords) where {
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
primitiveAllocates :: Text -> Bool;
primitiveAllocates name = name `Set.member` allocatingPrimitives;
primitiveHeapWords :: Text -> Maybe Int;
primitiveHeapWords name = lookup name [("stmWaitRequest#", 16), ("submitIORead#", 16), ("submitIOWrite#", 16), ("submitIOOpen#", 21), ("adoptIOHandle#", 5), ("stmBegin#", 3), ("writeTVar#", 4), ("newDelayTVar#", 8), ("newPromptTag#", 1), ("makeStableName#", 4), ("newMutVar#", 3), ("fork#", 9), ("newMVar#", 9), ("readMVar#", 5), ("takeMVar#", 5), ("putMVar#", 5)];
allocatingPrimitives :: Set Text;
allocatingPrimitives = Set.fromList (concat [arrayPrimitives, byteArrayPrimitives, referencePrimitives, transactionPrimitives, concurrencyPrimitives, controlPrimitives]);
arrayPrimitives :: [Text];
arrayPrimitives = ["newArray#", "cloneArray#", "cloneMutableArray#", "freezeArray#", "thawArray#", "newSmallArray#", "cloneSmallArray#", "cloneSmallMutableArray#", "freezeSmallArray#", "thawSmallArray#", "resizeSmallMutableArray#"];
byteArrayPrimitives :: [Text];
byteArrayPrimitives = ["newByteArray#", "newPinnedByteArray#", "newAlignedPinnedByteArray#", "resizeMutableByteArray#", "shrinkMutableByteArray#"];
referencePrimitives :: [Text];
referencePrimitives = ["newMutVar#", "makeStableName#"];
transactionPrimitives :: [Text];
transactionPrimitives = ["newTVar#", "readTVar#", "readTVarIO#", "writeTVar#", "newDelayTVar#", "stmBegin#", "stmCommit#", "stmAbort#", "stmActive#", "stmWaitRequest#", "stmWaitResult#"];
concurrencyPrimitives :: [Text];
concurrencyPrimitives = ["fork#", "myThreadId#", "yield#", "awaitIO#", "adoptIOHandle#", "submitIORead#", "submitIOWrite#", "submitIOOpen#", "newMVar#", "readMVar#", "takeMVar#", "putMVar#", "tryTakeMVar#", "tryPutMVar#", "newPromptTag#", "aihcControl0#", "aihcResume#"];
controlPrimitives :: [Text];
controlPrimitives = ["aihcExit#", "unsafeCoerce#", "raise#", "raiseIO#", "catch#", "runRW#", "keepAlive#", "seq#", "prompt#"]
}
