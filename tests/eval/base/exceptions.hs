-- Exceptions: throw, throwIO, catch, handle, try, evaluate, bracket,
-- finally and onException.
import Control.Exception
import GHC.Num.Integer

errorCall :: ErrorCall -> IO ()
errorCall (ErrorCall m) = putStrLn ("caught: " ++ m)

arith :: ArithException -> IO ()
arith DivideByZero = putStrLn "caught: divide by zero"
arith _ = putStrLn "caught: another arithmetic exception"

anything :: SomeException -> IO ()
anything _ = putStrLn "caught: some exception"

tryArith :: IO Int -> IO (Either ArithException Int)
tryArith = try

tryError :: IO Int -> IO (Either ErrorCall Int)
tryError = try

report :: Either ErrorCall Int -> IO ()
report (Left (ErrorCall m)) = putStrLn ("Left " ++ m)
report (Right n) = putStrLn ("Right " ++ show n)

main :: IO ()
main = do
  -- error and throw in pure code, throwIO in IO.
  catch (evaluate (error "boom" + 1) >>= print) errorCall
  catch (print (length [1, 2, throw (ErrorCall "not forced")])) errorCall
  catch (print (throw (ErrorCall "thrown") + 2)) errorCall
  catch (throwIO (ErrorCall "from IO") >> putStrLn "not reached") errorCall
  handle errorCall (putStrLn "no exception")
  -- Division by zero.
  catch (print (7 `div` 0)) arith
  catch (print (7 `mod` 0)) arith
  handle anything (print (snd (quotRem 7 0)))
  -- Integer division by zero, also with a large dividend.
  let big = integerMul (integerFromInt 4294967296) (integerFromInt 4294967296)
      zero = integerFromInt 0
  catch (print (integerToInt (integerQuot (integerFromInt 7) zero))) arith
  catch (print (integerToInt (integerDiv big zero))) arith
  handle anything (print (integerToInt (fst (integerDivMod big zero))))
  print (integerToInt (integerDiv (integerFromInt 7) (integerFromInt 2)))
  -- try returns the exception or the result.
  tryArith (evaluate (10 `div` 2)) >>= either arith print
  tryArith (evaluate (10 `div` 0)) >>= either arith print
  tryError (return (error "lazy")) >>= \r -> case r of
    Left _ -> putStrLn "Left"
    Right _ -> putStrLn "Right: return does not force its value"
  tryError (evaluate (head [])) >>= either (\_ -> putStrLn "Left: head") print
  -- A thunk that raises an exception raises it again on the next use.
  let t = error "again" :: Int
  tryError (evaluate t) >>= report
  tryError (evaluate (t + 1)) >>= report
  -- Nested handlers: the innermost handler runs, and it can raise again.
  catch
    (catch (throwIO (ErrorCall "inner")) (\(ErrorCall m) -> throwIO (ErrorCall (m ++ " again"))))
    errorCall
  -- bracket releases the resource on success and on an exception.
  n <- bracket (putStrLn "acquire 1" >> return 6) (\_ -> putStrLn "release 1") (\x -> return (x * 7))
  print n
  r <- tryError (bracket (putStrLn "acquire 2" >> return 1) (\_ -> putStrLn "release 2") (\x -> print x >> throwIO (ErrorCall "in use") >> return x))
  report r
  bracket_ (putStrLn "acquire 3") (putStrLn "release 3") (putStrLn "use 3")
  -- finally and onException.
  (putStrLn "body 1" `finally` putStrLn "finally 1") `catch` errorCall
  (throwIO (ErrorCall "body 2") `finally` putStrLn "finally 2") `catch` errorCall
  (putStrLn "body 3" `onException` putStrLn "not run") `catch` errorCall
  (throwIO DivideByZero `onException` putStrLn "on exception 4") `catch` arith
  -- The program goes on after a handler.
  mapM_ (\i -> catch (print (100 `div` i)) arith) [5, 0, 4]
  putStrLn "done"
