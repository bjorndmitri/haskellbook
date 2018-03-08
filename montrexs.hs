import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

rDec :: Num a => ReaderT a Identity a
rDec = ReaderT $ Identity <$> (+(-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity <$> show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
    putStrLn $ "Hi: " ++ show r
    return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    putStrLn $ "Hi: " ++ show s
    return $ (show s, s + 1)   

isValid :: String -> Bool
isValid = (elem) '!'

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- lift getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn $ "Good, was very excite: " ++ e
