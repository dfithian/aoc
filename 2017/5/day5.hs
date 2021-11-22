import ClassyPrelude
import qualified Data.Array as A
import Data.List (unfoldr)
import Data.Text (strip)

type IntArray = A.Array Int Int

doJump :: FilePath -> IO Int
doJump path = do
  input <- unpack . strip <$> readFileUtf8 path
  xs <- forM (lines input) $ \ x ->
    maybe (fail $ "not a digit: " <> x) pure $ readMay x
  let initial = A.listArray (0, length xs - 1) xs
  pure . length $ unfoldr (\ (i, next) -> ((),) <$> jump i next) (0, initial)

jump :: Int -> IntArray -> Maybe (Int, IntArray)
jump i xs =
  case A.inRange (A.bounds xs) i of
    False -> Nothing
    True ->
      let step = xs A.! i
      in Just (i + step, xs A.// [(i, step + 1)])
