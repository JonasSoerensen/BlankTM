module Machine where
import Numeric.Natural (Natural)
import Data.Map
import Prelude hiding (head, lookup, Left, Right)
import Control.Monad.State
import Data.Maybe (fromMaybe)

-- | The positions that the head of the machine may occupy
type Position = Natural

-- | The combination of the head position and the tape
data Machine a = Machine{
    head :: Position
    -- | Maps are the most ideal, lists end up having to copy the whole list if the machine moves right, and finger trees are bounded
    ,tape :: Map Position a
}

-- | Get the symbol of the current 
headSymbol :: a -> Machine a -> a
headSymbol blank machine = fromMaybe blank (lookup (head machine) (tape machine))

-- | The Direction that the head can move
data Direction = Left | Right

-- | Move a Natural number by a supplied direction
move :: Natural -> Direction -> Natural
move 0 Left = 0
move n Left = n - 1
move n Right = n + 1

-- | Run the machine indefinetly
run :: (a -> b -> (a, b, Direction)) -> b -> a -> State (Machine b) [(a, b)]
run transition blank state = do
    machine <- get
    let symbol = headSymbol blank machine 
        (state', symbol', direction) = state `transition` symbol 
        pos' = head machine `move` direction
        tape' = pos' `insert` symbol' $ tape machine
    
    put $ Machine pos' tape'

    ((state,symbol):) <$> run transition blank state'
