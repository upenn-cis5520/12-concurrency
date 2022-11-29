{-
---
fulltitle: "In class exercise: Concurrency Monad Transformer"
date: November 30, 2022
---
-}

module TransC where

import Control.Monad (ap, liftM, (>=>))
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadTrans (..))
{-
This exercise depends on the `Sequence` datatype from Haskell's containers
library.  This data structure implements efficient sequential data, with
logorithmic indexing and append operations and constant time access to the
head and tail of the data structure.
-}

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.IO as IO
{-
Note, today is all about testing.
-}

import Test.HUnit (Test, runTestTT, (~?=))

{-
Testing IO interactions
-----------------------
-}

-- a)

{-
The Input and Output classes from the [Concurrency](../soln/11-concurrency/Concurrency.html) lecture
are useful not just for implementing concurrent programs, but they are also a
way that we can test monadic computations that do console I/O.

Here are the definitions that we saw before, describing monads that support
non-blocking I/O.
-}

class Monad m => Output m where
  write :: String -> m ()

class Monad m => Input m where
  input :: m (Maybe String) -- only return input if it is ready

instance Output IO where
  write = putStr

instance Input IO where
  input = do
    x <- IO.hReady IO.stdin
    if x then Just <$> getLine else return Nothing

{-
And here is a simple program that does some IO in an arbitrary monad.
-}

-- | Wait for some input to appear, and when it does, repeat it.
echo :: (Input m, Output m) => m ()
echo = do
  ms <- input
  case ms of
    Just str -> write str >> write "\n"
    Nothing -> echo

{-
If I run this program in ghci, using the `IO` monad by default, I can see that
it just spits out whatever I type in.

            Main*> echo
            Hello        <---- I typed this
            Hello        <---- GHCi printed this

Try it out yourself!

But how can we make a unit test for this (simple) program?

The answer is that we will *mock* the IO monad using a different, pure monad.
Below is a definition of a `FakeIO` monad --- it is just a state monad with
two components: a log of all of the lines that were written to the output
and a list of all inputs, which may or may not be ready.
-}

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrite :: Seq String, -- what has been written
    fsInput :: [Maybe String] -- what to read from
  }

{-
We will eventually be able to run this monad by giving it a list
of inputs and it will give us back the log of `write`s.
-}

-- >>> runFakeIO echo [Nothing, Nothing, Just "Hello"]

{-
Should produce the output

      ["hello", "\n"]

We can make this `FakeIO` monad an instance of our two classes by
remembering all of the strings that were written
and by providing access to the inputs, one by one.
-}

instance Output FakeIO where
  write s = do
    st <- S.get
    let oldLog = fsWrite st
    let newLog = oldLog <> Seq.singleton s
    S.put $ st {fsWrite = newLog}

{-
Complete the definition of the input function so that it accesses the
`fsInput` component of current state, returning the head (if any) and updating
`fsInput` to be the tail of the list.
-}

instance Input FakeIO where
  input = undefined

{-
We can run the `FakeIO` monad by giving it an initial state.
-}

runFakeIO :: FakeIO () -> [Maybe String] -> [String]
runFakeIO comp inputs =
  toList (fsWrite (S.execState comp initState))
  where
    initState = FS {fsWrite = Seq.empty, fsInput = inputs}

{-
Here are two examples of unit tests for IO programs.
-}

testEcho :: Test
testEcho =
  runFakeIO
    echo
    [Nothing, Nothing, Just "hello"]
    ~?= ["hello", "\n"]

-- >>> runTestTT testEcho

testEcho2 :: Test
testEcho2 =
  runFakeIO
    (echo >> echo)
    [Just "hello", Nothing, Just "CIS 5520"]
    ~?= ["hello", "\n", "CIS 5520", "\n"]

-- >>> runTestTT testEcho2

{-
Now write a test of your own, for a simple IO progam of your own devising.
-}

test3 :: Test
test3 = runFakeIO undefined undefined ~?= undefined

-- >>> runTestTT test3

{-
A generic concurrency monad
---------------------------
-}

-- b)

{-
The Concurrency monad that we presented in class was specialized to atomic
actions in the IO monad. But now that we have a mocked version of the IO
monad, we should be more general. Compare this definition of `Action` to the
one from before; this one is *parameterized* by a monad in which the atomic
actions are run.
-}

data Action m
  = Atom (m (Action m)) -- an atomic computation, returning a new action
  | Fork (Action m) (Action m) -- create a new thread
  | Stop -- terminate this thread

{-
We add this new `m` as an additional argument to `C`.
-}

newtype C m a = C {runC :: (a -> Action m) -> Action m}

{-
Now, make this new type a monad:
-}

instance Monad m => Monad (C m) where
  return x = undefined
  m >>= f = undefined

instance Monad m => Applicative (C m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (C m) where
  fmap = liftM

-- c)

{-
Next, to make sure you follow how these generalizations work, add the type
signatures for our library of concurrency operations. Of course, vscode will
just add them with a click, but make sure that you understand why these operations
have the types that they do.
-}

atom m = C $ \k -> Atom (fmap k m)

run m = sched [runC m $ const Stop]

fork m = C $ \k -> Fork (runC m $ const Stop) (k ())

sched [] = return ()
sched (Atom m : cs) = m >>= \a -> sched (cs ++ [a])
sched (Fork a1 a2 : cs) = sched (cs ++ [a1, a2])
sched (Stop : cs) = sched cs

{-
Testing concurrent IO
---------------------
-}

-- d)

{-
Next, show how to implement input and output for
this new parameterized concurrency monad.
-}

instance Input m => Input (C m) where
  input = undefined

instance Output m => Output (C m) where
  write = undefined

{-
(More generally, note that `C` is a *monad transformer*. We can make the
concurrency monad an instance of the monad transformers class, which will
allow it to work gracefully with other monad transformers.)
-}

instance MonadTrans C where
  lift = atom

{-
Let's define and test a *concurrent* program that does IO.

For example, given an output function:
-}

example :: Output m => C m ()
example = do
  fork (write "Hello " >> write "5520")
  write "CIS"

{-
We can run it in the IO monad

          ghci>  run (example :: C IO ())
          Hello CIS5520

Or run it in the *Concurrent* FakeIO monad.
-}

runCFakeIO :: C FakeIO () -> [Maybe String] -> [String]
runCFakeIO x inputs = undefined

testWrite :: Test
testWrite = runCFakeIO example [] ~?= ["Hello ", "CIS", "5520"]

-- >>> runTestTT testWrite

{-
Write your own example of a (terminating) concurrent program, and a test
demonstrating what it does.
-}

example2 :: (Input m, Output m) => C m ()
example2 = undefined

testExample2 :: Test
testExample2 = undefined

-- >>> runTestTT testExample2
