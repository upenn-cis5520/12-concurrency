{-# LANGUAGE DataKinds #-}
{-
---
fulltitle: "In class exercise: Concurrency Monads and Extensible Effects"
date: December 30, 2022
---
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module FreerC where

import Control.Monad (ap, liftM, (>=>))
import Control.Monad.Freer
import qualified Control.Monad.Freer as Effect
import qualified Control.Monad.Freer.Error as Effect
import qualified Control.Monad.Freer.Reader as Effect
import qualified Control.Monad.Freer.State as Effect
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
The InputMonad and OutputMonad classes from the [Concurrency](../soln/11-concurrency/Concurrency.html) lecture
are useful not just for implementing concurrent programs, but they are also a
way that we can test monadic computations that do console I/O.

Here are the definitions that we saw before, describing monads that support
non-blocking I/O.
-}

class Monad m => InputMonad m where
  input :: m (Maybe String) -- only return input if it is ready

class Monad m => OutputMonad m where
  write :: String -> m ()

{-
Testing IO interactions
-----------------------
-}

-- a)

data InputOutput a where
  Write :: String -> InputOutput ()
  Input :: InputOutput (Maybe String)

instance (Member InputOutput effs) => InputMonad (Eff effs) where
  input = send Input

instance (Member InputOutput effs) => OutputMonad (Eff effs) where
  write s = send (Write s)

interpretIO ::
  (LastMember IO effs, Member IO effs) =>
  Eff (InputOutput ': effs) ~> Eff effs
interpretIO = Effect.interpretM f
  where
    f :: InputOutput a -> IO a
    f Input = do
      x <- IO.hReady IO.stdin
      if x then Just <$> getLine else return Nothing
    f (Write s) = putStr s

{-
And here is a simple program that does some IO in an arbitrary monad.
-}

-- | Wait for some input to appear, and when it does, repeat it.
echo :: Member InputOutput m => Eff m ()
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

type FakeIO = Effect.State FakeState

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

interpretState ::
  Eff (InputOutput ': effs) ~> Eff (FakeIO ': effs)
interpretState = Effect.reinterpret f
  where
    f :: InputOutput a -> Eff (FakeIO ': effs) a
    f Input = do
      st <- Effect.get
      let (v, rest) = case fsInput st of
            [] -> (Nothing, [])
            (x : xs) -> (x, xs)
      Effect.put $ st {fsInput = rest}
      return v
    f (Write s) = do
      st <- Effect.get
      let oldLog = fsWrite st
      let newLog = oldLog <> Seq.singleton s
      Effect.put $ st {fsWrite = newLog}

{-
Complete the definition of the input function so that it accesses the
`fsInput` component of current state, returning the head (if any) and updating
`fsInput` to be the tail of the list.

We can run the `FakeIO` monad by giving it an initial state.
-}

runFakeIO :: Eff '[InputOutput] a -> [Maybe String] -> [String]
runFakeIO comp inputs =
  toList (fsWrite (Effect.run (Effect.execState initState g)))
  where
    g = interpretState comp
    initState :: FakeState
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
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

testEcho2 :: Test
testEcho2 =
  runFakeIO
    (echo >> echo)
    [Just "hello", Nothing, Just "CIS 552"]
    ~?= ["hello", "\n", "CIS 552", "\n"]

-- >>> runTestTT testEcho2
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

{-
Now write a test of your own, for a simple IO progam of your own devising.
-}

test3 :: Test
test3 = runFakeIO undefined undefined ~?= undefined

-- >>> runTestTT test3

{-
A Concurrency as an Effect
--------------------------
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

instance InputMonad m => InputMonad (C m) where
  input = undefined

instance OutputMonad m => OutputMonad (C m) where
  write = undefined

{-
(More generally, note that `C` is a *monad transformer*. We can make the
concurrency monad an instance of the monad transformers class, which will
allow it to work gracefully with other monad transformers.)
-}

{-
instance MonadTrans C where
    lift = atom

-}

{-
Let's define and test a *concurrent* program that does IO.

For example, given an output function:
-}

example :: Member InputOutput m => C (Eff m) ()
example = do
  fork (write "Hello " >> write "552")
  write "CIS"

{-
We can run it in the IO monad

          Main*>  run (example :: C IO ())
          Hello CIS552

Or run it in the *Concurrent* FakeIO monad.
-}

runCFakeIO :: C (Eff '[InputOutput]) () -> [Maybe String] -> [String]
runCFakeIO x inputs = undefined

testWrite :: Test
testWrite = runCFakeIO example [] ~?= ["Hello ", "CIS", "552"]

-- >>> runTestTT testWrite
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

{-
Write your own example of a (terminating) concurrent program, and a test
demonstrating what it does.
-}

--example2 :: (Input m, Output m) => C m ()
example2 = undefined

testExample2 :: Test
testExample2 = undefined

-- >>> runTestTT testExample2
