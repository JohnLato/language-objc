-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Toolkit.UNames
-- Copyright   :  (c) [1998..2003] Manuel M. T. Chakravarty
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Portability :  portable
--
-- Unique Names
--
--  Generates unqiue names according to a method of L. Augustsson, M. Rittri
--  & D. Synek ``Functional pearl: On generating unique names'', Journal of
--  Functional Programming 4(1), pp 117-123, 1994.
--
--  WARNING: DON'T tinker with the implementation!  It uses UNSAFE low-level 
--	     operations!
--
--
--  * This module provides an ordering relation on names (e.g., for using
--    `FiniteMaps'), but no assumption maybe made on the order in which names
--    are generated from the name space.  Furthermore, names are instances of
--    `Ix' to allow to use them as indicies.
--
--  * A supply should be used *at most* once to *either* split it or extract a 
--    stream of names.  A supply used repeatedly will always generate the same
--    set of names (otherwise, the whole thing wouldn't be referential
--    transparent).  
--
--  * If you ignored the warning below, looked at the implementation, and lost
--    faith, consider that laziness means call-by-need *and* sharing, and that
--    sharing is realized by updating evaluated thunks.
--
--  * ATTENTION: No clever CSE or unnecessary argument elimination may be
--    applied to the function `names'!
--
module Language.C.Toolkit.UNames (NameSupply, Name(..),
	       rootSupply, splitSupply, names, namesStartingFrom)
where
import Data.Ix
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef       (IORef, newIORef, readIORef, writeIORef)


-- Name supply definition (EXPORTED ABSTRACTLY)
--
newtype NameSupply = NameSupply (IORef Int)

-- Name (EXPORTED ABSTRACTLY)
--
newtype Name = Name Int
--             deriving (Show, Eq, Ord, Ix)
-- FIXME: nhc98, v1.08 can't derive Ix
             deriving (Eq, Ord)
instance Ix Name where
  range   (Name from, Name to)            = map Name (range (from, to))
  index   (Name from, Name to) (Name idx) = index   (from, to) idx
  inRange (Name from, Name to) (Name idx) = inRange (from, to) idx

namesStartingFrom :: Int -> [Name]
namesStartingFrom k = map Name [k..]

-- we want to show the number only, to be useful for generating unqiue
-- printable names
--
instance Show Name where
  show (Name i) = show i


--	  	      *** DON'T TOUCH THE FOLLOWING *** 
--  and if you believe in the lambda calculus better also don't look at it
--          ! here lives the daemon of unordered destructive updates !

-- The initial supply (EXPORTED)
--
rootSupply :: NameSupply
{-# NOINLINE rootSupply #-}
rootSupply  = NameSupply (unsafeNewIntRef 1)

-- Split a name supply into a stream of supplies (EXPORTED)
--
splitSupply   :: NameSupply -> [NameSupply]
splitSupply s  = repeat s

-- Given a name supply, yield a stream of names (EXPORTED)
--
names                :: NameSupply -> [Name]
--
--  The recursion of `theNames' where `s' is passed as an argument is crucial, 
--  as it forces the creation of a new closure for `unsafeReadAndIncIntRef s'
--  in each recursion step.  Sharing a single closure or building a cyclic
--  graph for a nullary `theNames' would always result in the same name!  If
--  the compiler ever gets clever enough to optimize this, we have to prevent
--  it from doing so.
--
names (NameSupply s)  = 
  theNames s
  where
    theNames s = Name (unsafeReadAndIncIntRef s) : theNames s


-- UNSAFE mutable variables
-- ------------------------

-- WARNING: The following does not exist, or at least, it belongs to another
--	    world.  And if you believe into the lambda calculus, you don't
--	    want to know about this other world.
--
--		   *** DON'T TOUCH NOR USE THIS STUFF *** 
--              (unless you really know what you are doing!)

-- UNSAFELY create a mutable integer (EXPORTED)
--
unsafeNewIntRef   :: Int -> IORef Int
unsafeNewIntRef i  = unsafePerformIO (newIORef i)

-- UNSAFELY increment a mutable integer and yield its value before the
-- increment (EXPORTED)
--
unsafeReadAndIncIntRef    :: IORef Int -> Int
unsafeReadAndIncIntRef mv  = unsafePerformIO $ do
			       v <- readIORef mv
			       writeIORef mv (v + 1)
			       return v
