{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

{-
The file is part of the Haskell Object Observation Debugger,
(HOOD) March 2010 release.

HOOD is a small post-mortem debugger for the lazy functional
language Haskell. It is based on the concept of observation of
intermediate data structures, rather than the more traditional
stepping and variable examination paradigm used by imperative
language debuggers.

Copyright (c) Andy Gill, 1992-2000
Copyright (c) The University of Kansas 2010
Copyright (c) Maarten Faddegon, 2013-2015

All rights reserved. HOOD is distributed as free software under
the license in the file "License", which available from the HOOD
web page, <http://www.haskell.org/hood>

This module produces CDS's, based on the observation made on Haskell
objects, including base types, constructors and functions.

WARNING: unrestricted use of 'unsafePerformIO' below.

This was ported for the version found on <www.haskell.org/hood>.
-}
module Debug.Hood.Observe

{-
************************************************************************
*                                                                      *
                                 Exports
*                                                                      *
************************************************************************
-}
  (
   -- * The main Hood API


    observe        -- (Observable a) => String -> a -> a
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  , Observing      -- a -> a
  , Observable(..) -- Class
  , runO           -- IO a -> IO ()
  , printO         -- a -> IO ()
  , putStrO        -- String -> IO ()

   -- * For advanced users, that want to render their own datatypes.
  , (<<)           -- (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
  , thunk          -- (Observable a) => a -> ObserverM a
  , send
  , observeBase
  , observeOpaque

  -- * For users that want to write there own render drivers.

  , debugO         -- IO a -> IO [CDS]
  , CDS(..)
  ) where

{-
************************************************************************
*                                                                      *
                          Imports and infixing
*                                                                      *
************************************************************************
-}
import System.IO
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Array as Array
import Data.List
--import System

-- The only non standard one we assume
--import IOExts
import Data.IORef
import System.IO.Unsafe
import GHC.Generics
import Text.PrettyPrint.FPretty hiding ((<$>))

import Control.Concurrent

import Control.Exception ( Exception, throw )
import qualified Control.Exception as Exception
{-
 ( catch
                , Exception(..)
                , throw
                ) as Exception
-}
import Data.Dynamic ( Dynamic )

import Prelude

infixl 9 <<

{-
************************************************************************
*                                                                      *
                        External start functions
*                                                                      *
************************************************************************

Run the observe ridden code.
-}

-- | run some code and return the CDS structure (for when you want to write your own debugger).
debugO :: IO a -> IO [CDS]
debugO program =
     do { initUniq
        ; startEventStream
        ; let errorMsg e = "[Escaping Exception in Code : " ++ show e ++ "]"
        ; ourCatchAllIO (do { program ; return () })
                        (hPutStrLn stderr . errorMsg)
        ; events <- endEventStream
        ; return (eventsToCDS events)
        }

-- | print a value, with debugging
printO :: (Show a) => a -> IO ()
printO expr = runO (print expr)

-- | print a string, with debugging
putStrO :: String -> IO ()
putStrO expr = runO (putStr expr)

-- | The main entry point; run some IO code, and debug inside it.
--
-- An example of using this debugger is
--
-- @runO (print [ observe "+1" (+1) x | x <- observe "xs" [1..3]])@
--
-- @[2,3,4]
-- -- +1
--  { \ 1  -> 2
--  }
-- -- +1
--  { \ 2  -> 3
--  }
-- -- +1
--  { \ 3  -> 4
--  }
-- -- xs
--  1 : 2 : 3 : []@
--
-- Which says, the return is @[2,3,4]@, there were @3@ calls to +1
-- (showing arguments and results), and @xs@, which was the list
-- @1 : 2 : 3 : []@.
--

runO :: IO a -> IO ()
runO program =
    do { cdss <- debugO program
       ; let cdss1 = rmEntrySet cdss
       ; let cdss2 = simplifyCDSSet cdss1
       ; let output1 = cdssToOutput cdss2
       ; let output2 = commonOutput output1
       ; let ptyout  = pretty 80 (foldr (<>) nil (map renderTop output2))
       ; hPutStrLn stderr ""
       ; hPutStrLn stderr ptyout
       }

{-
************************************************************************
*                                                                      *
                              Simulations
*                                                                      *
************************************************************************

Here we provide stubs for the functionally that is not supported
by some compilers, and provide some combinators of various flavors.
-}


ourCatchAllIO :: IO a -> (Exception.SomeException -> IO a) -> IO a
ourCatchAllIO = Exception.catch

handleExc :: Parent -> Exception.SomeException -> IO a
handleExc context exc = return (send "throw" (return throw << exc) context)

{-
************************************************************************
*                                                                      *
                               Instances
*                                                                      *
************************************************************************

The Haskell Base types
-}

instance Observable Int         where { observer = observeBase }
instance Observable Bool        where { observer = observeBase }
instance Observable Integer     where { observer = observeBase }
instance Observable Float       where { observer = observeBase }
instance Observable Double      where { observer = observeBase }
instance Observable Char        where { observer = observeBase }

instance Observable ()          where { observer = observeOpaque "()" }

-- utilities for base types.
-- The strictness (by using seq) is the same
-- as the pattern matching done on other constructors.
-- we evalute to WHNF, and not further.

observeBase :: (Show a) => a -> Parent -> a
observeBase lit cxt = seq lit $ send (show lit) (return lit) cxt

observeOpaque :: String -> a -> Parent -> a
observeOpaque str val cxt = seq val $ send str (return val) cxt

-- The Constructors.

instance (Observable a,Observable b) => Observable (a,b) where
  observer (a,b) = send "," (return (,) << a << b)

instance (Observable a,Observable b,Observable c) => Observable (a,b,c) where
  observer (a,b,c) = send "," (return (,,) << a << b << c)

instance (Observable a,Observable b,Observable c,Observable d)
         => Observable (a,b,c,d) where
  observer (a,b,c,d) = send "," (return (,,,) << a << b << c << d)

instance (Observable a,Observable b,Observable c,Observable d,Observable e)
         => Observable (a,b,c,d,e) where
  observer (a,b,c,d,e) = send "," (return (,,,,) << a << b << c << d << e)

instance (Observable a) => Observable [a] where
  observer (a:as) = send ":"  (return (:) << a << as)
  observer []     = send "[]" (return [])

instance (Observable a) => Observable (Maybe a) where
  observer (Just a) = send "Just"    (return Just << a)
  observer Nothing  = send "Nothing" (return Nothing)

instance (Observable a,Observable b) => Observable (Either a b) where
  observer (Left a)  = send "Left"  (return Left  << a)
  observer (Right a) = send "Right" (return Right << a)

-- Arrays.

instance (Ix a,Observable a,Observable b) => Observable (Array.Array a b) where
  observer arr = send "array" (return Array.array << Array.bounds arr
                                                  << Array.assocs arr
                              )

-- IO monad.

instance (Observable a) => Observable (IO a) where
  observer fn cxt =
        do res <- fn
           send "<IO>" (return return << res) cxt


-- Functions.

instance (Observable a,Observable b) => Observable (a -> b) where
  observer fn cxt arg = sendObserveFnPacket (
        do arg <- thunk arg
           thunk (fn arg)) cxt

  observers = defaultFnObservers

-- The Exception *datatype* (not exceptions themselves!).
-- For now, we only display IOExceptions and calls to Error.

instance Observable Exception.SomeException where
--  observer (IOException a)      = observeOpaque "IOException" (IOException a)
--  observer (ErrorCall a)        = send "ErrorCall"   (return ErrorCall << a)
  observer other                = send "<Exception>" (return other)

instance Observable Dynamic where { observer = observeOpaque "<Dynamic>" }

{-
************************************************************************
*                                                                      *
                      Classes and Data Definition
*                                                                      *
************************************************************************
-}

-- | The 'Observable' class defines how data types are observed.
-- For 'Generic' data types, this can be derived on GHC 7.2 or later.
-- For example:
--
-- @
--   data MyType = MyConstr Int String deriving 'Generic'
--   instance 'Observable' MyType
-- @
class Observable a where
        {-
         - This reveals the name of a specific constructor.
         - and gets ready to explain the sub-components.
         -
         - We put the context second so we can do eta-reduction
         - with some of our definitions.
         -}
        observer  :: a -> Parent -> a
        default observer :: (Generic a, GObservable (Rep a)) => a -> Parent -> a
        observer x c = to (gdmobserver (from x) c)

        {-
         - This used used to group several observer instances together.
         -}
        observers :: String -> (Observer -> a) -> a
        observers label arg = defaultObservers label arg

class GObservable f where
        gdmobserver :: f a -> Parent -> f a
        gdmObserveChildren :: f a -> ObserverM (f a)
        gdmShallowShow :: f a -> String

-- Meta: data types
instance (GObservable a) => GObservable (M1 D d a) where
        gdmobserver m@(M1 x) cxt = M1 (gdmobserver x cxt)
        gdmObserveChildren = gthunk
        gdmShallowShow = error "gdmShallowShow not defined on <<Meta: data types>>"

-- Meta: Selectors
instance (GObservable a, Selector s) => GObservable (M1 S s a) where
        gdmobserver m@(M1 x) cxt
          = M1 (gdmobserver x cxt)
          -- Uncomment next two lines to record selector names
          -- | selName m == "" = M1 (gdmobserver x cxt)
          -- | otherwise       = M1 (send (selName m ++ " =") (gdmObserveChildren x) cxt)
        gdmObserveChildren  = gthunk
        gdmShallowShow      = error "gdmShallowShow not defined on <<Meta: selectors>>"

-- Meta: Constructors
instance (GObservable a, Constructor c) => GObservable (M1 C c a) where
        gdmobserver m1            = send (gdmShallowShow m1) (gdmObserveChildren m1)
        gdmObserveChildren (M1 x) = do {x' <- gdmObserveChildren x; return (M1 x')}
        gdmShallowShow            = conName

-- Unit: used for constructors without arguments
instance GObservable U1 where
        gdmobserver x _    = x
        gdmObserveChildren = return
        gdmShallowShow     = error "gdmShallowShow not defined on <<the unit type>>"

-- Sums: encode choice between constructors
instance (GObservable a, GObservable b) => GObservable (a :+: b) where
        gdmobserver (L1 x) = send (gdmShallowShow x) (gdmObserveChildren $ L1 x)
        gdmobserver (R1 x) = send (gdmShallowShow x) (gdmObserveChildren $ R1 x)
        gdmShallowShow (L1 x) = gdmShallowShow x
        gdmShallowShow (R1 x) = gdmShallowShow x
        gdmObserveChildren (L1 x) = do {x' <- gdmObserveChildren x; return (L1 x')}
        gdmObserveChildren (R1 x) = do {x' <- gdmObserveChildren x; return (R1 x')}

-- Products: encode multiple arguments to constructors
instance (GObservable a, GObservable b) => GObservable (a :*: b) where
        gdmobserver (a :*: b) cxt = (gdmobserver a cxt) :*: (gdmobserver b cxt)
        gdmObserveChildren (a :*: b) = do a'  <- gdmObserveChildren a
                                          b'  <- gdmObserveChildren b
                                          return (a' :*: b')
        gdmShallowShow = error "gdmShallowShow not defined on <<the product type>>"

-- Constants: additional parameters and recursion of kind *
instance (Observable a) => GObservable (K1 i a) where
        gdmobserver (K1 x) cxt = K1 $ observer x cxt
        gdmObserveChildren = gthunk
        gdmShallowShow = error "gdmShallowShow not defined on <<constant types>>"

gthunk :: (GObservable f) => f a -> ObserverM (f a)
gthunk a = ObserverM $ \ parent port ->
                ( gdmobserver_ a (Parent
                                { observeParent = parent
                                , observePort   = port
                                })
                , port+1 )

gdmobserver_ :: (GObservable f) => f a -> Parent -> f a
gdmobserver_ a context = gsendEnterPacket a context

gsendEnterPacket :: (GObservable f) => f a -> Parent -> f a
gsendEnterPacket r context = unsafeWithUniq $ \ node ->
     do { sendEvent node context Enter
        ; ourCatchAllIO (evaluate (gdmobserver r context))
                        (handleExc context)
        }

type Observing a = a -> a

newtype Observer = O (forall a . (Observable a) => String -> a -> a)

defaultObservers :: (Observable a) => String -> (Observer -> a) -> a
defaultObservers label fn = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
        ; let observe' sublabel a
               = unsafeWithUniq $ \ subnode ->
                 do { sendEvent subnode (Parent node 0)
                                        (Observe sublabel)
                    ; return (observer_ a (Parent
                        { observeParent = subnode
                        , observePort   = 0
                        }))
                    }
        ; return (observer_ (fn (O observe'))
                       (Parent
                        { observeParent = node
                        , observePort   = 0
                        }))
        }
defaultFnObservers :: (Observable a, Observable b)
                      => String -> (Observer -> a -> b) -> a -> b
defaultFnObservers label fn arg = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
        ; let observe' sublabel a
               = unsafeWithUniq $ \ subnode ->
                 do { sendEvent subnode (Parent node 0)
                                        (Observe sublabel)
                    ; return (observer_ a (Parent
                        { observeParent = subnode
                        , observePort   = 0
                        }))
                    }
        ; return (observer_ (fn (O observe'))
                       (Parent
                        { observeParent = node
                        , observePort   = 0
                        }) arg)
        }

{-
************************************************************************
*                                                                      *
                          The ObserveM Monad
*                                                                      *
************************************************************************

The Observer monad, a simple state monad,
for placing numbers on sub-observations.
-}

newtype ObserverM a = ObserverM { runMO :: Int -> Int -> (a,Int) }

instance Functor ObserverM where
    fmap = liftM

instance Applicative ObserverM where
    pure = return
    (<*>) = ap

instance Monad ObserverM where
        return a = ObserverM (\ c i -> (a,i))
        fn >>= k = ObserverM (\ c i ->
                case runMO fn c i of
                  (r,i2) -> runMO (k r) c i2
                )

thunk :: (Observable a) => a -> ObserverM a
thunk a = ObserverM $ \ parent port ->
                ( observer_ a (Parent
                                { observeParent = parent
                                , observePort   = port
                                })
                , port+1 )

(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }

{-
************************************************************************
*                                                                      *
                          observe and friends
*                                                                      *
************************************************************************

Our principal function and class
-}

-- | 'observe' observes data structures in flight.
--
-- An example of use is
--  @
--    map (+1) . observe \"intermeduate\" . map (+2)
--  @
--
-- In this example, we observe the value that flows from the producer
-- @map (+2)@ to the consumer @map (+1)@.
--
-- 'observe' can also observe functions as well a structural values.
--
{-# NOINLINE observe #-}
observe :: (Observable a) => String -> a -> a
observe name a = generateContext name a

{- This gets called before observer, allowing us to mark
 - we are entering a, before we do case analysis on
 - our object.
 -}

{-# NOINLINE observer_ #-}
observer_ :: (Observable a) => a -> Parent -> a
observer_ a context = sendEnterPacket a context

data Parent = Parent
        { observeParent :: !Int -- my parent
        , observePort   :: !Int -- my branch number
        } deriving Show
root = Parent 0 0


-- The functions that output the data. All are dirty.

unsafeWithUniq :: (Int -> IO a) -> a
unsafeWithUniq fn
  = unsafePerformIO $ do { node <- getUniq
                         ; fn node
                         }

generateContext :: (Observable a) => String -> a -> a
generateContext label orig = unsafeWithUniq $ \ node ->
     do { sendEvent node (Parent 0 0) (Observe label)
        ; return (observer_ orig (Parent
                        { observeParent = node
                        , observePort   = 0
                        })
                  )
        }

send :: String -> ObserverM a -> Parent -> a
send consLabel fn context = unsafeWithUniq $ \ node ->
     do { let (r,portCount) = runMO fn node 0
        ; sendEvent node context (Cons portCount consLabel)
        ; return r
        }

sendEnterPacket :: (Observable a) => a -> Parent -> a
sendEnterPacket r context = unsafeWithUniq $ \ node ->
     do { sendEvent node context Enter
        ; ourCatchAllIO (evaluate (observer r context))
                        (handleExc context)
        }

evaluate :: a -> IO a
evaluate a = a `seq` return a


sendObserveFnPacket :: ObserverM a -> Parent -> a
sendObserveFnPacket fn context = unsafeWithUniq $ \ node ->
     do { let (r,_) = runMO fn node 0
        ; sendEvent node context Fun
        ; return r
        }

{-
************************************************************************
*                                                                      *
                             Event stream
*                                                                      *
************************************************************************

Trival output functions
-}

data Event = Event
                { portId     :: !Int
                , parent     :: !Parent
                , change     :: !Change
                }
        deriving Show

data Change
        = Observe       !String
        | Cons    !Int  !String
        | Enter
        | Fun
        deriving Show

startEventStream :: IO ()
startEventStream = writeIORef events []

endEventStream :: IO [Event]
endEventStream =
        do { es <- readIORef events
           ; writeIORef events badEvents
           ; return es
           }

sendEvent :: Int -> Parent -> Change -> IO ()
sendEvent nodeId parent change =
        do { nodeId `seq` parent `seq` return ()
           ; change `seq` return ()
           ; takeMVar sendSem
           ; es <- readIORef events
           ; let event = Event nodeId parent change
           ; writeIORef events (event `seq` (event : es))
           ; putMVar sendSem ()
           }

-- local
events :: IORef [Event]
events = unsafePerformIO $ newIORef badEvents

badEvents :: [Event]
badEvents = error "Bad Event Stream"

-- use as a trivial semiphore
{-# NOINLINE sendSem #-}
sendSem :: MVar ()
sendSem = unsafePerformIO $ newMVar ()
-- end local

{-
************************************************************************
*                                                                      *
                         Unique name supply code
*                                                                      *
************************************************************************

Use the single threaded version
-}

initUniq :: IO ()
initUniq = writeIORef uniq 1

getUniq :: IO Int
getUniq
    = do { takeMVar uniqSem
         ; n <- readIORef uniq
         ; writeIORef uniq $! (n + 1)
         ; putMVar uniqSem ()
         ; return n
         }

peepUniq :: IO Int
peepUniq = readIORef uniq

-- locals
{-# NOINLINE uniq #-}
uniq :: IORef Int
uniq = unsafePerformIO $ newIORef 1

{-# NOINLINE uniqSem #-}
uniqSem :: MVar ()
uniqSem = unsafePerformIO $ newMVar ()

{-
************************************************************************
*                                                                      *
                         Global, initualizers, etc
*                                                                      *
************************************************************************
-}

openObserveGlobal :: IO ()
openObserveGlobal =
     do { initUniq
        ; startEventStream
        }

closeObserveGlobal :: IO [Event]
closeObserveGlobal =
     do { evs <- endEventStream
        ; putStrLn ""
        ; return evs
        }

{-
************************************************************************
*                                                                      *
                     The CDS and converting functions
*                                                                      *
************************************************************************
-}

data CDS = CDSNamed String         CDSSet
         | CDSCons Int String     [CDSSet]
         | CDSFun  Int             CDSSet CDSSet
         | CDSEntered Int
        deriving (Show,Eq,Ord)

type CDSSet = [CDS]


eventsToCDS :: [Event] -> CDSSet
eventsToCDS pairs = getChild 0 0
   where
     res i = (!) out_arr i

     bnds = (0, length pairs)

     mid_arr :: Array Int [(Int,CDS)]
     mid_arr = accumArray (flip (:)) [] bnds
                [ (pnode,(pport,res node))
                | (Event node (Parent pnode pport) _) <- pairs
                ]

     out_arr = array bnds       -- never uses 0 index
                [ (node,getNode'' node change)
                | (Event node _ change) <- pairs
                ]

     getNode'' ::  Int -> Change -> CDS
     getNode'' node change =
       case change of
        (Observe str) -> CDSNamed str (getChild node 0)
        (Enter)       -> CDSEntered node
        (Fun)         -> CDSFun node (getChild node 0) (getChild node 1)
        (Cons portc cons)
                      -> CDSCons node cons
                                [ getChild node n | n <- [0..(portc-1)]]

     getChild :: Int -> Int -> CDSSet
     getChild pnode pport =
        [ content
        | (pport',content) <- (!) mid_arr pnode
        , pport == pport'
        ]

render  :: Int -> Bool -> CDS -> Doc
render prec par (CDSCons _ ":" [cds1,cds2]) =
        if (par && not needParen)
        then doc -- dont use paren (..) because we dont want a grp here!
        else paren needParen doc
   where
        doc = grp (softline <> renderSet' 5 False cds1 <> text " : ") <>
              renderSet' 4 True cds2
        needParen = prec > 4
render prec par (CDSCons _ "," cdss) | length cdss > 0 =
        nest 2 (text "(" <> foldl1 (\ a b -> a <> text ", " <> b)
                            (map renderSet cdss) <>
                text ")")
render prec par (CDSCons _ name cdss) =
        paren (length cdss > 0 && prec /= 0)
              (nest 2
                 (text name <> foldr (<>) nil
                                [ softline <> renderSet' 10 False cds
                                | cds <- cdss
                                ]
                 )
              )

{- renderSet handles the various styles of CDSSet.
 -}

renderSet :: CDSSet -> Doc
renderSet = renderSet' 0 False

renderSet' :: Int -> Bool -> CDSSet -> Doc
renderSet' _ _      [] = text "_"
renderSet' prec par [cons@(CDSCons {})]    = render prec par cons
renderSet' prec par cdss                   =
        nest 0 (text "{ " <> foldl1 (\ a b -> a <> line <>
                                    text ", " <> b)
                                    (map renderFn pairs) <>
                line <> text "}")

   where
        pairs = nub (sort (findFn cdss))
        -- local nub for sorted lists
        nub []                  = []
        nub (a:a':as) | a == a' = nub (a' : as)
        nub (a:as)              = a : nub as

renderFn :: ([CDSSet],CDSSet) -> Doc
renderFn (args,res)
        = grp  (nest 3
                (text "\\ " <>
                 foldr (\ a b -> nest 0 (renderSet' 10 False a) <> sp <> b)
                       nil
                       args <> softline <>
                 text "-> " <> renderSet' 0 False res
                )
               )

findFn :: CDSSet -> [([CDSSet],CDSSet)]
findFn = foldr findFn' []

findFn' (CDSFun _ arg res) rest =
    case findFn res of
       [(args',res')] -> (arg : args', res') : rest
       _              -> ([arg], res) : rest
findFn' other rest = ([],[other]) : rest

renderTops []   = nil
renderTops tops = line <> foldr (<>) nil (map renderTop tops)

renderTop :: Output -> Doc
renderTop (OutLabel str set extras) =
        nest 2 (text ("-- " ++ str) <> line <>
                renderSet set
                <> renderTops extras) <> line

rmEntry :: CDS -> CDS
rmEntry (CDSNamed str set)   = CDSNamed str (rmEntrySet set)
rmEntry (CDSCons i str sets) = CDSCons i str (map rmEntrySet sets)
rmEntry (CDSFun i a b)       = CDSFun i (rmEntrySet a) (rmEntrySet b)
rmEntry (CDSEntered i)       = error "found bad CDSEntered"

rmEntrySet = map rmEntry . filter noEntered
  where
        noEntered (CDSEntered _) = False
        noEntered _              = True

simplifyCDS :: CDS -> CDS
simplifyCDS (CDSNamed str set) = CDSNamed str (simplifyCDSSet set)
simplifyCDS (CDSCons _ "throw"
                  [[CDSCons _ "ErrorCall" set]]
            ) = simplifyCDS (CDSCons 0 "error" set)
simplifyCDS cons@(CDSCons i str sets) =
        case spotString [cons] of
          Just str | not (null str) -> CDSCons 0 (show str) []
          _ -> CDSCons 0 str (map simplifyCDSSet sets)

simplifyCDS (CDSFun i a b) = CDSFun 0 (simplifyCDSSet a) (simplifyCDSSet b)
        -- replace with
        --      CDSCons i "->" [simplifyCDSSet a,simplifyCDSSet b]
        -- for turning off the function stuff.

simplifyCDSSet = map simplifyCDS

spotString :: CDSSet -> Maybe String
spotString [CDSCons _ ":"
                [[CDSCons _ str []]
                ,rest
                ]
           ]
        = do { ch <- case reads str of
                       [(ch,"")] -> return ch
                       _ -> Nothing
             ; more <- spotString rest
             ; return (ch : more)
             }
spotString [CDSCons _ "[]" []] = return []
spotString other = Nothing

paren :: Bool -> Doc -> Doc
paren False doc = grp (nest 0 doc)
-- paren True  doc = grp (nest 0 (text "(" <> nest 0 doc <> softline <> text ")"))
paren True  doc = grp (text "(" <> doc <> softline <> text ")")

sp :: Doc
sp = text " "

data Output = OutLabel String CDSSet [Output]
            | OutData  CDS
              deriving (Eq,Ord)


commonOutput :: [Output] -> [Output]
commonOutput = sortBy byLabel
  where
     byLabel (OutLabel lab _ _) (OutLabel lab' _ _) = compare lab lab'

cdssToOutput :: CDSSet -> [Output]
cdssToOutput =  map cdsToOutput

cdsToOutput (CDSNamed name cdsset)
            = OutLabel name res1 res2
  where
      res1 = [ cdss | (OutData cdss) <- res ]
      res2 = [ out  | out@(OutLabel {}) <- res ]
      res  = cdssToOutput cdsset
cdsToOutput cons@(CDSCons {}) = OutData cons
cdsToOutput    fn@(CDSFun {}) = OutData fn

nil = Text.PrettyPrint.FPretty.empty
grp = Text.PrettyPrint.FPretty.group
