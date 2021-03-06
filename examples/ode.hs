{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Data.Functor ((<$>))
import           Data.IORef
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr)
import           Foreign.Ptr (nullFunPtr)
import           Foreign.Storable (Storable, peek, poke)
import qualified Language.C.Inline.Nag as C
import           System.IO.Unsafe (unsafePerformIO)

C.context C.nagCtx

C.include "<stdio.h>"
C.include "<nag.h>"
C.include "<nagd02.h>"

-- Main types
------------------------------------------------------------------------

type Fn
  =  CDouble
     -- ^ The indipendent variable @x@
  -> V.Vector CDouble
     -- ^ @y_i@ for @i = 1, 2, ..., neq@
  -> V.Vector CDouble
     -- ^ @f_i@ for @i = 1, 2, ..., neq@

type Jacobian
  =  CDouble
     -- ^ The indipendent variable @x@
  -> V.Vector CDouble
     -- ^ @y_i@ for @i = 1, 2, ..., neq@
  -> V.Vector CDouble
     -- ^ Jacobian matrix, @pw[(i - 1) * neq + j - 1@ must contain the
     -- value of @∂f_i/∂y_j@, for @i, j = 1, 2, ..., neq@

type Interval = (CDouble, CDouble)

data Failure = Failure
  {  failureMessage :: String
  , _failureAt :: CDouble
  } deriving (Eq, Show)

data ErrorControl
  = Relative
  | Absolute
  | Mixed
  deriving (Eq, Show)

data Options = Options
  { optionsTolerance :: !CDouble
  , optionsErrorControl :: !ErrorControl
  } deriving (Eq, Show)

-- IO solving
------------------------------------------------------------------------

data OutputIO a = OutputIO
  { outputIOStartState :: a
  , outputIOStep :: a -> CDouble -> V.Vector CDouble -> IO (a, CDouble)
  }

solveIO
  :: Options
  -> Fn
  -> Maybe Jacobian
  -> Interval
  -> Maybe (OutputIO a)
  -> V.Vector CDouble
  -> IO (Either Failure (V.Vector CDouble, Maybe a))
solveIO Options{..} fcn mbJac (x, xend) mbOutput y = do
  -- IO version of the right-hande function
  let fcnIO neq x y f _comm = do
        fImm <- fcn x <$> vectorFromC neq y
        vectorToC fImm neq f
  -- Function pointer for the Jacobian function.  We use a function
  -- pointer directly because we want it to be NULL if the user hasn't
  -- provided a function.
  jacFunPtr <- case mbJac of
    Nothing -> return nullFunPtr
    Just jac -> do
      let jacIO neq x y pw  _comm = do
            pwImm <- jac x <$> vectorFromC neq y
            vectorToC pwImm (neq*neq) pw
      $(C.mkFunPtr [t| C.Nag_Integer -> CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr C.Nag_User -> IO () |]) jacIO
  (outputFunPtr, outputGetResult) <- case mbOutput of
    Nothing -> return (nullFunPtr, return Nothing)
    Just OutputIO{..} -> do
      outputStateRef <- newIORef outputIOStartState
      let outputIO neq xsol y _comm = do
            x <- peek xsol
            y' <- vectorFromC neq y
            outputState <- readIORef outputStateRef
            (outputState', x') <- outputIOStep outputState x y'
            writeIORef outputStateRef outputState'
            poke xsol x'
      outputFunPtr <- $(C.mkFunPtr [t| C.Nag_Integer -> Ptr CDouble -> Ptr CDouble -> Ptr C.Nag_User -> IO ()|]) outputIO
      return (outputFunPtr, Just <$> readIORef outputStateRef)
  -- Error control
  err <- case optionsErrorControl of
    Relative -> [C.exp| Nag_ErrorControl{ Nag_Relative } |]
    Absolute -> [C.exp| Nag_ErrorControl{ Nag_Absolute } |]
    Mixed -> [C.exp| Nag_ErrorControl{ Nag_Mixed } |]
  -- Record the last visited x in an 'IORef' to store it in the
  -- 'Failure' if there was a problem.
  xendRef <- newIORef x
  yMut <- V.thaw y
  res <- C.withNagError $ \fail_ -> do
    xend' <- [C.block| double {
        double x = $(double x);
        Nag_User comm;
        nag_ode_ivp_bdf_gen(
          $vec-len:yMut,
          $fun:(void (*fcnIO)(Integer neq, double x, const double y[], double f[], Nag_User *comm)),
          $(void (*jacFunPtr)(Integer neq, double x, const double y[], double pw[], Nag_User *comm)),
          &x, $vec-ptr:(double yMut[]), $(double xend),
          $(double optionsTolerance), $(Nag_ErrorControl err),
          $(void (*outputFunPtr)(Integer neq, double *xsol, const double y[], Nag_User *comm)),
          NULLDFN, &comm, $(NagError *fail_));
        return x;
      } |]
    writeIORef xendRef xend'
  case res of
    Left s -> do
      xend' <- readIORef xendRef
      return $ Left $ Failure s xend'
    Right () -> do
      y' <- V.freeze yMut
      mbOutput <- outputGetResult
      return $ Right (y', mbOutput)

-- Pure solver
------------------------------------------------------------------------

data Output a = Output
  { outputStartState :: a
  , outputStep :: a -> CDouble -> V.Vector CDouble -> (a, CDouble)
  }

{-
outputInterval :: CDouble -> Output [(CDouble, V.Vector CDouble)]
outputInterval interval = Output
  { outputStartState = []
  , outputStep = \xs x y -> (xs ++ [(x, y)], x + interval)
  }

outputFixed :: [CDouble] -> Output ([CDouble], [(CDouble, V.Vector CDouble)])
outputFixed xs = Output
  { outputStartState = (xs, [])
  , outputStep = \(steps, ys) x y -> case steps of
      [] -> ((steps, ys), x+1)
      step:steps -> ((steps, ys ++ [(x, y)]), step)
  }

outputNothing :: CDouble -> Output ()
outputNothing x = Output
  { outputStartState = ()
  , outputStep = \() _ _ -> ((), x)
  }
-}

{-# NOINLINE solve #-}
solve
  :: Options
  -> Fn
  -> Maybe Jacobian
  -> Interval
  -> Maybe (Output a)
  -> V.Vector CDouble
  -> Either Failure (V.Vector CDouble, Maybe a)
solve opts fcn mbJac int mbOutput y = unsafePerformIO $
  solveIO opts fcn mbJac int mbOutputIO y
  where
    mbOutputIO = case mbOutput of
      Nothing -> Nothing
      Just Output{..} -> Just $ OutputIO outputStartState $ \s x y -> return $ outputStep s x y

-- Oregonator
------------------------------------------------------------------------

oregonator :: IO (V.Vector CDouble)
oregonator = do
  let x = 0
  let xend = 360
  let tol = 1e-5
  let res = solve (Options tol Relative) f (Just jac) (x, xend) Nothing y0
  case res of
    Left err -> error $ "Oregonator failed " ++ failureMessage err
    Right (v, _) -> return v
  where
    f :: Fn
    f _ y =
      let y1 = y V.! 0 ; y2 = y V.! 1 ; y3 = y V.! 2
      in V.fromList
           [ s * (y2 - y1 * y2 + y1 - q * (y1 * y1))
           , (-y2 - y1 * y2 + y3) / s
           , w * (y1 - y3)
           ]

    jac :: Jacobian
    jac _ y =
      let y1 = y V.! 0 ; y2 = y V.! 1 ; _y3 = y V.! 2
      in V.fromList
         [  s * (1 - y2 - 2 * q * y1),  s * (1 - y1),   0
         ,  -y2 / s,                    (-1 - y1) / s,  1 / s
         ,  w,                          0,              -2
         ]

    y0 = V.fromList [1, 2, 3]

    s = 77.27 ; q = 8.375E-06 ; w = 0.161

-- Hires
------------------------------------------------------------------------

hires :: IO (V.Vector CDouble)
hires = do
  let x = 0
  let xend = 321.8122
  let res = solve (Options tol Relative) f (Just jac) (x, xend) Nothing y0
  case res of
    Left err -> error $ "Hires failed " ++ failureMessage err
    Right (v, _) -> return v
  where
    f :: Fn
    f _ y =
      let y1 = y V.! 0 ; y2 = y V.! 1 ; y3 = y V.! 2 ; y4 = y V.! 3
          y5 = y V.! 4 ; y6 = y V.! 5 ; y7 = y V.! 6 ; y8 = y V.! 7
      in V.fromList
        [ -1.71 * y1 + 0.43 * y2 + 8.32 * y3 + 0.0007
        , 1.71 * y1 - 8.75 * y2
        , -10.03 * y3 + 0.43 * y4 + 0.035 * y5
        , 8.32 * y2 + 1.71 * y3 - 1.12 * y4
        , -1.745 * y5 + 0.43 * y6 + 0.43 * y7
        , -280 * y6 * y8 + 0.69 * y4 + 1.71 * y5 - 0.43 * y6 + 0.69 * y7
        , 280 * y6 * y8 - 1.81 * y7
        , -280 * y6 * y8 + 1.81 * y7
        ]

    jac :: Jacobian
    jac _ y =
      let _y1 = y V.! 0 ; _y2 = y V.! 1 ; _y3 = y V.! 2 ; _y4 = y V.! 3
          _y5 = y V.! 4 ; y6 = y V.! 5 ; _y7 = y V.! 6 ; y8 = y V.! 7
      in V.fromList
         [  -1.71,  0.43,   8.32,    0,      0,       0,                 0,      0
         ,  1.71,   -8.75,  0,       0,      0,       0,                 0,      0
         ,  0,      0,      -10.03,  0.43,   0.035,   0,                 0,      0
         ,  0,      8.32,   1.71,    -1.12,  0,       0,                 0,      0
         ,  0,      0,      0,       0,      -1.745,  0.43,              0.43,   0
         ,  0,      0,      0,       0.69,   1.71,    -280 * y8 - 0.43,  0.69,   -280 * y6
         ,  0,      0,      0,       0,      0,       280 * y8,          -1.81,  280 * y6
         ,  0,      0,      0,       0,      0,       -280 * y8,         1.81,   -280 * y6
         ]

    y0 = V.fromList [1, 0, 0, 0, 0, 0, 0, 0.0057]

    tol = 1.0e-6

-- NAG
------------------------------------------------------------------------

nagTest :: IO (V.Vector CDouble)
nagTest = do
  let fcn _x y =
        let y1 = y V.! 0
            y2 = y V.! 1
            y3 = y V.! 2
        in V.fromList
          [ y1 * (-0.04) + y2 * 1e4 * y3
          , y1 * 0.04 - y2 * 1e4 * y3 - y2 * 3e7 * y2
          , y2 * 3e7 * y2
          ]
  let jac _x y =
        let _y1 = y V.! 0
            y2 = y V.! 1
            y3 = y V.! 2
        in V.fromList
          [  -0.04,  y3 * 1e4,                y2 * 1e4
          ,  0.04,   y3 * (-1e4) - y2 * 6e7,  y2 * (-1e4)
          ,  0.0,    y2 * 6e7,                0.0
          ]
  let x = 1
  let y = V.fromList [1.0, 0.0, 0.0]
  let tol = 10**(-3)
  let res = solve (Options tol Relative) fcn (Just jac) (x, 10) Nothing y
  case res of
    Left err -> error $ "NAG test failed " ++ show err
    Right (y', _) -> return y'

-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  print =<< nagTest
  print =<< oregonator
  print =<< hires

-- Utils
------------------------------------------------------------------------

vectorFromC :: Storable a => C.Nag_Integer -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' $ fromIntegral len

vectorToC :: Storable a => V.Vector a -> C.Nag_Integer -> Ptr a -> IO ()
vectorToC vec neq ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' $ fromIntegral neq) vec
