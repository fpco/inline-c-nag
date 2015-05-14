# Using `inline-c` with NAG

This package a `C.Context` and various utilities which make it easy to
use the NAG library from Haskell.  We present two examples which not
only demonstrate that but also show a nice mix of the features available
in `inline-c`.

## One dimensional FFT

In this first example we will compute the forward discrete Fourier
transform of a sequence of complex numbers, using the
[`nag_sum_fft_complex_1d`](http://www.nag.com/numeric/CL/nagdoc_cl24/html/C06/c06pcc.html)
function in the NAG library.

While the example is short it showcases various features, including the
already seen ordinary and vector anti-quoting; but also some NAG
specific goodies such as handling of custom types (complex numbers) and
error handling using the `withNagError` function, defined in the
`Language.C.Inline.Nag` module provided by `inline-c-nag`.

```
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Language.C.Inline.Nag as C
import qualified Data.Vector.Storable as V
import           Foreign.C.Types

-- Set the 'Context' to the one provided by "Language.C.Inline.Nag".
-- This gives us access to NAG types such as 'Complex' and 'NagError',
-- and also includes the vector and function pointers anti-quoters.
C.context C.nagCtx

-- Include the headers files we need.
C.include "<nag.h>"
C.include "<nagc06.h>"

-- | Computes the discrete Fourier transform for the given sequence of
-- 'Complex' numbers.  Returns 'Left' if some error occurred, together
-- with the error message.
forwardFFT :: V.Vector C.Complex -> IO (Either String (V.Vector C.Complex))
forwardFFT x_orig = do
  -- "Thaw" the input vector -- the input is an immutable vector, and by
  -- "thawing" it we create a mutable copy of it.
  x <- V.thaw x_orig
  -- Use 'withNagError' to easily check whether the NAG operation was
  -- successful.
  C.withNagError $ \fail_ -> do
    [C.exp| void {
       nag_sum_fft_complex_1d(
         // We're computing a forward transform
         Nag_ForwardTransform,
         // We take the pointer underlying 'x' and it's length, using the
         // appropriate anti-quoters
         $vec-ptr:(Complex *x), $vec-len:x,
         // And pass in the NagError structure given to us by
         // 'withNagError'.
         $(NagError *fail_))
      } |]
    -- Turn the mutable vector back to an immutable one using 'V.freeze'
    -- (the inverse of 'V.thaw').
    V.freeze x

-- Run our function with some sample data and print the results.
main :: IO ()
main = do
  let vec = V.fromList
        [ Complex 0.34907 (-0.37168)
        , Complex 0.54890 (-0.35669)
        , Complex 0.74776 (-0.31175)
        , Complex 0.94459 (-0.23702)
        , Complex 1.13850 (-0.13274)
        , Complex 1.32850   0.00074
        , Complex 1.51370   0.16298
        ]
  printVec vec
  Right vec_f <- forwardFFT vec
  printVec vec_f
  where
    printVec vec = do
      V.forM_ vec $ \(Complex re im) -> putStr $ show (re, im) ++ " "
      putStrLn ""
```

Note how we're able to use the `nag_sum_fft_complex_1d` function just
for the feature we need, using the `Nag_ForwardTransform` enum directly
in the C code, instead of having to define some Haskell interface for
it.  Using facilities provided by `inline-c-nag` we're also able to have
nice error handling, automatically extracting the error returned by NAG
if something goes wrong.

### Nelder-Mead optimization

For a more complex example, we'll write an Haskell function that
performs Nelder-Mead optimization using the
[`nag_opt_simplex_easy`](http://www.nag.com/numeric/CL/nagdoc_cl24/html/E04/e04cbc.html)
function provided by NAG.

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Storable (poke)
import qualified Language.C.Inline.Nag as C
import           Foreign.C.Types

C.context C.nagCtx

C.include "<math.h>"
C.include "<nag.h>"
C.include "<nage04.h>"
C.include "<nagx02.h>"

nelderMead
  :: V.Vector CDouble
  -- ^ Starting point
  -> (V.Vector CDouble -> CDouble)
  -- ^ Function to minimize
  -> C.Nag_Integer
  -- ^ Maximum number of iterations (must be >= 1).
  -> IO (Either String (CDouble, V.Vector CDouble))
  -- ^ Position of the minimum.  'Left' if something went wrong, with
  -- error message. 'Right', together with the minimum cost and its
  -- position, if it could be found.
nelderMead xImm pureFunct maxcal = do
    -- Create function that the C code will use.
    let funct n xc fc _comm = do
          xc' <- newForeignPtr_ xc
          let f = pureFunct $ V.unsafeFromForeignPtr0 xc' $ fromIntegral n
          poke fc f
    -- Create mutable input/output vector for C code
    x <- V.thaw xImm
    -- Call the C code
    C.withNagError $ \fail_ -> do
      minCost <- [C.block| double {
          // The function takes an exit parameter to store the minimum
          // cost.
          double f;
          // We hardcode sensible values (see NAG documentation) for the
          // error tolerance, computed using NAG's nag_machine_precision.
          double tolf = sqrt(nag_machine_precision);
          double tolx = sqrt(tolf);
          // Call the function
          nag_opt_simplex_easy(
            // Get vector length and pointer.
            $vec-len:x, $vec-ptr:(double *x),
            &f, tolf, tolx,
            // Pass function pointer to our Haskell function using the fun
            // anti-quotation.
            $fun:(void (*funct)(Integer n, const double *xc, double *fc, Nag_Comm *comm)),
            // We do not provide a "monitoring" function.
            NULL,
            // Capture Haskell variable with the max number of iterations.
            $(Integer maxcal),
            // Do not provide the Nag_Comm parameter, which we don't need.
            NULL,
            // Pass the NagError parameter provided by withNagError
            $(NagError *fail_));
          return f;
        } |]
      -- Get a new immutable vector by freezing the mutable one.
      minCostPos <- V.freeze x
      return (minCost, minCostPos)

-- Optimize a two-dimensional function.  Example taken from
-- <http://www.nag.com/numeric/CL/nagdoc_cl24/examples/source/e04cbce.c>.
main :: IO ()
main = do
  let funct = \x ->
        let x0 = x V.! 0
            x1 = x V.! 1
        in exp x0 * (4*x0*(x0+x1)+2*x1*(x1+1.0)+1.0)
      start = V.fromList [-1, 1]
  Right (minCost, minPos) <- nelderMead start funct 500
  putStrLn $ "Minimum cost: " ++ show minCost
  putStrLn $ "End positition: " ++ show (minPos V.! 0) ++ ", " ++ show (minPos V.! 1)
```

Again, in this example we use a function with a very complex and
powerful signature, such as the one for the Nelder-Mead optimization in
NAG, in a very specific way -- avoiding the high cost of having to
specify a well-designed Haskell interface for it.
