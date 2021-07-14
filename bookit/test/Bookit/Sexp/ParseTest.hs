{-# LANGUAGE OverloadedStrings #-}

-- |
module Bookit.Sexp.ParseTest (tests) where

import Bookit.Sexp
  ( Atom (AtomStr, AtomSym),
    Loc (Loc),
    Sexp (SexpAtom, SexpList),
    SrcPos (SrcPos),
    mkLoc,
  )
import qualified Bookit.Sexp.Parse as Parse
import qualified Bookit.Sexp.Ppr as Ppr
import Bookit.Sexp.TypesTest (genAtom, genSexp, genStr, genSym)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Hedgehog
  ( Group (Group),
    Property,
    TestT,
    footnoteShow,
    forAll,
    property,
    success,
    test,
    tripping,
    withTests,
    (===),
  )
import qualified Hedgehog
import System.IO (hFlush, stdout)
import qualified Text.Megaparsec as MP

tests :: Hedgehog.Group
tests =
  Group
    "Bookit.Sexp.Parse"
    [ ("round-tripping Parse.sexp", prop_trip_sexp),
      ("round-tripping Parse.atom", prop_trip_atom),
      ("round-tripping Parse.str", prop_trip_str),
      ("round-tripping Parse.sym", prop_trip_sym),
      -- one-off tests
      ("test: math example 1", test_mathEg1)
    ]

-------------------------------------------------------------------------------
-- Round-tripping tests
-------------------------------------------------------------------------------

prop_trip_sexp :: Property
prop_trip_sexp = property $ do
  sexp <- forAll $ genSexp 5
  tripping sexp Ppr.pprSimple (fmap clearLoc <$> MP.parse Parse.sexp "")

prop_trip_atom :: Property
prop_trip_atom = property $ do
  atom <- forAll genAtom
  tripping atom Ppr.atom (MP.parseMaybe Parse.atom)

prop_trip_str :: Property
prop_trip_str = property $ do
  str <- forAll genStr
  tripping str Ppr.str (MP.parseMaybe Parse.str)

prop_trip_sym :: Property
prop_trip_sym = property $ do
  sym <- forAll genSym
  tripping sym Ppr.sym (MP.parseMaybe Parse.sym)

-- | Clear location information from an 'Sexp'.
clearLoc :: Sexp Loc -> Sexp ()
clearLoc (SexpAtom _ atom) = SexpAtom () atom
clearLoc (SexpList _ xs) = SexpList () (clearLoc <$> xs)

-------------------------------------------------------------------------------
-- One-off parsing tests
-------------------------------------------------------------------------------

test_mathEg1 :: Property
test_mathEg1 = withTests 1 $
  property $ do
    let inp =
          Text.unlines
            [ "(math",
              "  (eq m (/ (- qy py)(- qx px)))",
              ")"
            ]
        expected =
          SexpList
            (mkLoc 1 1 3 2)
            [ SexpAtom (mkLoc 1 2 1 6) (AtomSym "math"),
              SexpList
                (mkLoc 2 3 2 32)
                [ SexpAtom (mkLoc 2 4 2 6) (AtomSym "eq"),
                  SexpAtom (mkLoc 2 7 2 8) (AtomSym "m"),
                  SexpList
                    (mkLoc 2 9 2 31)
                    [ SexpAtom (mkLoc 2 10 2 11) (AtomSym "/"),
                      SexpList
                        (mkLoc 2 12 2 21)
                        [ SexpAtom (mkLoc 2 13 2 14) (AtomSym "-"),
                          SexpAtom (mkLoc 2 15 2 17) (AtomSym "qy"),
                          SexpAtom (mkLoc 2 18 2 20) (AtomSym "py")
                        ],
                      SexpList
                        (mkLoc 2 21 2 30)
                        [ SexpAtom (mkLoc 2 22 2 23) (AtomSym "-"),
                          SexpAtom (mkLoc 2 24 2 26) (AtomSym "qx"),
                          SexpAtom (mkLoc 2 27 2 29) (AtomSym "px")
                        ]
                    ]
                ]
            ]
    let outM = MP.parse Parse.sexp "" inp
    case outM of
      Left err -> do
        liftIO . putStrLn . MP.errorBundlePretty $ err
        fail "Parse failed."
      Right out -> expected === out
