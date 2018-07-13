{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest
main = defaultMainWithDoctests "doctests"

#else

import Distribution.Simple (defaultMain)
main = defaultMain

#endif
