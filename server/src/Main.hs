{-# LANGUAGE CPP #-}

module Main
(
    main
)
where

#ifdef DEBUG
import RunLocal (run)
#else
import RunNetwork (run)
#endif

main :: IO ()
main = run
