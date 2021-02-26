{-# LANGUAGE CPP #-}

module Terminal where

#ifdef mingw32_HOST_OS

foreign import ccall unsafe "terminal.h enable_echo" enableEcho :: IO ()
foreign import ccall unsafe "terminal.h disable_echo" disableEcho :: IO ()

#else

import System.IO

enableEcho = hSetEcho stdout True

disableEcho = hSetEcho stdout False

#endif
