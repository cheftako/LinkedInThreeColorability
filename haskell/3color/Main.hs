-- | Chris Pettitt's 3-Coloring Solver

module Main where

import Control.Monad.State
import System.Environment (getArgs, getProgName)
import System.FilePath.Posix

import CSP
import FastSolver

import Parse (load)

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    when (null args) (error $ "usage: " ++ progName ++ " FILE")

    let graphFile = head args
        outFile = replaceFileName graphFile (progName ++ "_" ++ takeFileName graphFile ++ "_out")

    inGraph <- load graphFile
    
    let csp = fromGraph inGraph
    let solution = solve csp

    case solution of
      Just x ->
        writeFile outFile ("true\n" ++ showSolution x)
      Nothing ->
        writeFile outFile "false\n"
