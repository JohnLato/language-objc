{-# OPTIONS -cpp #-}
{-# LINE 63 "Language/C/Parser/Lexer.x" #-}

module Language.C.Parser.Lexer (lexC, parseError) where

import Data.Char (isDigit)
import Numeric   (readDec, readOct, readHex)

import Language.C.Toolkit.Position  (Position(..), Pos(posOf))
import Language.C.Toolkit.Idents    (lexemeToIdent)

import Language.C.Parser.Tokens
import Language.C.Toolkit.ParserMonad


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#else
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,150) [-8,110,115,0,120,130,156,182,191,217,243,252,278,304,313,339,365,446,542,638,734,830,926,1020,1093,0,-4,-3,-89,-100,-93,-86,-102,0,3,4,-101,-94,-84,-82,1131,1220,1295,354,1372,174,204,0,1410,1448,264,265,0,1518,1588,348,349,0,0,-20,1037,1045,-19,1658,128,1682,1755,0,-18,1116,1486,-17,1778,185,1833,1906,0,1940,1969,1557,1862,1626,1851,2001,1053,2030,1382,0,123,184,1509,2054,245,2082,2106,2179,0,247,367,2204,2250,1047,2258,2282,2355,0,0,0,0,0,87,-38,0,0,92,0,-33,-5,23,99,27,83,70,84,0,0,0,73,0,89,95,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,105]

alex_table :: Array Int Int
alex_table = listArray (0,2610) [0,2,2,2,2,2,25,25,26,28,29,30,31,33,33,34,36,37,38,58,58,67,67,128,2,111,88,15,137,118,119,64,105,106,116,114,145,110,81,117,43,48,48,48,48,48,48,48,48,48,134,146,121,127,123,133,138,40,40,40,40,40,40,40,40,40,40,40,41,40,40,40,40,40,40,40,40,40,40,40,40,40,40,107,139,108,129,40,143,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,147,130,148,112,2,2,2,2,2,2,2,2,2,2,8,3,144,115,-1,126,113,-1,131,-1,8,3,-1,2,120,124,125,122,2,136,109,141,149,8,135,17,0,142,87,0,0,140,0,8,0,17,9,3,-1,5,6,6,6,6,6,6,6,6,6,5,6,6,6,6,6,6,6,6,6,9,0,17,9,3,0,-1,-1,0,-1,-1,0,8,3,0,0,7,6,6,6,6,6,6,6,6,6,9,94,17,0,87,132,65,0,0,8,-1,17,9,3,0,0,7,6,6,6,6,6,6,6,6,6,12,12,12,12,12,12,12,12,12,9,46,17,11,3,0,-1,0,-1,-1,46,-1,11,3,0,0,0,12,12,12,12,12,12,12,12,12,11,94,74,0,87,47,96,46,0,11,0,0,14,3,47,0,46,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,14,0,47,14,3,0,0,0,0,0,0,47,14,3,0,0,13,12,12,12,12,12,12,12,12,12,14,94,0,103,51,52,0,0,0,14,0,0,16,51,52,0,13,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,16,51,52,16,0,0,-1,0,0,-1,51,52,0,0,0,0,0,4,4,4,4,4,4,4,4,4,16,0,0,77,96,44,44,44,44,44,44,44,44,82,82,0,0,4,4,4,4,4,4,4,4,4,85,56,57,0,0,0,0,45,0,0,56,57,0,0,0,0,45,0,0,53,0,39,0,0,0,0,0,0,32,0,0,0,85,56,57,0,103,0,0,45,0,0,56,57,0,0,0,39,45,0,0,53,0,0,32,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,20,20,20,20,20,20,20,20,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,21,21,21,21,21,21,21,21,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,10,18,18,18,18,18,18,18,18,18,18,18,18,18,22,22,22,22,22,22,22,22,22,22,18,18,18,18,18,18,18,22,22,22,22,22,22,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,23,18,18,18,18,22,22,22,22,22,22,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,0,0,-1,0,18,-1,0,0,0,0,0,0,0,19,19,19,19,19,19,19,19,58,0,0,0,0,96,0,18,58,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,79,79,79,79,79,79,79,79,79,79,0,18,0,0,0,0,18,18,0,0,18,18,0,0,0,0,0,0,0,18,0,0,0,18,0,18,0,18,103,24,22,22,22,22,22,22,22,22,22,22,0,0,0,0,67,0,0,22,22,22,22,22,22,70,70,70,70,70,70,70,70,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,0,22,22,22,22,22,22,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,42,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,97,0,0,0,0,73,0,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,42,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,0,0,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,0,0,0,0,42,0,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,77,0,44,44,44,44,44,44,44,44,82,82,80,80,80,80,80,80,80,80,80,80,0,85,0,0,0,0,0,0,45,0,0,0,0,0,0,0,77,45,49,49,49,49,49,49,49,49,49,49,0,0,0,0,0,85,0,0,0,0,0,85,45,0,0,0,0,0,50,0,0,45,0,0,0,0,77,50,49,49,49,49,49,49,49,49,49,49,0,0,0,0,0,85,0,0,0,0,0,85,50,-1,0,0,-1,0,50,67,0,50,0,0,0,0,0,50,71,71,71,71,71,71,71,71,0,87,0,0,0,0,0,85,0,0,0,0,0,0,50,91,91,91,91,91,91,91,91,50,54,54,54,54,54,54,54,54,54,54,0,0,0,0,0,0,0,54,54,54,54,54,54,0,0,0,0,0,55,0,0,0,0,0,0,94,0,55,0,79,79,79,79,79,79,79,79,79,79,54,54,54,54,54,54,0,0,0,0,0,55,76,0,0,0,0,0,76,0,55,54,54,54,54,54,54,54,54,54,54,0,0,0,0,0,0,0,54,54,54,54,54,54,76,0,0,0,0,55,76,0,0,0,0,0,0,150,55,78,78,78,78,78,78,78,78,78,78,0,54,54,54,54,54,54,0,0,0,0,0,55,58,0,0,0,0,0,0,0,55,63,63,63,63,63,63,63,63,63,63,59,0,0,0,0,59,0,63,63,63,63,63,63,0,60,60,60,60,60,60,60,60,0,0,0,0,0,0,0,59,0,0,0,0,0,0,0,0,0,63,63,63,63,63,63,0,0,0,0,0,0,0,0,0,0,0,0,0,59,0,0,0,0,59,59,0,0,59,59,0,0,0,0,0,0,0,59,0,0,0,59,0,59,0,59,0,66,63,63,63,63,63,63,63,63,63,63,0,0,0,0,67,0,0,63,63,63,63,63,63,72,72,72,72,72,72,72,72,72,72,0,0,0,0,0,0,0,72,72,72,72,72,72,0,0,0,63,63,63,63,63,63,0,0,0,0,0,0,0,0,0,68,0,0,0,0,68,0,0,72,72,72,72,72,72,69,69,69,69,69,69,69,69,0,0,0,0,0,0,0,68,77,0,82,82,82,82,82,82,82,82,82,82,0,80,80,80,80,80,80,80,80,80,80,85,0,0,0,0,68,0,0,0,0,68,68,76,0,68,68,0,0,76,0,0,0,0,68,0,0,0,68,0,68,0,68,85,75,72,72,72,72,72,72,72,72,72,72,76,0,0,0,0,0,76,72,72,72,72,72,72,0,0,0,0,0,0,0,0,0,0,0,78,78,78,78,78,78,78,78,78,78,0,0,0,0,0,72,72,72,72,72,72,83,76,0,0,0,0,0,76,78,78,78,78,78,78,78,78,78,78,0,0,0,0,0,0,0,0,0,0,0,83,76,0,83,76,0,84,76,84,0,76,79,79,79,79,79,79,79,79,79,79,0,0,0,0,0,-1,0,0,-1,0,0,83,76,0,86,0,86,0,76,80,80,80,80,80,80,80,80,80,80,87,0,0,0,-1,0,0,-1,0,0,0,0,0,0,92,92,92,92,92,92,92,92,0,0,0,0,0,0,87,0,0,0,0,0,0,0,0,0,0,0,0,0,93,93,93,93,93,93,93,93,93,93,89,0,0,0,0,89,94,93,93,93,93,93,93,0,90,90,90,90,90,90,90,90,0,0,0,0,0,0,0,89,0,0,0,0,94,0,0,0,0,93,93,93,93,93,93,0,0,0,0,0,0,0,0,0,0,0,0,0,89,0,0,0,0,89,89,0,0,89,89,0,0,0,0,0,-1,0,89,-1,0,0,89,0,89,0,89,0,95,93,93,93,93,93,93,93,93,93,93,0,96,0,0,0,0,0,93,93,93,93,93,93,0,0,100,100,100,100,100,100,100,100,-1,0,0,-1,0,0,0,0,-1,0,0,-1,0,0,0,0,93,93,93,93,93,93,0,0,96,0,0,0,0,0,0,0,96,0,0,0,103,0,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,98,0,0,0,0,98,0,102,102,102,102,102,102,0,99,99,99,99,99,99,99,99,0,0,0,0,103,0,0,98,0,0,0,0,103,0,0,0,0,102,102,102,102,102,102,0,0,0,0,0,0,0,0,0,0,0,0,0,98,0,0,0,0,98,98,0,0,98,98,0,0,0,0,0,0,0,98,0,0,0,98,0,98,0,98,0,104,102,102,102,102,102,102,102,102,102,102,0,0,0,0,0,0,0,102,102,102,102,102,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,102,102,102,102,102,102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,2610) [-1,9,10,11,12,13,10,10,97,109,103,97,114,10,10,116,110,101,100,39,39,39,39,61,32,33,34,35,61,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,61,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,61,93,94,95,61,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,9,10,11,12,13,9,10,11,12,13,9,10,61,45,10,61,43,13,38,10,9,10,13,32,60,61,61,62,32,61,62,61,46,32,61,34,-1,61,34,-1,-1,61,-1,32,-1,34,9,10,39,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,32,-1,34,9,10,-1,10,10,-1,13,13,-1,9,10,-1,-1,48,49,50,51,52,53,54,55,56,57,32,92,34,-1,34,124,92,-1,-1,32,39,34,9,10,-1,-1,48,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,32,76,34,9,10,-1,10,-1,10,13,85,13,9,10,-1,-1,-1,49,50,51,52,53,54,55,56,57,32,92,92,-1,34,76,34,108,-1,32,-1,-1,9,10,85,-1,117,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,32,-1,108,9,10,-1,-1,-1,-1,-1,-1,117,9,10,-1,-1,48,49,50,51,52,53,54,55,56,57,32,92,-1,92,76,76,-1,-1,-1,32,-1,-1,9,85,85,-1,48,49,50,51,52,53,54,55,56,57,49,50,51,52,53,54,55,56,57,32,108,108,9,-1,-1,10,-1,-1,13,117,117,-1,-1,-1,-1,-1,49,50,51,52,53,54,55,56,57,32,-1,-1,46,34,48,49,50,51,52,53,54,55,56,57,-1,-1,49,50,51,52,53,54,55,56,57,69,76,76,-1,-1,-1,-1,76,-1,-1,85,85,-1,-1,-1,-1,85,-1,-1,88,-1,105,-1,-1,-1,-1,-1,-1,112,-1,-1,-1,101,108,108,-1,92,-1,-1,108,-1,-1,117,117,-1,-1,-1,105,117,-1,-1,120,-1,-1,112,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,34,-1,-1,10,-1,39,13,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,39,-1,-1,-1,-1,34,-1,63,39,48,49,50,51,52,53,54,55,48,49,50,51,52,53,54,55,48,49,50,51,52,53,54,55,56,57,-1,92,-1,-1,-1,-1,97,98,-1,-1,101,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,-1,118,92,120,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,39,-1,-1,65,66,67,68,69,70,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,97,98,99,100,101,102,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,34,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,-1,-1,-1,-1,-1,-1,76,-1,-1,-1,-1,-1,-1,-1,46,85,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,69,108,-1,-1,-1,-1,-1,76,-1,-1,117,-1,-1,-1,-1,46,85,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,69,108,10,-1,-1,13,-1,76,39,-1,117,-1,-1,-1,-1,-1,85,48,49,50,51,52,53,54,55,-1,34,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,108,48,49,50,51,52,53,54,55,117,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,-1,-1,76,-1,-1,-1,-1,-1,-1,92,-1,85,-1,48,49,50,51,52,53,54,55,56,57,97,98,99,100,101,102,-1,-1,-1,-1,-1,108,70,-1,-1,-1,-1,-1,76,-1,117,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,102,-1,-1,-1,-1,76,108,-1,-1,-1,-1,-1,-1,46,85,48,49,50,51,52,53,54,55,56,57,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,108,39,-1,-1,-1,-1,-1,-1,-1,117,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,-1,39,-1,65,66,67,68,69,70,-1,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,-1,63,-1,-1,-1,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,-1,-1,101,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,-1,118,-1,120,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,39,-1,-1,65,66,67,68,69,70,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,-1,-1,34,-1,-1,-1,-1,39,-1,-1,97,98,99,100,101,102,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,-1,63,46,-1,48,49,50,51,52,53,54,55,56,57,-1,48,49,50,51,52,53,54,55,56,57,69,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,70,-1,101,102,-1,-1,76,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,-1,118,101,120,48,49,50,51,52,53,54,55,56,57,102,-1,-1,-1,-1,-1,108,65,66,67,68,69,70,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,97,98,99,100,101,102,69,70,-1,-1,-1,-1,-1,76,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,69,70,-1,101,102,-1,43,76,45,-1,108,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,10,-1,-1,13,-1,-1,101,102,-1,43,-1,45,-1,108,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,10,-1,-1,13,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,34,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,-1,39,92,65,66,67,68,69,70,-1,48,49,50,51,52,53,54,55,-1,-1,-1,-1,-1,-1,-1,63,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,-1,-1,101,102,-1,-1,-1,-1,-1,10,-1,110,13,-1,-1,114,-1,116,-1,118,-1,120,48,49,50,51,52,53,54,55,56,57,-1,34,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,48,49,50,51,52,53,54,55,10,-1,-1,13,-1,-1,-1,-1,10,-1,-1,13,-1,-1,-1,-1,97,98,99,100,101,102,-1,-1,34,-1,-1,-1,-1,-1,-1,-1,34,-1,-1,-1,92,-1,48,49,50,51,52,53,54,55,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,-1,39,-1,65,66,67,68,69,70,-1,48,49,50,51,52,53,54,55,-1,-1,-1,-1,92,-1,-1,63,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,-1,-1,97,98,-1,-1,101,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,-1,118,-1,120,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,150) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,27,-1,-1,-1,-1,-1,-1,35,35,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,59,-1,-1,-1,-1,-1,-1,-1,-1,68,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,89,89,89,89,89,89,-1,-1,-1,98,98,98,98,98,98,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,150) [[],[],[(AlexAccSkip)],[(AlexAcc (alex_action_1))],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[(AlexAccSkip)],[],[],[],[],[],[],[],[(AlexAccSkip)],[],[],[],[],[],[],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_8))],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_9))],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_18))],[],[],[],[],[],[(AlexAcc (alex_action_11))],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_12))],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_14))],[(AlexAcc (alex_action_15))],[(AlexAcc (alex_action_16))],[(AlexAcc (alex_action_17))],[(AlexAcc (alex_action_24))],[(AlexAcc (alex_action_19))],[(AlexAcc (alex_action_20))],[(AlexAcc (alex_action_21))],[(AlexAcc (alex_action_23))],[(AlexAcc (alex_action_22))],[(AlexAcc (alex_action_25))],[(AlexAcc (alex_action_26))],[(AlexAcc (alex_action_27))],[(AlexAcc (alex_action_28))],[(AlexAcc (alex_action_29))],[(AlexAcc (alex_action_31))],[(AlexAcc (alex_action_30))],[(AlexAcc (alex_action_33))],[(AlexAcc (alex_action_32))],[(AlexAcc (alex_action_34))],[(AlexAcc (alex_action_35))],[(AlexAcc (alex_action_43))],[(AlexAcc (alex_action_36))],[(AlexAcc (alex_action_37))],[(AlexAcc (alex_action_38))],[(AlexAcc (alex_action_39))],[(AlexAcc (alex_action_40))],[(AlexAcc (alex_action_41))],[(AlexAcc (alex_action_42))],[(AlexAcc (alex_action_44))],[(AlexAcc (alex_action_45))],[(AlexAcc (alex_action_46))],[(AlexAcc (alex_action_47))],[(AlexAcc (alex_action_48))],[(AlexAcc (alex_action_49))],[(AlexAcc (alex_action_50))],[(AlexAcc (alex_action_51))],[(AlexAcc (alex_action_52))],[(AlexAcc (alex_action_53))],[(AlexAcc (alex_action_54))],[(AlexAcc (alex_action_55))],[(AlexAcc (alex_action_56))],[(AlexAcc (alex_action_57))],[(AlexAcc (alex_action_58))],[]]
{-# LINE 226 "Language/C/Parser/Lexer.x" #-}

-- We use the odd looking list of string patterns here rather than normal
-- string literals since GHC converts the latter into a sequence of string
-- comparisons (ie a linear search) but it translates the former using its
-- effecient pattern matching which gives us the expected radix-style search.
-- This gives change makes a significant performance difference.
--
idkwtok :: String -> Position -> P CToken
idkwtok ('a':'l':'i':'g':'n':'o':'f':[])		     = tok CTokAlignof
idkwtok ('_':'_':'a':'l':'i':'g':'n':'o':'f':[])	     = tok CTokAlignof
idkwtok ('_':'_':'a':'l':'i':'g':'n':'o':'f':'_':'_':[])     = tok CTokAlignof
idkwtok ('a':'s':'m':[])				     = tok CTokAsm
idkwtok ('_':'_':'a':'s':'m':[])			     = tok CTokAsm
idkwtok ('_':'_':'a':'s':'m':'_':'_':[])		     = tok CTokAsm
idkwtok ('a':'u':'t':'o':[])				     = tok CTokAuto
idkwtok ('b':'r':'e':'a':'k':[])			     = tok CTokBreak
idkwtok ('_':'B':'o':'o':'l':[])			     = tok CTokBool
idkwtok ('c':'a':'s':'e':[])				     = tok CTokCase
idkwtok ('c':'h':'a':'r':[])				     = tok CTokChar
idkwtok ('c':'o':'n':'s':'t':[])			     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':[])		     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':'_':'_':[])	     = tok CTokConst
idkwtok ('c':'o':'n':'t':'i':'n':'u':'e':[])		     = tok CTokContinue
idkwtok ('_':'C':'o':'m':'p':'l':'e':'x':[])		     = tok CTokComplex
idkwtok ('d':'e':'f':'a':'u':'l':'t':[])		     = tok CTokDefault
idkwtok ('d':'o':[])					     = tok CTokDo
idkwtok ('d':'o':'u':'b':'l':'e':[])			     = tok CTokDouble
idkwtok ('e':'l':'s':'e':[])				     = tok CTokElse
idkwtok ('e':'n':'u':'m':[])				     = tok CTokEnum
idkwtok ('e':'x':'t':'e':'r':'n':[])			     = tok CTokExtern
idkwtok ('f':'l':'o':'a':'t':[])			     = tok CTokFloat
idkwtok ('f':'o':'r':[])				     = tok CTokFor
idkwtok ('g':'o':'t':'o':[])				     = tok CTokGoto
idkwtok ('i':'f':[])					     = tok CTokIf
idkwtok ('i':'n':'l':'i':'n':'e':[])			     = tok CTokInline
idkwtok ('_':'_':'i':'n':'l':'i':'n':'e':[])		     = tok CTokInline
idkwtok ('_':'_':'i':'n':'l':'i':'n':'e':'_':'_':[])	     = tok CTokInline
idkwtok ('i':'n':'t':[])				     = tok CTokInt
idkwtok ('l':'o':'n':'g':[])				     = tok CTokLong
idkwtok ('r':'e':'g':'i':'s':'t':'e':'r':[])		     = tok CTokRegister
idkwtok ('r':'e':'s':'t':'r':'i':'c':'t':[])		     = tok CTokRestrict
idkwtok ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':[])	     = tok CTokRestrict
idkwtok ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':'_':'_':[]) = tok CTokRestrict
idkwtok ('r':'e':'t':'u':'r':'n':[])			     = tok CTokReturn
idkwtok ('s':'h':'o':'r':'t':[])			     = tok CTokShort
idkwtok ('s':'i':'g':'n':'e':'d':[])			     = tok CTokSigned
idkwtok ('_':'_':'s':'i':'g':'n':'e':'d':[])		     = tok CTokSigned
idkwtok ('_':'_':'s':'i':'g':'n':'e':'d':'_':'_':[])	     = tok CTokSigned
idkwtok ('s':'i':'z':'e':'o':'f':[])			     = tok CTokSizeof
idkwtok ('s':'t':'a':'t':'i':'c':[])			     = tok CTokStatic
idkwtok ('s':'t':'r':'u':'c':'t':[])			     = tok CTokStruct
idkwtok ('s':'w':'i':'t':'c':'h':[])			     = tok CTokSwitch
idkwtok ('t':'y':'p':'e':'d':'e':'f':[])		     = tok CTokTypedef
idkwtok ('t':'y':'p':'e':'o':'f':[])			     = tok CTokTypeof
idkwtok ('_':'_':'t':'y':'p':'e':'o':'f':[])		     = tok CTokTypeof
idkwtok ('_':'_':'t':'y':'p':'e':'o':'f':'_':'_':[])	     = tok CTokTypeof
idkwtok ('_':'_':'t':'h':'r':'e':'a':'d':[])		     = tok CTokThread
idkwtok ('u':'n':'i':'o':'n':[])			     = tok CTokUnion
idkwtok ('u':'n':'s':'i':'g':'n':'e':'d':[])		     = tok CTokUnsigned
idkwtok ('v':'o':'i':'d':[])				     = tok CTokVoid
idkwtok ('v':'o':'l':'a':'t':'i':'l':'e':[])		     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':[])	     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':'_':'_':[]) = tok CTokVolatile
idkwtok ('w':'h':'i':'l':'e':[])			     = tok CTokWhile
idkwtok ('_':'_':'l':'a':'b':'e':'l':'_':'_':[])             = tok CTokLabel
idkwtok ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':[]) = tok (CTokGnuC GnuCAttrTok)
--						ignoreAttribute >> lexToken
idkwtok ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':'_':'_':[]) = tok (CTokGnuC GnuCAttrTok)
--						ignoreAttribute >> lexToken
idkwtok ('_':'_':'e':'x':'t':'e':'n':'s':'i':'o':'n':'_':'_':[]) =
						tok (CTokGnuC GnuCExtTok)
idkwtok ('_':'_':'b':'u':'i':'l':'t':'i':'n':'_':rest)
        | rest == "va_arg"             = tok (CTokGnuC GnuCVaArg)
        | rest == "offsetof"           = tok (CTokGnuC GnuCOffsetof)
        | rest == "types_compatible_p" = tok (CTokGnuC GnuCTyCompat)

idkwtok cs = \pos -> do
  name <- getNewName
  let ident = lexemeToIdent pos cs name
  tyident <- isTypeIdent ident
  if tyident
    then return (CTokTyIdent pos ident)
    else return (CTokIdent   pos ident)

ignoreAttribute :: P ()
ignoreAttribute = skipTokens 0
  where skipTokens n = do
          tok <- lexToken
          case tok of
            CTokRParen _ | n == 1    -> return ()
                         | otherwise -> skipTokens (n-1)
            CTokLParen _             -> skipTokens (n+1)
            _                        -> skipTokens n

tok :: (Position -> CToken) -> Position -> P CToken
tok tc pos = return (tc pos)

-- converts the first character denotation of a C-style string to a character
-- and the remaining string
--
oneChar             :: String -> (Char, String)
oneChar ('\\':c:cs)  = case c of
			 'n'  -> ('\n', cs)
			 't'  -> ('\t', cs)
			 'v'  -> ('\v', cs)
			 'b'  -> ('\b', cs)
			 'r'  -> ('\r', cs)
			 'f'  -> ('\f', cs)
			 'a'  -> ('\a', cs)
			 'e'  -> ('\ESC', cs)  --GNU C extension
			 '\\' -> ('\\', cs)
			 '?'  -> ('?', cs)
			 '\'' -> ('\'', cs)
			 '"'  -> ('"', cs)
			 'x'  -> case head (readHex cs) of
			           (i, cs') -> (toEnum i, cs')
			 _    -> case head (readOct (c:cs)) of
			           (i, cs') -> (toEnum i, cs')
oneChar (c   :cs)    = (c, cs)

normalizeEscapes [] = []
normalizeEscapes cs = case oneChar cs of
                        (c, cs') -> c : normalizeEscapes cs'

adjustPos :: String -> Position -> Position
adjustPos str (Position fname row _) = Position fname' row' 0
  where
    str'            = dropWhite . drop 1 $ str
    (rowStr, str'') = span isDigit str'
    row'	    = read rowStr
    str'''	    = dropWhite str''
    fnameStr	    = takeWhile (/= '"') . drop 1 $ str'''
    fname'	    | null str''' || head str''' /= '"'	= fname
		    -- try and get more sharing of file name strings
		    | fnameStr == fname			= fname
		    | otherwise				= fnameStr
    --
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

{-# INLINE token_ #-}
-- token that ignores the string
token_ :: (Position -> CToken) -> Position -> Int -> String -> P CToken
token_ tok pos _ _ = return (tok pos)

{-# INLINE token #-}
-- token that uses the string
token :: (Position -> a -> CToken) -> (String -> a)
      -> Position -> Int -> String -> P CToken
token tok read pos len str = return (tok pos (read $ take len str))


-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (Position, 	-- current position,
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,[]) = Nothing
alexGetChar (p,(c:s))  = let p' = alexMove p c in p' `seq`
                           Just (c, (p', s))

alexMove :: Position -> Char -> Position
alexMove (Position f l c) '\t' = Position f l     (((c+7) `div` 8)*8+1)
alexMove (Position f l c) '\n' = Position f (l+1) 1
alexMove (Position f l c) _    = Position f l     (c+1)

lexicalError :: P a
lexicalError = do
  pos <- getPos
  (c:cs) <- getInput
  failP pos
        ["Lexical error!",
         "The character " ++ show c ++ " does not fit here."]

parseError :: P a
parseError = do
  tok <- getLastToken
  failP (posOf tok)
        ["Syntax error!",
         "The symbol `" ++ show tok ++ "' does not fit here."]

lexToken :: P CToken
lexToken = do
  pos <- getPos
  inp <- getInput
  case alexScan (pos, inp) 0 of
    AlexEOF -> return CTokEof
    AlexError inp' -> lexicalError
    AlexSkip  (pos', inp') len -> do
        setPos pos'
        setInput inp'
	lexToken
    AlexToken (pos', inp') len action -> do
        setPos pos'
        setInput inp'
        tok <- action pos len inp
        setLastToken tok
        return tok

lexC :: (CToken -> P a) -> P a
lexC cont = do
  tok <- lexToken
  cont tok

alex_action_1 = \pos len str -> setPos (adjustPos (take len str) pos) >> lexToken 
alex_action_4 = \pos len str -> idkwtok (take len str) pos 
alex_action_5 = token CTokILit (fst . head . readOct) 
alex_action_6 = token CTokILit (fst . head . readDec) 
alex_action_7 = token CTokILit (fst . head . readHex . drop 2) 
alex_action_8 = token CTokCLit (fst . oneChar . tail) 
alex_action_9 = token CTokCLit (fst . oneChar . tail . tail) 
alex_action_10 = token CTokFLit id 
alex_action_11 = token CTokSLit normalizeEscapes 
alex_action_12 = token CTokSLit (normalizeEscapes . tail) 
alex_action_13 = token_ CTokLParen 
alex_action_14 = token_ CTokRParen  
alex_action_15 = token_ CTokLBracket 
alex_action_16 = token_ CTokRBracket 
alex_action_17 = token_ CTokArrow 
alex_action_18 = token_ CTokDot 
alex_action_19 = token_ CTokExclam 
alex_action_20 = token_ CTokTilde 
alex_action_21 = token_ CTokInc 
alex_action_22 = token_ CTokDec 
alex_action_23 = token_ CTokPlus 
alex_action_24 = token_ CTokMinus 
alex_action_25 = token_ CTokStar 
alex_action_26 = token_ CTokSlash 
alex_action_27 = token_ CTokPercent 
alex_action_28 = token_ CTokAmper 
alex_action_29 = token_ CTokShiftL 
alex_action_30 = token_ CTokShiftR 
alex_action_31 = token_ CTokLess 
alex_action_32 = token_ CTokLessEq 
alex_action_33 = token_ CTokHigh 
alex_action_34 = token_ CTokHighEq 
alex_action_35 = token_ CTokEqual 
alex_action_36 = token_ CTokUnequal 
alex_action_37 = token_ CTokHat 
alex_action_38 = token_ CTokBar 
alex_action_39 = token_ CTokAnd 
alex_action_40 = token_ CTokOr 
alex_action_41 = token_ CTokQuest 
alex_action_42 = token_ CTokColon 
alex_action_43 = token_ CTokAssign 
alex_action_44 = token_ CTokPlusAss 
alex_action_45 = token_ CTokMinusAss 
alex_action_46 = token_ CTokStarAss 
alex_action_47 = token_ CTokSlashAss 
alex_action_48 = token_ CTokPercAss 
alex_action_49 = token_ CTokAmpAss 
alex_action_50 = token_ CTokHatAss 
alex_action_51 = token_ CTokBarAss 
alex_action_52 = token_ CTokSLAss 
alex_action_53 = token_ CTokSRAss 
alex_action_54 = token_ CTokComma 
alex_action_55 = token_ CTokSemic 
alex_action_56 = token_ CTokLBrace 
alex_action_57 = token_ CTokRBrace 
alex_action_58 = token_ CTokEllipsis 
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "GenericTemplate.hs" #-}

{-# LINE 45 "GenericTemplate.hs" #-}

{-# LINE 66 "GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 87 "GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 98 "GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
