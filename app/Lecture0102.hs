module Lecture0102 where

-- Covers Lecture 1 and 2.

annotatedBoard :: [[(Char, Int)]]
annotatedBoard = [[(file, rank) | rank <- [1 .. 8]] | file <- reverse ['a' .. 'h']]

capitalPiecesFile :: [Char]
capitalPiecesFile = ['R', 'N', 'B', 'Q', 'K'] ++ reverse (take 3 capitalPiecesFile)

initialBoard :: [String]
initialBoard =
  [ capitalPiecesFile,
    replicate 8 'i'
  ]
    ++ replicate 4 (replicate 8 '.')
    ++ reverse (take 2 initialBoard)

initialPieces :: [(Char, (Char, Int))]
initialPieces =
  concat
    [ annotateFile capitalPiecesFile 'h',
      annotateFile (replicate 8 'i') 'g',
      annotateFile (replicate 8 'i') 'b',
      annotateFile capitalPiecesFile 'a'
    ]

-- helper for initialPieces
annotateFile :: [Char] -> Char -> [(Char, (Char, Int))]
annotateFile pieces file = zip pieces [(file, rank) | rank <- [1 .. 8]]

scorePieceType :: Char -> Int
scorePieceType c
  | c == 'K' = 100
  | c == 'Q' = 9
  | c == 'R' = 5
  | c == 'B' || c == 'N' = 3
  | c == 'i' = 1
  | otherwise = error "Invalid piece type"

scorePlayer :: [Char] -> Int
scorePlayer pieces = sum [scorePieceType p | p <- pieces]

-- You are given only a list of pieces each player has left.
-- Predict winner based on that, using the score for each piece type.
-- Answer must be "White won", "Black won" or "Draw".
predictWinner :: [Char] -> [Char] -> String
predictWinner whitePieces blackPieces
  | scorePlayer whitePieces > scorePlayer blackPieces = "White won"
  | scorePlayer whitePieces < scorePlayer blackPieces = "Black won"
  | otherwise = "Draw"
