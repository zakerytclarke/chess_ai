import Data.Hashable
data Color = Black | White deriving (Eq, Show)

data Piece = Pawn Color | Rook Color | Knight Color | Bishop Color | Queen Color | King Color | Empty | DNE

instance Show Piece where
  show (Pawn Black) = "p"
  show (Rook Black) = "r"
  show (Knight Black) = "n"
  show (Bishop Black) = "b"
  show (Queen Black) = "q"
  show (King Black) = "k"
  show (Pawn White) = "P"
  show (Rook White) = "R"
  show (Knight White) = "N"
  show (Bishop White) = "B"
  show (Queen White) = "Q"
  show (King White) = "K"
  show Empty = "_"
  show DNE = "E" -- Piece is off the board


opposite Black = White
opposite White = Black

color Empty = Black

color (Pawn Black) = Black
color (Rook Black) = Black
color (Knight Black) = Black
color (Bishop Black) = Black
color (Queen Black) = Black
color (King Black) = Black

color (Pawn White) = White
color (Rook White) = White
color (Knight White) = White
color (Bishop White) = White
color (Queen White) = White
color (King White) = White


value (Empty) = 0
value (Pawn White) = 1
value (Rook White) = 5
value (Knight White) = 3
value (Bishop White) = 3
value (Queen White) = 9
value (King White) = 1000

value (Pawn Black) = -1
value (Rook Black) = -5
value (Knight Black) = -3
value (Bishop Black) = -3
value (Queen Black) = -9
value (King Black) = -1000



board=
  [
    [(Rook Black),(Knight Black),(Bishop Black),(Queen Black),(King Black),(Bishop Black),(Knight Black),(Rook Black)],
    [(Pawn Black),(Pawn Black),(Pawn Black),(Pawn Black),(Pawn Black),(Pawn Black),(Pawn Black),(Pawn Black)],
    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
    [(Pawn White),(Pawn White),(Pawn White),(Pawn White),(Pawn White),(Pawn White),(Pawn White),(Pawn White)],
    [(Rook White),(Knight White),(Bishop White),(Queen White),(King White),(Bishop White),(Knight White),(Rook White)]
  ]


difficulty = 2


printBoard board = do
  putStrLn (foldr (++) "" (map rowhelper board))
  where rowhelper row = (foldr ((++) . show) "" row)++"\n"

boardHash board color = hash $ (show color)++boardHelper
  where boardHelper = (foldr (++) "" (map rowhelper board))
        rowhelper row = (foldr ((++) . show) "" row)

{-
printBoard board =  foldr (++) "" (map printRow board)
  where printRow row = foldr (++) "" (map printPiece row)++"|\n - - - - - - - -\n"
        printPiece piece = "|" ++ (show piece)
-}


scoreBoard board = (foldr (+) 0 (map rowhelper board))
  where rowhelper row = (foldr ((+) . value) 0 row)



getMoves board x y = piece
  where piece = board !! y !! x


validXY x y = if ((x<0) || (x>7) || (y<0) || (y>7))
                    then False
                    else True

pieceAt board x y = if (validXY x y)
                    then board !! y !! x
                    else DNE

emptyAt board x y = helper (pieceAt board x y)
  where helper Empty = True
        helper DNE = False
        helper n = False


whiteAt board x y = helper (pieceAt board x y)
  where helper Empty = False
        helper DNE = True
        helper n = helperhelper (color n)
        helperhelper Black = False
        helperhelper White = True

blackAt board x y = helper (pieceAt board x y)
  where helper Empty = False
        helper DNE = True
        helper n = helperhelper (color n)
        helperhelper Black = True
        helperhelper White = False



move board x y nx ny = nB2
  where piece = board !! y !! x
        r1 = replace x (board !! y) Empty
        nB1 = replace y board r1
        r2 = replace nx (nB1 !! ny) piece
        nB2 = replace ny nB1 r2

--Moves and returns wrapped board if legal; otherwise []
safeMove board x y nx ny = if (validXY nx ny)
                            then [(move board x y nx ny)]
                            else []




generateMoves :: [[Piece]] -> Int -> Int -> Piece -> [[[Piece]]]
generateMoves board x y (Pawn Black) = move1++move2++move3++move4
  where piece1 = (blackAt board x (y+1)) || (whiteAt board x (y+1))
        piece2 = (blackAt board x (y+2)) || (whiteAt board x (y+2))
        piece3 = (whiteAt board (x+1) (y+1))
        piece4 = (whiteAt board (x-1) (y+1))
        move1 = if (not piece1) --Not a piece one ahead
                  then safeMove board x y x (y+1)
                  else []
        move2 = if (not piece1 && not piece2) --Not a piece one ahead
                  then safeMove board x y x (y+2)
                  else []
        move3 = if piece3 --Not a piece one ahead
                  then safeMove board x y (x+1) (y+1)
                  else []
        move4 = if piece4 --Not a piece one ahead
                  then safeMove board x y (x-1) (y+1)
                  else []

generateMoves board x y (Pawn White) = move1++move2++move3++move4
  where piece1 = (blackAt board x (y-1)) || (whiteAt board x (y-1))
        piece2 = (blackAt board x (y-2)) || (whiteAt board x (y-2))
        piece3 = (blackAt board (x+1) (y-1))
        piece4 = (blackAt board (x-1) (y-1))
        move1 = if (not piece1) --Not a piece one ahead
                  then safeMove board x y x (y-1)
                  else []
        move2 = if (not piece1 && not piece2) --Not a piece one ahead
                  then safeMove board x y x (y-2)
                  else []
        move3 = if piece3 --Not a piece one ahead
                  then safeMove board x y (x+1) (y-1)
                  else []
        move4 = if piece4 --Not a piece one ahead
                  then safeMove board x y (x-1) (y-1)
                  else []


generateMoves board x y (King Black) = move1
  where piece1 = (blackAt board (x+1) y)
        piece2 = (blackAt board (x-1) y)
        piece3 = (blackAt board x (y+1))
        piece4 = (blackAt board x (y-1))
        piece5 = (blackAt board (x+1) (y+1))
        piece6 = (blackAt board (x-1) (y+1))
        piece7 = (blackAt board (x+1) (y-1))
        piece8 = (blackAt board (x-1) (y-1))
        move1= if (not piece1)
                  then safeMove board x y (x+1) y
                  else []
        move2= if (not piece2)
                  then safeMove board x y (x-1) y
                  else []
        move3= if (not piece3)
                  then safeMove board x y x (y+1)
                  else []
        move4= if (not piece4)
                  then safeMove board x y x (y-1)
                  else []
        move5= if (not piece5)
                  then safeMove board x y (x+1) (y+1)
                  else []
        move6= if (not piece6)
                  then safeMove board x y (x-1) (y+1)
                  else []
        move7= if (not piece7)
                  then safeMove board x y (x+1) (y-1)
                  else []
        move8= if (not piece8)
                  then safeMove board x y (x-1) (y-1)
                  else []

generateMoves board x y (King White) = move1++move2++move3++move4++move5++move6++move7++move8
  where piece1 = (whiteAt board (x+1) y)
        piece2 = (whiteAt board (x-1) y)
        piece3 = (whiteAt board x (y+1))
        piece4 = (whiteAt board x (y-1))
        piece5 = (whiteAt board (x+1) (y+1))
        piece6 = (whiteAt board (x-1) (y+1))
        piece7 = (whiteAt board (x+1) (y-1))
        piece8 = (whiteAt board (x-1) (y-1))
        move1= if (not piece1)
                  then safeMove board x y (x+1) y
                  else []
        move2= if (not piece2)
                  then safeMove board x y (x-1) y
                  else []
        move3= if (not piece3)
                  then safeMove board x y x (y+1)
                  else []
        move4= if (not piece4)
                  then safeMove board x y x (y-1)
                  else []
        move5= if (not piece5)
                  then safeMove board x y (x+1) (y+1)
                  else []
        move6= if (not piece6)
                  then safeMove board x y (x-1) (y+1)
                  else []
        move7= if (not piece7)
                  then safeMove board x y (x+1) (y-1)
                  else []
        move8= if (not piece8)
                  then safeMove board x y (x-1) (y-1)
                  else []




generateMoves board x y (Knight Black) = move1++move2++move3++move4++move5++move6++move7++move8
  where piece1 = (blackAt board (x+1) (y-2))
        piece2 = (blackAt board (x+2) (y-1))
        piece3 = (blackAt board (x+2) (y+1))
        piece4 = (blackAt board (x+1) (y+2))
        piece5 = (blackAt board (x-1) (y+2))
        piece6 = (blackAt board (x-2) (y+1))
        piece7 = (blackAt board (x-2) (y-1))
        piece8 = (blackAt board (x-1) (y-2))
        move1= if (not piece1)
                  then safeMove board x y (x+1) (y-2)
                  else []
        move2= if (not piece2)
                  then safeMove board x y (x+2) (y-1)
                  else []
        move3= if (not piece3)
                  then safeMove board x y (x+2) (y+1)
                  else []
        move4= if (not piece4)
                  then safeMove board x y (x+1) (y+2)
                  else []
        move5= if (not piece5)
                  then safeMove board x y (x-1) (y+2)
                  else []
        move6= if (not piece6)
                  then safeMove board x y (x-2) (y+1)
                  else []
        move7= if (not piece7)
                  then safeMove board x y (x-2) (y-1)
                  else []
        move8= if (not piece8)
                  then safeMove board x y (x-1) (y-2)
                  else []

generateMoves board x y (Knight White) = move1++move2++move3++move4++move5++move6++move7++move8
  where piece1 = (whiteAt board (x+1) (y-2))
        piece2 = (whiteAt board (x+2) (y-1))
        piece3 = (whiteAt board (x+2) (y+1))
        piece4 = (whiteAt board (x+1) (y+2))
        piece5 = (whiteAt board (x-1) (y+2))
        piece6 = (whiteAt board (x-2) (y+1))
        piece7 = (whiteAt board (x-2) (y-1))
        piece8 = (whiteAt board (x-1) (y-2))
        move1= if (not piece1)
                  then safeMove board x y (x+1) (y-2)
                  else []
        move2= if (not piece2)
                  then safeMove board x y (x+2) (y-1)
                  else []
        move3= if (not piece3)
                  then safeMove board x y (x+2) (y+1)
                  else []
        move4= if (not piece4)
                  then safeMove board x y (x+1) (y+2)
                  else []
        move5= if (not piece5)
                  then safeMove board x y (x-1) (y+2)
                  else []
        move6= if (not piece6)
                  then safeMove board x y (x-2) (y+1)
                  else []
        move7= if (not piece7)
                  then safeMove board x y (x-2) (y-1)
                  else []
        move8= if (not piece8)
                  then safeMove board x y (x-1) (y-2)
                  else []


generateMoves board x y (Rook Black) = (helper board x y 0 1)++(helper board x y 1 0)++(helper board x y (-1) 0)++(helper board x y 0 (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (blackAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y (Rook White) = (helper board x y 0 1)++(helper board x y 1 0)++(helper board x y (-1) 0)++(helper board x y 0 (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (whiteAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y (Bishop Black) = (helper board x y 1 1)++(helper board x y 1 (-1))++(helper board x y (-1) 1)++(helper board x y (-1) (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (blackAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y (Bishop White) = (helper board x y 1 1)++(helper board x y 1 (-1))++(helper board x y (-1) 1)++(helper board x y (-1) (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (whiteAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y (Queen Black) = (helper board x y 0 1)++(helper board x y 1 0)++(helper board x y (-1) 0)++(helper board x y 0 (-1))++(helper board x y 1 1)++(helper board x y 1 (-1))++(helper board x y (-1) 1)++(helper board x y (-1) (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (blackAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y (Queen White) = (helper board x y 0 1)++(helper board x y 1 0)++(helper board x y (-1) 0)++(helper board x y 0 (-1))++(helper board x y 1 1)++(helper board x y 1 (-1))++(helper board x y (-1) 1)++(helper board x y (-1) (-1))
        --Check if there is a blockage to prevent moving through
        where helper board x y dx dy = if ((emptyAt board nx ny) && (validXY nx ny))
                            then move1++(helper (move board x y nx ny) nx ny dx dy)
                            else move1
                            where nx=x+dx
                                  ny=y+dy
                                  piece1 = (whiteAt board nx ny)
                                  move1 = if (not piece1)
                                            then safeMove board x y nx ny
                                            else []

generateMoves board x y _ = []


getAllMoves board colorDesired = (helper 0 0)
  where helper 7 7 = if ((color (pieceAt board 7 7))==colorDesired)
                      then (generateMoves board 7 7 (pieceAt board 7 7))
                      else []
        helper n 7 = if ((color (pieceAt board n 7))==colorDesired)
                      then (generateMoves board n 7 (pieceAt board n 7))++(helper (n+1) 0)
                      else (helper (n+1) 0)
        helper n m = if ((color (pieceAt board n m))==colorDesired)
                      then (generateMoves board n m (pieceAt board n m))++(helper n (m+1))
                      else (helper n (m+1))



findBest colorDesired arr = helper arr (large,[])
  where op = if (colorDesired==White)
              then (>)
              else (<)
        large = 10000*if (colorDesired==White) then -1 else 1
        helper [] best = best
        helper (x:xs) best = if (op (fst x) (fst best))
                              then (helper xs x)
                              else (helper xs best)




miniMax board colorDesired 0 = findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->(scoreBoard x,x)) possibleMoves

miniMax board colorDesired n = findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->((fst (miniMax x (opposite colorDesired) (n-1))),x)) possibleMoves


miniMaxMemey board colorDesired 0 played = findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->(scoreBoard x,x)) possibleMoves

miniMaxMemey board colorDesired n played = if (inMap (boardHash board colorDesired) played)
                                          then (0,[[]])
                                          else findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->((fst (miniMaxMem x (opposite colorDesired) (n-1) (storeMap (boardHash board colorDesired) played))),x)) possibleMoves

miniMaxMem board colorDesired 0 played = findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->(scoreBoard x,x)) possibleMoves

miniMaxMem board colorDesired n played = if (inMap (boardHash board colorDesired) played)
                                          then (0,[[]],played)
                                          else findBest colorDesired scored
  where possibleMoves = getAllMoves board colorDesired
        scored = map (\x->((fst (miniMaxMem x (opposite colorDesired) (n-1) (storeMap (boardHash board colorDesired) played))),x)) possibleMoves
        scoreHelper [] = []
        scoreHelper (l:ls) = 

play = loop board White
  where loop board currentColor = do
                      putStrLn ""
                      putStrLn ((show currentColor)++"'s move")
                      printBoard board
                      moveIn<-getLine
                      let x= -1+read ((head moveIn):[]) :: Int
                      let y= 8-read ((head (tail (tail moveIn))):[]) :: Int
                      let nx= -1+read ((head (tail (tail (tail (tail moveIn))))):[]) :: Int
                      let ny= 8-read ((head (tail (tail (tail (tail (tail (tail moveIn))))))):[]) :: Int
                      let newBoard=(move board x y nx ny)
                      if ((length moveIn) /= 7)
                        then (loop board currentColor)
                      else if (((color (pieceAt board x y))==currentColor)&&(validXY nx ny))
                            then gameAI newBoard (opposite currentColor)
                            else loop board currentColor
        gameAI board currentColor = do
                          putStrLn ""
                          putStrLn ((show currentColor)++"'s move")
                          printBoard board
                          loop (snd $ miniMax board currentColor difficulty) (opposite currentColor)


playAI = gameAI board White
  where gameAI board currentColor = do
                    putStrLn ""
                    putStrLn ((show currentColor)++"'s move")
                    printBoard board
                    gameAI (snd $ miniMax board currentColor difficulty) (opposite currentColor)



inMap _ [] = False
inMap val (x:xs) = if (x==val)
                    then True
                    else inMap val xs
storeMap val mapey = val:mapey






replace _ [] _ = []
replace 0 (_:xs) a = a:xs
replace n (x:xs) a =
  if n < 0
    then (x:xs)
    else x: replace (n-1) xs a
