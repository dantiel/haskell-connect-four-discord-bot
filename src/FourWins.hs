module FourWins where
import Data.List.Split

type Move = (Int, Int)
type PlayerMoves = [Move]
type GameState = (PlayerMoves, PlayerMoves)
type Board = [String]


columns :: Int --arbitrary board sizes are possible!
columns = 7    --however the game cannot be won when board is smaller than 4...

rows :: Int    --NOTE: bigger boards require more computing...
rows = 6


middle :: Int
middle = succ (div columns 2)


p1symbol :: Char
p1symbol = 'X'

p2symbol :: Char
p2symbol = 'O'


encodeGameState :: GameState -> String
encodeGameState ([],[]) = "|"
encodeGameState (p1, p2) = encode p1 ++ "|" ++ encode p2    
  where encode = tail . foldr (\(x, y) en -> en ++ ";" ++ show x ++ "," ++ show y) ""

decodeGameState :: String -> GameState
decodeGameState "|" = ([],[])
decodeGameState str = (decodePlayerMoves p1str, decodePlayerMoves p2str)
  where [p1str, p2str] = splitOn "|" str
        arrayToTuple [x, y] = ((read x)::Int, (read y)::Int)
        decodePlayerMoves = map (arrayToTuple . splitOn ",") . splitOn ";"


difficulty :: Int --for all values >2 computations takes big time
difficulty = 2    --this is the amount of possible future turns being taken
                  --into account.
                  --only even values.

blankBoard :: [String]
blankBoard = [[' '|y<-[1..columns]]|x<-[1..rows]]


getPossibleMoves :: PlayerMoves -> PlayerMoves
getPossibleMoves poss = [(i,j)|i<-[1..columns],j<-[(checkAvailability i poss)], j/=0]


checkAvailability :: Int -> PlayerMoves -> Int
checkAvailability column poss = if stack<rows then succ(stack) else 0
  where
    stack=if tmp==[] then 0 else foldr (max) (head tmp) (tail tmp)
    tmp=[snd(pos)|pos<-poss,fst(pos)==column]


getConnectedTwo :: PlayerMoves -> [(Move,Move)]
getConnectedTwo [] = []
getConnectedTwo (_:[]) = []
getConnectedTwo (pos:poss) = [sortTuple(i,pos)|i<-poss,
                                ((1>=abs ((fst i)-(fst pos))) && 1>=abs ((snd i)-(snd pos)))
                             ] ++ getConnectedTwo poss


getConnectedThree :: [(Move,Move)] -> [(Move,(Move,Move))]
getConnectedThree [] = []
getConnectedThree (_:[]) = []
getConnectedThree (pos:poss) = [if (snd i)==(fst pos) then ((fst i),((snd i),(snd pos))) else ((fst pos),((snd pos),(snd i)))|i<-poss,
                                  ((snd i)==(fst pos) || (snd pos)==(fst i)) &&
                                  (((fst (fst i))-(fst (snd i)))==((fst (fst pos))-(fst (snd pos))) &&
                                  (((snd (fst i))-(snd (snd i)))==((snd (fst pos))-(snd (snd pos)))))
                               ] ++ getConnectedThree poss


getConnectedFour :: [(Move,(Move,Move))] -> Int
getConnectedFour [] = 0
getConnectedFour (_:[]) = 0
getConnectedFour (pos:poss) = (length [True|i<-poss,
                                ((fst (snd i))==(fst pos) && (snd (snd i))==(fst (snd pos))) ||
                                ((fst (snd pos))==(fst i) && (snd (snd pos))==(fst (snd i)))
                              ]) + getConnectedFour poss


sortTuple :: (Move,Move) -> (Move,Move)
sortTuple (x,y) = ((min x y),(max x y))


getSituationQuality :: PlayerMoves -> PlayerMoves -> Int -> Int
getSituationQuality p1 p2 0 = ((length pairs_p1)*2 + (length triples_p1)*10 + win_p1*500
                              -((length pairs_p2)*2 + (length triples_p2)*10 + win_p2*500))
  where pairs_p1=getConnectedTwo (p1)
        pairs_p2=getConnectedTwo (p2)
        triples_p1=getConnectedThree (pairs_p1)
        triples_p2=getConnectedThree (pairs_p2)
        win_p1=getConnectedFour (triples_p1)
        win_p2=getConnectedFour (triples_p2)
getSituationQuality p1 p2 n = if (mod n 2)==0 then
                              ( (length pairs_p1)*2 + (length triples_p1)*10 + win_p1*500
                             -((length pairs_p2)*2 + (length triples_p2)*10 + win_p2*500))
                             + (sum (map (\x->(getSituationQuality p1 (x:p2) (pred n))) (getPossibleMoves (concat [p1,p2]))))
                              else
                              ( (length pairs_p1)*2 + (length triples_p1)*10 + win_p1*5000
                             -((length pairs_p2)*2 + (length triples_p2)*10 + win_p2*5000))
                             + (sum (map (\x->(getSituationQuality (x:p1) p2 (pred n))) (getPossibleMoves (concat [p1,p2]))))
  where pairs_p1=getConnectedTwo (p1)
        pairs_p2=getConnectedTwo (p2)
        triples_p1=getConnectedThree (pairs_p1)
        triples_p2=getConnectedThree (pairs_p2)
        win_p1=getConnectedFour (triples_p1)
        win_p2=getConnectedFour (triples_p2)


getBestMove :: [(Move,Int)] -> Move
getBestMove (pos:poss) = fst(tmp (poss) (pos))
  where
    tmp [] quality = quality
    tmp (x:xs) y =  tmp (xs) (if (snd y) < ((snd x)+pref) then x else y)
      where
        pref=(abs(middle-(fst (fst y)))*1)


doMove :: GameState -> GameState
doMove (p1,p2) = (((getBestMove(map (\x->(x, getSituationQuality (x:p1) p2 difficulty)) (getPossibleMoves (concat [p1,p2])))):p1),p2)


test :: GameState -> [(Move, Int -> Int)]
test (p1,p2) = map (\x->(x, getSituationQuality (x:p1) p2)) (getPossibleMoves (concat [p1,p2]))


myturn :: GameState -> Int -> IO ()
myturn gameState columnMove = putStr . drawMyturn $ newGameState
  where newGameState = getNextGameState gameState columnMove



drawGameState :: (PlayerMoves, PlayerMoves) -> String
drawGameState state = 
    drawBoard (putP1onBoard (snd state) (putP2onBoard (fst state) blankBoard))


getNextGameState :: GameState -> Int -> Either String GameState
getNextGameState (p1, p2) columnMove  = 
    if row > 0 then Right (doMove (p1,((columnMove,row):p2)))
    else Left "There be nothin' left!!" --error
  where row = (checkAvailability columnMove (concat [p1,p2]))


drawMyturn :: Either String GameState  -> String
drawMyturn gameState = case gameState of
    Left lState -> lState
    Right rState -> drawGameState rState ++ msg rState ++ "\n"
  where
    msg rState = if (win (fst rState)) then "Ye 'ave Lost!"
                 else if (win (snd rState)) then "Ye seem to 'ave won..."
                 else "It be yer turn"


win :: PlayerMoves -> Bool
win p = (getConnectedFour (getConnectedThree (getConnectedTwo p)))>0

putP1onBoard :: PlayerMoves -> Board -> Board
putP1onBoard [] board       = board
putP1onBoard (p1:p1s) board = insertPiece p1 p1symbol (putP1onBoard p1s board)


putP2onBoard :: PlayerMoves -> Board -> Board
putP2onBoard [] board       = board
putP2onBoard (p2:p2s) board = insertPiece p2 p2symbol (putP2onBoard p2s board)

--Print Functions:
insertPiece :: Move -> Char -> Board -> Board
insertPiece (column,row) char board = (take (pred row2) board) ++
                                    [(take (pred column) (board!!pred row2)) ++ [char] ++ (drop (column) (board!!pred row2))] ++
                                    (drop (row2) board)
  where row2=succ(rows-row)




printBoard :: Board -> IO ()
printBoard = putStr . drawBoard

drawBoard :: Board -> String
drawBoard [] = " "
drawBoard (xs:xss) = do
    drawLine xs ++ dividerstr ++ "\n" ++ drawBoard xss
  where dividerstr = concat [(if (mod c 2)==0 then "+" else "---")|c<-[0..(2*length xs)]]


drawLine :: String -> String
drawLine xs = "| " ++ str ++ "\n"
  where str = concat [(c:" | ")|c<-xs]

drawSpacingLine :: String -> String
drawSpacingLine xs = " " ++ str ++ "\n"
  where str = concat [(c:"  ")|c<-xs]


printLine :: String -> IO ()
printLine xs=putStrLn ("| "++str)
  where str=concat [(c:" | ")|c<-xs]



