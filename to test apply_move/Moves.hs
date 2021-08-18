module Moves where 

 -- Name: Navjeet Hundal
 -- ID: 30004202
 -- Tutorial number: T05
 -- I used the the code the prof put on this website 

import GameLogic


 -- finding all possible simple moves
simple_moves:: GameState -> [Move]
simple_moves st
                | _status st == Red
                    = (simpleKing (_redKings st))++(simplePiece (_redPieces st))
                | _status st == Black
                    = (simpleKing (_blackKings st))++ (simplePiece (_blackPieces st))
                | otherwise = []  
 where
    dir = case(_status st) of {Red -> -1;Black -> 1}
    simpleKing xs = [[(x,y),(x',y')]
                    | (x,y) <- xs
                    , (x',y') <- [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]
                    , notoccupied'(x',y') st && onboard(x',y')]
    simplePiece xs = 
                      [ [(x,y),(x',y')]
                      | (x,y) <- xs
                      , (x',y') <- let y'=y+dir in [(x+1,y'),(x-1,y')]
                      , notoccupied' (x',y') st && onboard (x',y')]
notoccupied :: (Int, Int) -> [(Int,Int)] -> Bool
notoccupied (a,b) [] = True
notoccupied (a,b) ((x,y):zs)
    | (a == x) && (b == y) = False
    | otherwise = notoccupied (a,b) zs
 -- checking if the move is on the board
onboard :: (Int,Int) -> Bool
onboard (x,y) 
    | ((x >= 0 && x <= 7) && (y >= 0 && y <= 7)) = True
    | otherwise = False
 -- the moves function which calls simple move if the user can make no jump moves
moves :: GameState -> [Move]
moves st 
    | jumpmoves /= [] = jumpmoves
    | otherwise = simplemoves
    where
        simplemoves = simple_moves st
        jumpmoves = jump_moves st
 -- finds all the possible jump moves
jump_moves :: GameState -> [Move]
jump_moves st
                | _status st == Red
                    = (jumpKing (_redKings st))++(jumpPiece (_redPieces st))
                | _status st == Black
                    = (jumpKing (_blackKings st))++ (jumpPiece (_blackPieces st))
                | otherwise = [] 
 where
    dir' = case(_status st) of {Red -> -1;Black -> 1}
    dir'' = case(_status st) of {Red -> -2;Black -> 2}
    jumpKing xs = [(x,y):ys | (x,y) <- xs, ys <- jumpKing' xs (x,y) [] (x,y)]
    jumpKing' xs start rem (x,y) = 
                   [(x'',y''):ys 
                   | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                   , notElem(x',y') rem && opponent_occupied(x',y') st && ((start==(x'',y'')) || notoccupied' (x'',y'') st) && onboard (x'',y'')
                   , ys <- jump_over (jumpKing' xs start ((x',y'):rem) (x'',y''))]

    jumpPiece xs = [(x,y):ys | (x,y) <- xs, ys <- jumpPiece' xs (x,y) [] (x,y)]
    jumpPiece' xs start rem (x,y) = case (isKing(x,y) st) of 
        (True) -> jumpKing' xs start rem (x,y)
        (False) -> [ (x'',y''):ys 
                   | ((x',y'),(x'',y'')) <- 
                        let y'=y+dir' 
                            y''=y+dir''  
                        in [((x+1,y'),(x+2,y'')),((x-1,y'),(x-2,y''))]
                   , notElem(x',y') rem && opponent_occupied (x',y') st && ((start==(x'',y'')) || notoccupied' (x'',y'') st) && onboard (x'',y'')
                   , ys <- jump_over (jumpPiece' xs start ((x',y'):rem) (x'',y'')) ]

  
    jump_over [] = [[]]
    jump_over z = z
 -- checks if the next move would have the pawn become a king
isKing :: (Int,Int) -> GameState -> Bool
isKing (a,b) st 
    | (_status st) == Red && b == 0 = True
    | (_status st) == Black && b == 7 = True
    | otherwise = False
        
 -- checks if there is an opponent 
opponent_occupied :: (Int,Int) -> GameState -> Bool
opponent_occupied (a,b) st 
    | _status st == Red = elem(a,b) ((_blackKings st) ++ (_blackPieces st))
    | otherwise = elem(a,b) ((_redKings st) ++ (_redPieces st))
 -- checks if the coord is empty
notoccupied' :: (Int,Int) -> GameState -> Bool
notoccupied' (a,b) st = notElem(a,b) ((_blackKings st) ++ (_blackPieces st) ++ (_redKings st) ++ (_redPieces st))

 -- function to apply the move 
apply_move :: Move -> GameState -> GameState
apply_move mv st 
                | (((_redKings st) ++ (_redPieces st)) == []) || (((_blackKings st) ++ (_blackPieces st)) == []) = st{_status = GameOver}
                | elem mv (moves st) == True = case(isJumpMove st) of {True -> (make_jump_move mv st){_status = case (_status st) of Red -> Black ;Black-> Red} ;False -> make_simple_move mv st} 
                | otherwise = st{_message = "Illegal move!!"}
 where


    isJumpMove st 
        | (jump_moves st) /= [] = True
        | otherwise = False
    -- make simple moves
    make_simple_move [start,end] st
        | _status st == Red && elem start (_redKings st)
               = st{_redKings = replace start end (_redKings st)
                     ,_status = change_player st
                     ,_message = ""} 
        | _status st == Black && elem start (_blackKings st)
               = st{_blackKings = replace start end (_blackKings st)
                     ,_status = change_player st
                     ,_message = ""} 
        | _status st == Red && elem start (_redPieces st)
               = case(isKing end st) of 
                (True) -> st{_redKings = [end] ++(_redKings st),_redPieces = remove start (_redPieces st),_status = change_player st,_message = ""} 
                (False) -> st{_redPieces = replace start end (_redPieces st),_status = change_player st,_message = ""}
                     
        | _status st == Black && elem start (_blackPieces st)
               = case(isKing end st) of 
                (True) -> st{_blackKings = [end] ++(_blackKings st),_blackPieces = remove start (_blackPieces st),_status = change_player st,_message = ""} 
                (False) -> st{_blackPieces = replace start end (_blackPieces st),_status = change_player st,_message = ""}

    make_jump_move (x:[]) st = st
    make_jump_move (start:(next:rest)) st
        | _status st == Red && elem start (_redKings st)
                = make_jump_move (next:rest)
                            (st{_blackKings = remove (jumped start next) (_blackKings st)
                               ,_blackPieces = remove (jumped start next) (_blackPieces st)
                               ,_redKings = next:(remove start (_redKings st))
                               ,_message = ""})
        | _status st == Black && elem start (_blackKings st)
                = make_jump_move (next:rest)
                            (st{_redKings = remove (jumped start next) (_redKings st)
                               ,_redPieces = remove (jumped start next) (_redPieces st)
                               ,_blackKings = next:(remove start (_blackKings st))
                               ,_message = ""})
        | _status st == Red && elem start (_redPieces st)
                = case(isKing next st) of
                 (True) -> make_jump_move (next:rest) (st{_blackPieces = remove (jumped start next) (_blackPieces st),_blackKings = remove (jumped start next) (_blackKings st),_redKings = [next]++(_redKings st),_redPieces = remove start (_redPieces st),_message = "done5"})
                 (False) -> make_jump_move (next:rest) (st{_blackPieces = remove (jumped start next) (_blackPieces st),_blackKings = remove (jumped start next) (_blackKings st),_redPieces = next:(remove start (_redPieces st)),_message =""})
                
        | _status st == Black && elem start (_blackPieces st)
                = case(isKing next st) of
                 (True) -> make_jump_move (next:rest) (st{_redPieces = remove (jumped start next) (_redPieces st),_redKings = remove (jumped start next) (_redKings st),_blackKings = [next]++(_blackKings st),_blackPieces = remove start (_blackPieces st),_message = "done5"})
                 (False) -> make_jump_move (next:rest) (st{_redPieces = remove (jumped start next) (_redPieces st),_redKings = remove (jumped start next) (_redKings st),_blackPieces = next:(remove start (_blackPieces st)),_message =""})
        
    

    
   
     

    replace start end [] = []
    replace start end (a:as)
        | a == start = end:as
        | otherwise = a:(replace start end as)

    change_player st
        | (_status st) == Red = Black
        | otherwise = Red
remove :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -- used the function from https://stackoverflow.com/questions/10050988/haskell-removes-all-occurrences-of-a-given-value-from-within-a-list-of-lists
remove (x,y) xs = filter(\e -> e /=(x,y)) xs

jumped :: (Int,Int) -> (Int,Int) -> (Int,Int) -- check which piece the checker jumped over
jumped (x,y) (x',y') = (div(x+x') 2,div(y+y') 2)

















