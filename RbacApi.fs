module RbacApi

// ================================
// Public types used in the API
// ================================

open TicTacToeCore


type TicTacToeRequest = {
    player: Player
    row: Row
    col: Column
    }

type MoveResult = 
    | KeepPlaying 
    | GameWon of Player
    | GameTied

type TicTacToeResponse = {
    result : MoveResult
    display: DisplayInfo 
    }

and DisplayInfo = string

// ================================
// API implementation
// ================================

type API() =

    // uses mutable state
    let mutable gameState  = Implementation.initialGameState

    // player X or O makes a move
    member this.Move (request:TicTacToeRequest) = 
        let cellPos = request.col, request.row   
        let player = request.player
        let newCell = {pos = cellPos; state = Played player}
        let newGameState = gameState |> Implementation.updateCell newCell 
        
        let displayStr = Implementation.displayStr newGameState 

        if newGameState |> Implementation.isGameWonBy player then
            // return the final result
            {result = GameWon player; display = displayStr}
        elif newGameState |> Implementation.isGameTied then
            // return the final result
            {result = GameTied; display = displayStr}
        else
            // update mutable gamestate
            gameState <- newGameState
            // keep playing
            {result = KeepPlaying; display = displayStr}


        // a subtle bug is that the mutable gameState is NOT updated for GameWon or GameTied !