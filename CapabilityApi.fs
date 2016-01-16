module CapabilityApi

open TicTacToeCore

// ================================
// Public types used in the API
// ================================

/// The capability to make a move at a particular location.
/// The gamestate, player and position are already "baked" into the function.
type MoveCapability = 
    unit -> TicTacToeResponse 

/// A capability along with the position the capability is associated with.
/// This allows the UI to show information so that the user
/// can pick a particular capability to exercise.
and NextMoveInfo = {
    // the pos is for UI information only
    // the actual pos is baked into the cap.
    playerToPlay : Player
    posToPlay : CellPosition 
    capability : MoveCapability }

and TicTacToeResponse = 
    | KeepPlaying of DisplayInfo * NextMoveInfo list 
    | GameWon of DisplayInfo * Player 
    | GameTied of DisplayInfo

and DisplayInfo = string


// ================================
// API implementation
// ================================

// return the move result case for a player 
let toKeepPlaying displayInfo nextMoves = 
    KeepPlaying (displayInfo, nextMoves)

// given a function, a player & a gameState & a position,
// create a NextMoveInfo with the capability to call the function
let makeNextMoveInfo f player gameState cellPos =
    // the capability has the player & cellPos & gameState baked in
    let capability() = f player cellPos gameState 
    {playerToPlay=player; posToPlay=cellPos; capability=capability}

// given a function, a player & a gameState & a list of positions,
// create a list of NextMoveInfos wrapped in a MoveResult
let makeKeepPlayingResponse f player gameState cellPosList =
    let displayInfo = Implementation.displayStr gameState 
    cellPosList
    |> List.map (makeNextMoveInfo f player gameState)
    |> toKeepPlaying displayInfo

// player X or O makes a move
let rec playerMove player cellPos gameState  = 
    let newCell = {pos = cellPos; state = Played player}
    let newGameState = gameState |> Implementation.updateCell newCell 
    let displayInfo = Implementation.displayStr newGameState 

    if newGameState |> Implementation.isGameWonBy player then
        // return the move result
        GameWon (displayInfo, player) 
    elif newGameState |> Implementation.isGameTied then
        // return the move result
        GameTied displayInfo 
    else
        let otherPlayer = Implementation.otherPlayer player 
        let moveResult = 
            newGameState 
            |> Implementation.remainingMoves
            |> makeKeepPlayingResponse playerMove otherPlayer newGameState
        moveResult 

/// Local API
type API() =

    // note - no mutable state!

    member this.NewGame() = 
        let gameState = Implementation.initialGameState

        // initial of valid moves for player X is all positions
        let moveResult = 
            Implementation.allPositions 
            |> makeKeepPlayingResponse playerMove PlayerX gameState

        // return new game
        moveResult 

// ================================
// Web API implementation
// ================================


/// Web-based (Html) API, which converts the MoveResult ouput to HTML
/// and stores the capabilities in a dictionary, keyed by a GUID.
type WebAPI() =

    let dict = System.Collections.Generic.Dictionary<_,_>()

    /// Given a capability, return a "token" (a GUID) for it
    let makeCapToken (cap:MoveCapability) =
        let guid = System.Guid.NewGuid().ToString()
        dict.Add(guid,cap)
        guid

    /// Given a token, find the corresponding capability
    let getCapFromToken guid =
        match dict.TryGetValue(guid) with
        | true, cap -> 
            dict.Remove(guid) |> ignore // remove so it can only be played once
            cap
        | false,_ -> 
            failwithf "Capability no longer available for Token '%s'" guid

    /// Convert an available move to HTML
    let availableMoveHtml availableMove =
        let displayStr = sprintf "%A" availableMove.posToPlay
        let capToken = makeCapToken availableMove.capability 
        sprintf "<a href='/move/%s'>Play %s</a>" capToken displayStr 

    /// Convert a list of available move to HTML
    let availableMovesHtml availableMoves =
        availableMoves 
        |> List.map availableMoveHtml 
        |> String.concat "\n"

    /// Convert the useful part of the a TicTacToeResponse to HTML
    let gameStateHtml response =
        match response with
        | KeepPlaying (disp,availableMoves) ->
            let movesHtml = availableMovesHtml availableMoves   
            sprintf "<h3>Keep Playing</h3>\n<pre>%s</pre>\n%s" disp movesHtml 
        | GameWon (disp,player) -> 
            sprintf "<h3>Game Won by: %A</h3>\n<pre>%s</pre>" player disp
        | GameTied disp ->
            sprintf "<h3>Game Tied</h3>\n<pre>%s</pre>" disp

    /// Convert an entire TicTacToeResponse to HTML
    let toHtml (response:TicTacToeResponse) =
        System.Text.StringBuilder()
            .Append("<html>\n")
            .Append("<body>\n")
            .Append(gameStateHtml response) 
            .Append("</body>\n")
            .Append("</html>\n")
            .ToString()

    /// Respond to a "NewGame" request by returning all nine moves
    member this.NewGame() = 
        let gameState = Implementation.initialGameState

        // initial list of valid moves for player X is all nine positions
        let moveResult = 
            Implementation.allPositions 
            |> makeKeepPlayingResponse playerMove PlayerX gameState

        // return new game
        moveResult |> toHtml

    /// Respond to a "Play" request. The only input is a token representing the capability
    member this.Play(moveToken) = 
        let moveCap = getCapFromToken moveToken
        moveCap() |> toHtml

