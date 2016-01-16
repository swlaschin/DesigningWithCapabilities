module TicTacToeCore

// ================================
// Domain types
// ================================

type Player = PlayerX | PlayerO
type Row = Top | Middle | Bottom
type Column = Left | Center | Right
type CellPosition = Column * Row 

type CellState = 
    | Played of Player 
    | Empty

type Cell = {
    pos : CellPosition 
    state : CellState 
    }

// ================================
// Implementation of TicTacToe logic
// ================================

module Implementation =

    /// private implementation of game state
    type GameState = {
        cells : Cell list
        }

    /// the list of all horizontal positions
    let allHorizPositions = [Left; Center; Right]
    
    /// the list of all horizontal positions
    let allVertPositions = [Top; Middle; Bottom]

    /// A type to store the list of cell positions in a line
    type Line = Line of CellPosition list

    /// a list of the eight lines to check for 3 in a row
    let linesToCheck = 
        let mkHLine v = Line [for h in allHorizPositions do yield (h,v)]
        let hLines= [for v in allVertPositions do yield mkHLine v] 

        let mkVLine h = Line [for v in allVertPositions do yield (h,v)]
        let vLines = [for h in allHorizPositions do yield mkVLine h] 

        let diagonalLine1 = Line [Left,Top; Center,Middle; Right,Bottom]
        let diagonalLine2 = Line [Left,Bottom; Center,Middle; Right,Top]

        // return all the lines to check
        [
        yield! hLines
        yield! vLines
        yield diagonalLine1 
        yield diagonalLine2 
        ]

    /// update a particular cell in the GameState 
    /// and return a new GameState
    let updateCell newCell gameState =

        // create a helper function
        let substituteNewCell oldCell =
            if oldCell.pos = newCell.pos then
                newCell
            else 
                oldCell                 

        // get a copy of the cells, with the new cell swapped in
        let newCells = gameState.cells |> List.map substituteNewCell 
        
        // return a new game state with the new cells
        {gameState with cells = newCells }


    /// get the cell corresponding to the cell position
    let getCell gameState posToFind = 
        gameState.cells 
        |> List.find (fun cell -> cell.pos = posToFind)


    // return the other player    
    let otherPlayer player = 
        match player with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX


    /// Return true if the game was won by the specified player
    let isGameWonBy player gameState = 
        
        // helper to check if a cell was played by a particular player
        let cellWasPlayedBy playerToCompare cell = 
            match cell.state with
            | Played player -> player = playerToCompare
            | Empty -> false

        // helper to see if every cell in the Line has been played by the same player
        let lineIsAllSamePlayer player (Line cellPosList) = 
            cellPosList 
            |> List.map (getCell gameState)
            |> List.forall (cellWasPlayedBy player)

        linesToCheck
        |> List.exists (lineIsAllSamePlayer player)


    /// Return true if all cells have been played
    let isGameTied gameState = 
        // helper to check if a cell was played by any player
        let cellWasPlayed cell = 
            match cell.state with
            | Played _ -> true
            | Empty -> false

        gameState.cells
        |> List.forall cellWasPlayed 

    /// determine the remaining moves 
    let remainingMoves gameState = 

        // helper to return Some if a cell is playable
        let playableCell cell = 
            match cell.state with
            | Played player -> None
            | Empty -> Some cell.pos

        gameState.cells
        |> List.choose playableCell

    // allPositions is the cross-product of the positions
    let allPositions = [
        for h in allHorizPositions do 
        for v in allVertPositions do 
            yield (h,v)
        ]

    // all cells are empty initially
    let emptyCells = 
        allPositions 
        |> List.map (fun pos -> {pos = pos; state = Empty})

    let initialGameState = { cells=emptyCells }         


    /// return the gamestate as a display string like this:
    ///  O | X | - 
    ///  - | - | X
    ///  O | O | X
    let displayStr gameState = 
        let cells = gameState.cells
        let cellToStr cell = 
            match cell.state with
            | Empty -> "-"            
            | Played player ->
                match player with
                | PlayerO -> "O"
                | PlayerX -> "X"

        let printCells cells  = 
            cells
            |> List.map cellToStr
            |> List.reduce (fun s1 s2 -> s1 + "|" + s2) 
            |> sprintf "|%s|\n"

        let topCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Top) 
        let centerCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Middle) 
        let bottomCells = 
            cells |> List.filter (fun cell -> snd cell.pos = Bottom) 
        
        "\n" + // add some space
        printCells topCells +
        printCells centerCells +
        printCells bottomCells +
        "\n"   // add some space