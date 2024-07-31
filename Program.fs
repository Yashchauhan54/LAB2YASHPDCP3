//Record part 1

type myCoach = {
    cName: string
    FormerPlayer: bool
}

type myStatistics = {
    Wins: int
    Losses: int
}

type Team = {
    clubName: string
    Coach: myCoach
    Stats: myStatistics
}

let teams = [
    { clubName = "Boston Celtics"; Coach = { cName = "Joe Mazzulla"; FormerPlayer = true }; Stats = { Wins = 3634; Losses = 2480 } }
    { clubName = "Golden State Warriors"; Coach = { cName = "Steve Kerr"; FormerPlayer = true }; Stats = { Wins = 2969; Losses = 3134 } }
    { clubName = "Houston Rockets"; Coach = { cName = "Ime Udoko"; FormerPlayer = true }; Stats = { Wins = 2369; Losses = 2237 } }
    { clubName = "Los Angeles Lakers"; Coach = { cName = "JJ Redick"; FormerPlayer = false }; Stats = { Wins = 3550; Losses = 2454 } }
    { clubName = "Chicago Bulls"; Coach = { cName = "Billy Donovan"; FormerPlayer = true }; Stats = { Wins = 2383; Losses = 2297 } }
]

let successfulTeams = 
    teams 
    |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)

printfn "RESULTS OF PART 1 RECORD"
printfn "Successful Teams:"

successfulTeams |> List.iter (fun team -> printfn "Club: %s, Coach: %s" team.clubName team.Coach.cName)


let successPercentages = 
    teams 
    |> List.map (fun team ->
        let wins = float team.Stats.Wins
        let losses = float team.Stats.Losses
        let percentage = (wins / (wins + losses)) * 100.0
        (team.clubName, percentage)  
    )

printfn "winning percentage of each teams:"
successPercentages |> List.iter (fun (name, percentage) -> printfn "Team: %s, Success Percentage: %.2f%%" name percentage)








//TASK 2 Discriminated union

type CuisineType = 
    | KoreanStyle
    | TurkishStyle

type FilmExperience = 
    | Standard
    | LargeScreen
    | MotionSeat
    | StandardWithSnacks
    | LargeScreenWithSnacks
    | MotionSeatWithSnacks

type Plan =
    | PlayBoardGame
    | Relax
    | WatchMovie of FilmExperience
    | DineAt of CuisineType
    | DriveLongDistance of int * float

let computeExpense (plan: Plan) : float =
    match plan with
    | PlayBoardGame -> 0.0
    | Relax -> 0.0
    | WatchMovie filmType ->
        match filmType with
        | Standard -> 12.0
        | LargeScreen -> 17.0
        | MotionSeat -> 20.0
        | StandardWithSnacks -> 17.0
        | LargeScreenWithSnacks -> 22.0
        | MotionSeatWithSnacks -> 25.0
    | DineAt cuisineType ->
        match cuisineType with
        | KoreanStyle -> 70.0
        | TurkishStyle -> 65.0
    | DriveLongDistance (miles, costPerMile) ->
        float miles * costPerMile

let plans = [
    PlayBoardGame
    Relax
    WatchMovie MotionSeat
    DineAt KoreanStyle
    DriveLongDistance (150, 0.2)
]
printfn "RESULTS OF PART 2 DISCRIMINATED UNION"
plans |> List.iter (fun plan ->
    let expense = computeExpense plan
    match plan with
    | PlayBoardGame -> printfn "Expense for PlayBoardGame: %.2f CAD" expense
    | Relax -> printfn "Expense for Relax: %.2f CAD" expense
    | WatchMovie filmType -> printfn "Expense for WatchMovie (%A): %.2f CAD" filmType expense
    | DineAt cuisineType -> printfn "Expense for DineAt (%A): %.2f CAD" cuisineType expense
    | DriveLongDistance (miles, _) -> printfn "Expense for DriveLongDistance (%d miles): %.2f CAD" miles expense
)

let totalExpense = 
    plans 
    |> List.fold (fun acc plan -> 
        acc + computeExpense plan
    ) 0.0

printfn "Total Expense for my Valentine's day plan is: %.2f CAD" totalExpense




