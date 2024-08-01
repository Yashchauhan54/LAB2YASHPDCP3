
type YashGenre = 
    | Horror
    | Drama
    | Thriller
    | Comedy
    | Fantasy
    | Sport

type MyDirector = {
    Name: string
    Movies: int
}

type Movie = {
    Name: string
    RunLength: int
    Genre: YashGenre
    Director: MyDirector
    IMDBRating: float
}

let movies = [
    { Name = "CODA"; RunLength = 111; Genre = Drama; Director = { Name = "Sian Heder"; Movies = 9 }; IMDBRating = 8.1 }
    { Name = "Belfast"; RunLength = 98; Genre = Comedy; Director = { Name = "Kenneth Branagh"; Movies = 23 }; IMDBRating = 7.3 }
    { Name = "Don’t Look Up"; RunLength = 138; Genre = Comedy; Director = { Name = "Adam McKay"; Movies = 27 }; IMDBRating = 7.2 }
    { Name = "Drive My Car"; RunLength = 179; Genre = Drama; Director = { Name = "Rysuke Hamaguchi"; Movies = 16 }; IMDBRating = 7.6 }
    { Name = "Dune"; RunLength = 155; Genre = Fantasy; Director = { Name = "Denis Villeneuve"; Movies = 24 }; IMDBRating = 8.1 }
    { Name = "King Richard"; RunLength = 144; Genre = Sport; Director = { Name = "Reinaldo Marcus Green"; Movies = 15 }; IMDBRating = 7.5 }
    { Name = "Licorice Pizza"; RunLength = 133; Genre = Comedy; Director = { Name = "Paul Thomas Anderson"; Movies = 49 }; IMDBRating = 7.4 }
    { Name = "Nightmare Alley"; RunLength = 150; Genre = Thriller; Director = { Name = "Guillermo Del Toro"; Movies = 22 }; IMDBRating = 7.1 }
]

let MyYashOscarWinners = 
    movies 
    |> List.filter (fun movie -> movie.IMDBRating > 7.4)

printfn "RESULT OF TEST 2 BY YASH CHAUHAN"
printfn "Probable Oscar Winners:"
MyYashOscarWinners |> List.iter (fun movie -> printfn "Title: %s, Rating: %.1f" movie.Name movie.IMDBRating)

let TimeDurationinhourandmin = 
    movies 
    |> List.map (fun movie -> 
        let hours = movie.RunLength / 60
        let minutes = movie.RunLength % 60
        sprintf "%s: %dh %dmin" movie.Name hours minutes
    )

printfn "Time Duration of Movies in Hours and Minutes:"
TimeDurationinhourandmin |> List.iter (fun duration -> printfn "%s" duration)
