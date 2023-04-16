open System

let rec getCard (deck: int list) (sum: int) : int list * int =
    match deck with
    | [] -> failwith "Deck is empty"
    | card :: rest ->
        let newSum = sum + card

        if newSum > 21 then
            if card = 11 then getCard rest (sum + 1) else (rest, sum)
        else
            (rest, newSum)

let rec playerTurn (deck: int list) (sum: int) : int list * int =
    printfn "Your current total: %d" sum
    printfn "Do you want another card? (y/n)"
    let choice = Console.ReadLine()

    match choice with
    | "y" ->
        let newDeck, newSum = getCard deck sum
        playerTurn newDeck newSum
    | "n" -> (deck, sum)
    | _ ->
        printfn "Invalid choice. Please enter y or n."
        playerTurn deck sum

let rec dealerTurn (deck: int list) (sum: int) : int list * int =
    if sum < 17 then
        let newDeck, newSum = getCard deck sum
        dealerTurn newDeck newSum
    else
        (deck, sum)

let dealCards (deck: int list) : int list * int * int =
    let newDeck, playerSum = getCard deck 0
    let newerDeck, dealerSum = getCard newDeck 0
    (newerDeck, playerSum, dealerSum)

let rec gameLoop (deck: int list) (playerSum: int) (dealerSum: int) =
    if playerSum > 21 then
        printfn "You busted. You lose!"
    else if dealerSum > 21 then
        printfn "Dealer busted. You win!"
    else if dealerSum >= 17 && playerSum >= dealerSum then
        printfn "You win!"
    else
        let newDeck, newPlayerSum = playerTurn deck playerSum
        let newerDeck, newDealerSum = dealerTurn newDeck dealerSum
        gameLoop newerDeck newPlayerSum newDealerSum

let shuffle (deck: int list) =
    let rnd = System.Random()
    List.sortBy (fun _ -> rnd.Next())

let deck =
    let rnd = System.Random()

    [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 10; 10; 10; 11 ]
    |> List.replicate 4
    |> List.concat
    |> List.sortBy (fun _ -> rnd.Next())

let initialDeck, playerSum, dealerSum = dealCards deck

printfn "Your initial cards: %d, %d" playerSum (List.head (List.tail initialDeck))
printfn "Dealer's initial card: %d" (List.head initialDeck)

gameLoop (List.tail (List.tail initialDeck)) playerSum dealerSum
