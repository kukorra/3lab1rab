open System

let maxchislo n =
    let rec findMax maxSoFar num =
        if num = 0 then maxSoFar
        else findMax (max maxSoFar (num % 10)) (num / 10)
    findMax 0 n

let getMaxDigits numbers = 
    numbers |> Seq.map maxchislo

let generateRandomNumbers count =
    let rnd = Random()
    Seq.init count (fun _ -> rnd.Next(1, 10000))

let rec readNumbers () =
    printf "Введите число (или пустую строку для завершения): "
    match Console.ReadLine() with
    | "" -> Seq.empty
    | input ->
        match Int32.TryParse(input) with
        | true, n when n > 0 -> Seq.append (seq { yield n }) (readNumbers ())
        | _ ->
            printfn "Некорректный ввод. Попробуйте снова."
            readNumbers ()

printf "Выберите способ ввода (1 - случайные числа, 2 - ввод вручную): "
match Console.ReadLine() with
| "1" ->
    let numbers = generateRandomNumbers 5
    printfn "Сгенерированные числа: %A" (numbers |> Seq.toList)
    printfn "Максимальные цифры: %A" (getMaxDigits numbers |> Seq.toList)
| "2" ->
    let numbers = readNumbers ()
    printfn "Введенные числа: %A" (numbers |> Seq.toList)
    printfn "Максимальные цифры: %A" (getMaxDigits numbers |> Seq.toList)
| _ ->
    printfn "Некорректный выбор."