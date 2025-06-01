open System.IO

type private CommandLineInstruction<'a> = HelloWorld of (unit -> 'a)

type private CommandLineProgram<'a> =
    | Pure of 'a
    | Free of CommandLineInstruction<CommandLineProgram<'a>>

let private mapI f =
    function
    | HelloWorld cont 
        -> HelloWorld <| fun () -> cont >> f <| ()

//bind doesn't need to be tail-recursive or CPS, because it builds structure, not executes it
let rec private bind f = 
    function
    | Free instr -> mapI (bind f) instr |> Free
    | Pure x     -> f x

type private CommandLineProgramBuilder = CommandLineProgramBuilder with
    member _.Bind(p, f) = bind f p
    member _.Return x = Pure x
    member _.ReturnFrom p = p

let private cmdBuilder = CommandLineProgramBuilder

let rec private interpretCPS (cont: 'a -> 'b) (clp: CommandLineProgram<'a>) : 'b =
    match clp with
    | Pure x 
        ->
        cont x
    | Free (HelloWorld next) 
        ->
        printfn "Hello World!"
        interpretCPS <| cont <| next ()

cmdBuilder { return! Free (HelloWorld (fun () -> Pure ())) } |> interpretCPS id