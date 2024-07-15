open System.IO

type CommandLineInstruction<'a> = HelloWorld of (unit -> 'a) 

type CommandLineProgram<'a> =
    | Pure of 'a 
    | Free of CommandLineInstruction<CommandLineProgram<'a>>

let private mapI f =  function  HelloWorld cont -> HelloWorld (fun () -> f (cont ()))   

let rec private bind f = 
    function
    | Free instr -> mapI (bind f) instr |> Free
    | Pure x -> f x

type CommandLineProgramBuilder() =
    member _.Bind(p, f) = bind f p
    member _.Return x = Pure x
    member _.ReturnFrom p = p

let private cmdBuilder = CommandLineProgramBuilder()
       
let rec interpret clp =
    match clp with 
    | Pure x                 -> 
                              x
    | Free (HelloWorld cont) ->
                              printfn "Hello World!"
                              interpret (cont ())

cmdBuilder { return! Free (HelloWorld (fun () -> Pure ())) } |> interpret
