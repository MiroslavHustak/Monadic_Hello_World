namespace StateMonad

open System
open System.Diagnostics

module StateMonad =   
  
    type Box<'T> = Box of 'T
  
    let x = Box 42    

    let (Box n1) = x    //nebo primo v parametru  let f (Box v) = v    
    printfn "%i" n1

    let n2 = x |> function Box v -> v
    printfn "%i" n2
      
    // STATE MONAD
    //******************************
    type State<'S, 'T> = State of ('S -> 'T * 'S)
  
    // Run a state computation
    let private runState (State f) initialState = f initialState
  
    // Return: Wraps a value into a State monad
    let private returnState x = State (fun s -> (x, s))
  
    // Bind: Chains stateful computations
    let private bindState (m: State<'S, 'T>) (f: 'T -> State<'S, 'U>) : State<'S, 'U> =
        State 
            (fun s 
                ->
                let (value, newState) = runState m s
                runState (f value) newState
            )
  
    type StateBuilder = StateBuilder with
        member _.Return(x) = returnState x
        member _.Bind(m, f) = bindState m f
        member _.ReturnFrom(m) = m
        member _.Zero() = returnState ()
  
    let state = StateBuilder

    //******************************

    let stateMonadExample () = 
        // ===== Example: Pure Counter =====
    
        // Increment counter by 1
        let increment = State(fun s -> (s, s + 1))
  
        // Composite computation using the state monad
        let counterExample =
            state
                {
                    let! _ = increment      // 0 -> 1
                    let! _ = increment      // 1 -> 2
                    let! c = increment      // 2 -> 3

                    return sprintf "Final count: %d" c
                }
  
        // Run with an initial state of 0
        let (result, finalState) = runState counterExample 0
  
        printfn "%s (state = %d)" result finalState

    let nonStateMonadExample () = 

        let increment s =
            let newValue = s + 1
            (newValue, newValue)
    
        let programNormal initialState =

            let (a, s1) = increment initialState
            let (b, s2) = increment s1
            let (c, s3) = increment s2
            sprintf "Final count: %d" c, s3
    
        // Run it
        let (resultNormal, finalStateNormal) = programNormal 0
        
        printfn "%s (nonState = %d)" resultNormal finalStateNormal

module Haskell_IO = 
   
    type RealWorld = RealWorldToken
    
    // The IO monad: a function that takes a RealWorld token and returns a new one plus a value
    type IO_Monad<'a> = IO_Monad of (RealWorld -> RealWorld * 'a)
    
    // Helper to unwrap (only internal use)
    let private runIO_helper (IO_Monad f) = f
    
    // Computation builder for nicer syntax
    type IOMonad() =

        member _.Return(x) : IO_Monad<'a> =
            IO_Monad (fun world -> world, x)
    
        member _.ReturnFrom(io: IO_Monad<'a>) = io
    
        member _.Bind(io: IO_Monad<'a>, binder: 'a -> IO_Monad<'b>) : IO_Monad<'b> =
            IO_Monad 
                (fun world 
                    ->
                    let world', a = runIO_helper io world
                    runIO_helper (binder a) world'
                )
    
        member _.Delay(f: unit -> IO_Monad<'a>) : IO_Monad<'a> =
            IO_Monad 
                (fun world
                    ->
                    let (IO_Monad delayed) = f()
                    delayed world
                )
    
        // For do! notation with sequencing (ignoring result)
        member this.Combine(io1: IO_Monad<unit>, io2: IO_Monad<'a>) : IO_Monad<'a> =
            this.Bind(io1, fun () -> io2)
        
        member _.Zero() : IO_Monad<unit> = 
            IO_Monad (fun world -> world, ())
    
    // Global builder instance
    let IO = IOMonad()
    
    // Primitive: actually perform a side effect and pass the world through
    let private primIO (action: unit -> 'a) : IO_Monad<'a> =
        IO_Monad
            (fun world 
                ->
                let result = action()
                world, result
            )  // world is just threaded through unchanged
    
    // Public primitives built on primIO
    let putStrLn (s: string) : IO_Monad<unit> =
        primIO (fun () -> printfn "%s" s)
    
    let putStr (s: string) : IO_Monad<unit> =
        primIO (fun () -> printf "%s" s)
    
    let getLine : IO_Monad<string> =
        primIO (fun () -> System.Console.ReadLine())
    
    // Example usage with the builder
    // Remove the () -> and call the IO actions directly
    let program : IO_Monad<unit> =
        IO 
            {
                do! putStrLn "text1"
                do! putStrLn "text2"
                do! putStrLn "Done!"
            }
    
    // To actually run an IO computation (only possible at the "edge" of the program)
    let runIO (IO_Monad f) =
        // We create a dummy token and discard it — the real sequencing is enforced by the type
        let dummyWorld = RealWorldToken
        let _, result = f dummyWorld
        result