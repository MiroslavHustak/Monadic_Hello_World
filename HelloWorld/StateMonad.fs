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
        let increment = State(fun s -> (s + 1, s + 1))
  
        // Composite computation using the state monad
        let counterExample =
            state
                {
                    let! a = increment      // 0 -> 1
                    let! b = increment      // 1 -> 2
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