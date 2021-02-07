type Format<'r,'a> = Format of ((string -> 'r) -> 'a)
// A formatter. This type holds all the information we need to create a formatting function, wrapped up in a way that makes it easy to compose.

// Build one of these up with primitives like s, string and int, join them together with <>, and when you're done, generate the final printing function with print.

// Example
// import Formatting exposing (..)

// let greeting =
//     s "Hello " <> string <> s "!"
// String -> Format a a
let s (v: string) =
   Format (fun r -> sprintf "%s" (r v))
// print : Format String a -> a
let print (f:Format<string,'a>): 'a = 
   let innerFn s =
      let (Format fn) = f
      fn s
   innerFn (fun (t:string) -> t)




let greeting = s "Hello"

let t = print  greeting

 //   C              A     K
// ((string -> 'b) -> 'a) ((string -> 'c) -> 'b) ((string -> 'c) -> 'a) 
// Format b a -> Format c b -> Format c a
let compose (f1: Format<'b,'a>) (f2: Format<'c,'b>) =
   let (Format fn1) = f1
   let (Format fn2) = f2
   fun r -> 
      
      let b = fn r
      b
   
   // let f s c =
   //    fun r1 -> 
   //       let innerFn r1 =
   //          fn1 r1
   //          let b = r1 s c

 // string -> ((string -> 'c) -> 'b)  -> 'a

// let s r =
