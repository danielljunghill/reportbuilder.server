// type Format<'r,'a> = Format of ((string -> 'r) -> 'a)

// A formatter. This type holds all the information we need to create a formatting function, wrapped up in a way that makes it easy to compose.

// Build one of these up with primitives like s, string and int, join them together with <>, and when you're done, generate the final printing function with print.
type Members = Members of string List 

let rec cross (l1: 'a List List) (l2: 'a List)=
   l1 |> List.fold (fun state a -> state @ (l2 |> List.map (fun b -> [ b ] @ a))) []

let rec collecting state (f: 'b ->  'a List) (m: 'b List)  =
   match m with
      | [] -> state
      | head :: tail ->
         let newList = f head
         let currentState =
            if List.isEmpty state then
               newList |> List.map (fun a -> [a])
            else
               cross state newList
         collecting currentState f tail

let rec cross2 (l1: 'a List List) (l2: 'a List) =
   l1 |> List.fold (fun state a -> state @ (l2 |> List.map (fun b -> [ b ] @ a))) []


let m1 = Members ["kv1"; "kv2"; ]; 
let m2 = Members ["2020"; "2021"; ];
let m3 = Members ["Personbil"; "Lastbil"];

let ms = [ m1; m2 ; m3]
let f (Members ms) = ms

let total = collecting  [] f ms


let rec collecting2 state (f: 'b ->  'a List) (m: 'b List)  =
   match m with
      | [] -> []
      | head :: tail ->
         let newList = f head
         printfn "%A" newList
         let currentState =
            if List.isEmpty state then
               newList |> List.map (fun a -> [a])
            else
               cross state newList
         currentState @ collecting2 currentState f tail


let total2 = collecting2  [] f [ m1 ; m2; m3]
         
// let rec crossForDrill (parent: 'a List) (child: 'a List) =
//    match parent with
//    |[] -> []
//    | head :: tail -> 
     
      
// //headers
// let rec drill state (f: 'b -> 'a List) (m: 'b List) =
//   match m with
//   | [] -> state
//   | head :: tail -> 
//    let newList = f head
 
//    let foldList (l1: 'a List) 
//    let currentstate =
//       if List.isEmpty state then
//          newList // add index
//       else
//          crossForDrill state newList
   
//    drill currentstate f tail


// let vv = crossForDrill ["a"; "b"] ["c"; "d"]
   
// let (r: string List) = drill [] f ms
// r
// Examplekv2
// import Formatting exposing (..)

// let greeting =
//     s "Hello " <> string <> s "!"
// // String -> Format a a
// let s (v: string) =
//    Format (fun r -> sprintf "%s" (r v))
// // print : Format String a -> a
// let print (f:Format<string,'a>): 'a = 
//    let innerFn s =
//       let (Format fn) = f
//       fn s
//    innerFn (fun (t:string) -> t)




// let greeting = s "Hello"

// let t = print  greeting

//  //   C              A     K
// // ((string -> 'b) -> 'a) ((string -> 'c) -> 'b) ((string -> 'c) -> 'a) 
// // Format b a -> Format c b -> Format c a
// let compose (f1: Format<'b,'a>) (f2: Format<'c,'b>) =
//    let (Format fn1) = f1
//    let (Format fn2) = f2
//    fun r -> 
      
//       let b = fn r
//       b
   
//    // let f s c =
//    //    fun r1 -> 
   //       let innerFn r1 =
   //          fn1 r1
   //          let b = r1 s c

 // string -> ((string -> 'c) -> 'b)  -> 'a
// type Attribute<'a> = Attribute of 'a
// type Msg = Msg of string
// // let s r =
// type AttributeAdder<'a> = AttributeAdder of ((bool *(Msg Attribute) List) -> ('a-> bool * (Msg Attribute ) List))
// let runAdder select (AttributeAdder (f)) = 1
//    if fns prev then prev
//    else  
//       let
//          nextAdder = next selected
//       in
//          first nextAdder, (second nextAdder) ++ (second prev)

// bind: AttritebuteAdder a -> (a -> AttributeAdder b) -> AttributeAdder b) =
// bind m f =
//    test select =
//       runAdder select