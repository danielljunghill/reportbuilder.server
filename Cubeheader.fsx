// type Format<'r,'a> = Format of ((string -> 'r) -> 'a)

// A formatter. This type holds all the information we need to create a formatting function, wrapped up in a way that makes it easy to compose.

// Build one of these up with primitives like s, string and int, join them together with <>, and when you're done, generate the final printing function with print.
#load "Area.fs"
#load "Model.fs"
open Model.NList
open Model
open Area




type Factor = Factor of int

type Id = Id of int


type Member = { Name: string; Factor: Factor}
type Members = Members of Member NList 

type MembersIdentifier = MembersIdentifier of Member NList

module Members =
   let values (Members members) = members
   let addFirst m (Members members)  = 
      members 
      |> NList.addFirst m
      |> Members


let rec cross (l1: 'a NList List) (l2: 'a NList)=
  l1 |> List.fold (fun state a -> state @ (l2 |> NList.map (fun b  -> NList.addFirst b a) |> NList.toList)) []
 //(fun b -> NList.addHead b a))) []
let rec collecting state (f: 'b ->  'a NList) (m: 'b List)  =
   match m with
      | [] -> state
      | head :: tail ->
         let newList = f head
         let currentState =
            if List.isEmpty state then
               newList 
               |> NList.toList
               |> List.map  (fun a -> { Head = a; Tail = []})             
            else
               cross state newList
         collecting currentState f tail

let createMember name = { Name = name; Factor = Factor 7}

let m1 = 
   [createMember "kv2"]
   |> NList.create2 (createMember "kv1")
   |> Members 
let m2 = 
    [(createMember "2021")]
   |> NList.create2 (createMember "2020") 
   |> Members 
let m3 = 
    [(createMember "norge")]
   |> NList.create2 (createMember "sverige")
   |> Members 

let fn (Members members) = members
let total = 
   collecting [] fn [ m1; m2; m3 ] 
   |> List.map MembersIdentifier



let rec cross2 totalt (l1: (int * 'a List) List) (l2: 'a List) =
   let length = List.length l2
   let newTotal = totalt / length
   printfn "newTotal %i %i %i" totalt newTotal length
   let result = l1 |> List.fold (fun state (j,a )-> state @ (l2 |> List.mapi (fun i b -> ((i * newTotal  + j ),[ b ] @ a)))) []
   newTotal,result


let ms = [ m1; m2 ; m3]
let f (Members ms) = ms

let totalw = collecting  [] f ms

type HeaderIdentifier =
   | Factor of Factor
   | Sequence of SequenceId

type HeaderName = HeaderName of string

type Attribute = Attribute of string
type Header =
   {
      Span: Span
      Start: Start
      Depth: Depth
      Name: string
      Attributes: Attribute List 
      Identifiers: HeaderIdentifier NList
   }


let rec cross3 (Span totalSpan) (parents: Header List) (members: Member List) =
   let newHeader index (Span span) (parent: Header) (m: Member) =
      {
         Start = Start (index * span + (startInt parent.Start))
         Depth = Depth ((depthInt parent.Depth) + 1)
         Span = Span span
         Name = m.Name
         Identifiers = NList.addFirst (m.Factor |> Factor) parent.Identifiers
         // todo atributes if 
         Attributes = []
      }
   let countMembers = List.length members
   let newSpan = totalSpan / countMembers |> Span
   // printfn "newTotal %i %i %i" totalt newTotal length
   parents 
   |> List.fold (fun state header -> state @ ( members |> List.mapi (fun i m -> newHeader i newSpan header m))) []
  

let rec collecting3 (Span totalSpan) state (m: Members List)  =
   match m with
      | [] -> []
      | head :: tail ->
         let (Members members) = head
         let totalState = totalSpan / (NList.length members) 
         let currentState =
            if List.isEmpty state then            
               members
               |> NList.toList
               |> List.mapi (fun i m ->  
                           {  
                               Name = m.Name
                               Identifiers =  m.Factor |> Factor |> NList.create 
                               Attributes = []
                               Start = (i * totalState + 1) |> Start 
                               Span = totalState |> Span
                               Depth = Depth 1} )
            else
               cross3 (Span totalSpan) state (members |> NList.toList)
         currentState @ collecting3 (Span totalState) currentState tail   
// type Factor = Factor of int

let headers = collecting3 (Span 12) [] [ m1 ; m2; m3]


// let addHeaders (Header Member List) 
