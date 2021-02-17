open System

type ValueKonceptId = ValueKonceptId of Guid

type ValueKonceptName = ValueKonceptName of String

let valueKonceptNameToString (ValueKonceptName name) = name
   
type  ValueKoncept =
    {
            Name: ValueKonceptName
            Id: ValueKonceptId
            Selected: Boolean
    }
type AbstractKonceptName = AbstractKonceptName of String

let abstractKonceptNameToString (AbstractKonceptName name) = name
   
type AbstractKonceptId = AbstractKonceptId of Guid

type  AbstractKoncept =
    {
            Name : AbstractKonceptName
            Id : AbstractKonceptId
            Selected: Boolean
    }

let createAbstract name : AbstractKoncept=
   { Name = name ; Id = Guid.NewGuid() |> AbstractKonceptId; Selected = false}

let createValue name : ValueKoncept =
   { Name = name ; Id = Guid.NewGuid() |> ValueKonceptId; Selected = false}

type DomainName = | DomainName of String

let domainNameToString (DomainName name) = name

type Factor = | Factor of int
type Member =
   {
      Id: Guid
      Name: String
      Factor: Factor
   }

module Member =
   let create factor name  =
      { Id = Guid.NewGuid(); Name = name ; Factor = factor}

   let fromList (f: 'a -> Member)  m = 
      m |> List.map f

type DomainMember = | DomainMember of Member
module DomainMember =
   let create factor name  = 
      Member.create factor name 
     |> DomainMember

let domainMemberToString (DomainMember m) = m
type Domain =
    {
      Name: DomainName
      Members:  DomainMember List
    }

type DefaultMember = | DefaultMember of Member  
module DefaultMember =
   let create factor name  = 
      Member.create factor name 
     |> DefaultMember

type Dimension =
   | DimensionWithDefault of (DefaultMember*Domain)
   | DimensionWithoutDefault of Domain

module Dimension =
   let members dimension =
      match dimension with
      | DimensionWithDefault (_,d) -> d.Members
      | DimensionWithoutDefault (d) -> d.Members

   let defaultMember dimension =
       match dimension with
         | DimensionWithDefault (d,_) ->  Some d 
         | DimensionWithoutDefault (_) -> None


   let fromListWithDefault f d m =
          m 
          |> Member.fromList f 
          |> List.map DomainMember
          |> fun members ->  DimensionWithDefault ((Member.create (Factor 1) (sprintf "total:%A" d) |> DefaultMember),{ Name = d ; Members = members })
  
   let fromListWithoutDefault f d m =
          m 
          |> Member.fromList f 
          |> List.map DomainMember
          |> fun members ->  DimensionWithoutDefault ({ Name = d ; Members = members })

   let fromStringListWithDefault =
         fromListWithDefault (Member.create (Factor 1))

   let fromStringListWithoutDefault =
         fromListWithoutDefault (Member.create (Factor 1))

type HyperDimension =
     | Opened of Dimension
     | Closed of Dimension

type HyperCubeName = | HyperCubeName of String

let hyperCubeNameToString (HyperCubeName name) = name

type HyperCubeId = HyperCubeId of Guid

type  HyperCube =
    {
           Name: HyperCubeName
           Head: HyperDimension
           Tail:  HyperDimension List
           List: HyperCubeId
    }

type DimensionalKoncept =
    | DimensionalAbstract  of (AbstractKoncept*  DimensionalKoncept List) 
    | DimensionalValue of ValueKoncept

type Koncept =
    Cube  of (HyperCube * DimensionalKoncept List)
    | Abstract  of (AbstractKoncept *  Koncept List)
    | Value of ValueKoncept

type Line = { Span: int; Start: int }

type HorizontalLine= HorizontalLine of Line

module HorizontalLine =
   let span (HorizontalLine line) = line.Span
   let start (HorizontalLine line) = line.Start
   let setSpan span (HorizontalLine line)  = HorizontalLine { line with Span = span}
   let incrementStart  (HorizontalLine line)  = HorizontalLine { line with Start = line.Start + 1}
   let inrcementStartWithSpan (HorizontalLine line)  = HorizontalLine { line with Start = line.Start + line.Span}
type VerticalLine = VerticalLine of Line

module VerticalLine =
   let span (VerticalLine line) = line.Span
   let start (VerticalLine line) = line.Start
   let setSpan  span (VerticalLine line) = VerticalLine { line with Span = span}
   let incrementStart  (VerticalLine line) = VerticalLine { line with Start = line.Start + 1}
   let inrcementStartWithSpan (VerticalLine line)  = VerticalLine { line with Start = line.Start + line.Span}

type Direction = 
   | Horizontal
   | Vertical

type Area =
   {
      VerticalLine: VerticalLine
      HorizontalLine: HorizontalLine
   }

type Depth = | Depth of int
module Area =
   let init direction totalSpan =
      match direction with
      | Horizontal ->
         { VerticalLine = VerticalLine { Span = 0; Start = 0};  HorizontalLine = HorizontalLine {Span = totalSpan; Start = 1}}
      | Vertical ->
         { VerticalLine = VerticalLine  { Span = totalSpan; Start = 1}; HorizontalLine = HorizontalLine {Span = 0; Start = 0}}

   let total (direction: Direction) (Depth depth) (area: Area) =
      let verticalLine, horizontalLine = area.VerticalLine, area.HorizontalLine
      match direction with
      | Horizontal ->
         let newVertical = VerticalLine.setSpan depth verticalLine 
         let newHorizontal = 
            horizontalLine
            |> HorizontalLine.inrcementStartWithSpan 
            |> HorizontalLine.setSpan 1
         { VerticalLine = newVertical ; HorizontalLine =  newHorizontal}
      | Vertical ->
         let newHorizontal = HorizontalLine.setSpan depth horizontalLine 
         let newVertical =
            verticalLine
            |> VerticalLine.inrcementStartWithSpan 
            |> VerticalLine.setSpan 1 
         { VerticalLine = newVertical ; HorizontalLine = newHorizontal}

let emptyArea = { VerticalLine = VerticalLine { Span = 0; Start = 0}; HorizontalLine = HorizontalLine { Span = 0; Start = 0}}  

type HeaderItem = {
   Area: Area
   Member : Member 
   Write: Boolean
}

type Header = Header of HeaderItem

module Header  =
   let create area =
      (Member.create (Factor 1)) 
      >> (fun m -> 
               {  
                  Area = area
                  Member = m
                  Write = true})

   let fromDimension (direction: Direction) depth (area: Area) dimension  =
      let members, defaultMember = Dimension.members dimension, Dimension.defaultMember dimension
      let calcSpan span =
         match defaultMember with
         | Some _ -> (span - 1)
         | None -> span 
         / members.Length
   
      let fArea  =
         match direction with
         | Horizontal ->
               let newSpan = HorizontalLine.span area.HorizontalLine |> calcSpan
               let start = HorizontalLine.start area.HorizontalLine 
               let verticalStart = VerticalLine.start area.VerticalLine + 1
               fun ordinal ->
                  { HorizontalLine =  HorizontalLine { Span = newSpan; Start = start + ordinal * newSpan}; VerticalLine = VerticalLine { Span = 1 ; Start = verticalStart}}
          | Vertical ->
               let newSpan = VerticalLine.span area.VerticalLine |> calcSpan
               let start = VerticalLine.start area.VerticalLine 
               let horizontalStart = HorizontalLine.start area.HorizontalLine + 1
               fun ordinal ->
                   { HorizontalLine = HorizontalLine { Span = 1; Start = horizontalStart}; VerticalLine = VerticalLine { Span = newSpan ; Start = start + ordinal * newSpan }}

      let memberHeaders = 
         members
         |> List.mapi (fun i (DomainMember m) ->  m.Name |> create (fArea i)|> Header)
       
      let da headers = 
         let result = headers |> List.rev |> List.head |> (fun (Header item) -> item.Area)
         printfn "header before total %A" result
         result
      let defaultMemberHeaders =
         defaultMember
         |> Option.map (fun (DefaultMember md) ->  
            let area = Area.total direction depth (da memberHeaders)
            (md.Name |> create area |> Header))

      memberHeaders, defaultMemberHeaders 

module Option =
   let toList (a: 'a option) =
      match a with
      | Some v -> [v]   
      | None -> [] 


//    let create (dimensions: Dimension list) =
//       let rec create' (dimensions: Dimension list)  (parent: Header option)=
//             match dimensions with
//             | [ dimension ] -> 
//                   let (state,d) = HeaderItem.fromDimension dimension 
//                   match parent with
//                   | Some p -> 
//                      let (Header (item,headers)) = p
//                      [ Header (item, headers @ state @ (d |> Option.toList)) ]
//                   | None -> state @ (d |> Option.toList)
//             | head :: tail -> 
//                let state,d = HeaderItem.fromDimension head 
//                let newState = 
//                   state 
//                   |> List.collect (Some >> (create' tail)) 
//                //add to parent
//                match parent with
//                | Some p -> 
//                      let (Header (item,headers)) = p
//                      [ Header (item, headers @ newState @ (d |> Option.toList)) ]
//                | None -> newState @ (d |> Option.toList)

//             | [] -> []
//       create' dimensions None
   // let setPostion xspan xstart yspan ystart (Header (item, headers)) =
   //    Header ({ item with XSpan = xspan; XStart = xstart; YSpan = yspan; YStart = ystart} , headers)



type SimpleHeader = | SimpleHeader of Header
// module SimpleHeader =
//    let setArea direction ordernr  position (SimpleHeader header)=
//       Header.setPostionSimple ordernr position direction header 
//       |> SimpleHeader   

type TotalHeader = | TotalHeader of Header

type HeadersSection = Headers of (SimpleHeader List * TotalHeader option)

module HeadersSection =
   let fromDimension direction depth area dimension =
      let s,t = Header.fromDimension direction depth area dimension
      Headers (s |> List.map SimpleHeader, t |> Option.map TotalHeader)

type AccumulatedHeader = 
   | Simple of SimpleHeader List
   | Total of TotalHeader * SimpleHeader list

  

//todo: Simple header a non empty list
let addColumns direction depth dimension (headers: SimpleHeader List)  =
   let (SimpleHeader (Header s)) = headers.Head
   let simples,total = Header.fromDimension direction depth s.Area dimension
   // match header with
   let notWritableHeaders = headers |> List.map (fun (SimpleHeader (Header item)) -> { item with Write = false } |> Header |> SimpleHeader)
   let mapHeaders i headers= 
      if i = 0 then 
         headers
      else notWritableHeaders 
      
   // | Simple s -> 
   let v1 = simples |> List.mapi (fun i simple -> Simple (SimpleHeader simple :: (mapHeaders i headers ))) 
   let v2 = total |> Option.toList |> List.map (fun t -> Total (TotalHeader t,notWritableHeaders))
   v1 @ v2

   // | Total (t,s) ->
   //    [ header ]

let addColumns' depth direction dimension (acc: AccumulatedHeader) =
   match acc with
   | Total _ -> [ acc ]
   | Simple simples -> addColumns direction depth dimension simples

let addDimension direction depth totalSpan (acc: AccumulatedHeader List) (dimension : Dimension) =
   match acc with
   | [] -> 
      let area = Area.init direction totalSpan
      let (simples,total)=  Header.fromDimension direction depth area dimension
      let v1 = simples |> List.map (fun simple -> Simple [SimpleHeader simple])
      let v2 = total |> Option.toList |> List.map (fun t -> Total (TotalHeader t,[]))
      v1 @ v2
   | _ -> acc |> List.collect (addColumns' depth direction dimension)

let calculateSpanForDimensions dimensions =
   let rec calculataSpan dims =
      match dims with
      |[] -> 1
      | head :: tail ->
         match head with
         | DimensionWithDefault (_,m) ->  m.Members.Length * (calculataSpan tail) + 1 
         | DimensionWithoutDefault (m)-> m.Members.Length * (calculataSpan tail)

   calculataSpan dimensions    
let getHeaders2 direction dimensions =
   let totalSpan = calculateSpanForDimensions dimensions
   let depthTotal = Depth dimensions.Length
   // dimensions |> List.fold (addDimension direction depthTotal totalSpan) []
   let rec fold depth acc dimensions =
      match dimensions with
      | [] -> acc
      | head :: tail -> 
         let (Depth d) = depth
         let nextDepth = (d - 1) |> Depth
         let state = addDimension direction depth totalSpan acc head
         fold nextDepth state tail
   fold depthTotal [] dimensions 

let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2" ] 
let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"] 
let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 
let dim4 = Dimension.fromStringListWithDefault (DomainName "Produkt2") [  "Tung"; "Lätt" ] 


let width = calculateSpanForDimensions [ dim1 ; dim2 ]
let headers = getHeaders2 Direction.Vertical [ dim1 ; dim2; dim3 ]
let header = headers |> List.rev |> List.head

let calcSpan koncept =
   let rec span state (koncept: DimensionalKoncept) =
      let newState = state + 1
      match koncept with
      | DimensionalAbstract (_, koncepts) -> 
           let rec span' state koncepts =
               match koncepts with
               | [] -> [ newState ]
               | head :: tail ->    
                  span newState head
                  @ span' newState tail
           span' state koncepts
      | DimensionalValue _ ->
            [ newState ] 
   span 0 koncept 

let headersFromKoncepts koncepts =
   
   let rec konceptHeader span (dimensionalKoncept: DimensionalKoncept) =
      match dimensionalKoncept with
      | DimensionalAbstract (ak, koncepts) -> 
            printfn "State %A" span
            [  Header (ak.Name 
               |> abstractKonceptNameToString 
               |> Header.create emptyArea)
            ] @ (koncepts |> List.collect (konceptHeader (span + 1)) )
      | DimensionalValue vk -> 
            printfn "State %A" span
            [
               Header (vk.Name 
               |> valueKonceptNameToString 
               |> Header.create emptyArea)
            ] 
   koncepts |> List.collect (konceptHeader 1)

let rec depth (dimensionalKoncept: DimensionalKoncept) =
   match dimensionalKoncept with
   | DimensionalAbstract (_, koncepts) -> 
        let rec calc koncepts =
           match koncepts with
           | [] -> [1]
           | head :: tail ->
               depth head @ calc tail
   
        calc koncepts
   | DimensionalValue _ -> [ 1 ]

let ca name koncepts =  DimensionalAbstract ((name|> AbstractKonceptName |> createAbstract), koncepts)
let ca1 name koncept =  DimensionalAbstract ((name|> AbstractKonceptName |> createAbstract), [ koncept ])
let empty = ca "e" []
let t = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [] )))
let t' = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [t ] )))
let t2 = ca "head" [ t; t ;empty; empty]
let testKoncept = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue ]); ])
let testKoncept1 = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue;testKoncept ;testKoncept]); ])
let testKoncept2 = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue ; testKoncept1]); ])


let headers1 = headersFromKoncepts [ testKoncept1 ; testKoncept2 ]



let rec span (koncept: DimensionalKoncept) =
   match koncept with
   | DimensionalAbstract (_, koncepts) -> 
           if koncepts.IsEmpty then  1
                 
           else
           let rec span' koncepts =
               match koncepts with
               | [] -> 0
               | head :: tail ->    
                  span head
                  + span' tail 
           span' koncepts
   | DimensionalValue _ -> 1


span t2
 1 t2 








