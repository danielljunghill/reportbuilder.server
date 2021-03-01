open System

type NList<'a> =
   {
         Head: 'a
         Tail: 'a List 
   }

module NList =

   let create a =
      {
            Head = a
            Tail = []
      }

   let create2 m l =
      {
            Head = m
            Tail = l
      }

   let toList m = 
     [ m.Head ] @ m.Tail

   let length m =
      1 + (m.Tail |> List.length)
     

   let last m =
      match m.Tail |> List.rev with
      | [] -> m.Head
      | head :: _ -> head

   let map f m =
      {
            Head = f m.Head
            Tail = m.Tail |> List.map f
      }
      
   let mapi f m =
      let rec mapi' i m =
         match m with
         | [] -> []
         | head :: tail -> [ f i head ] @ mapi' (i + 1) tail
      {
         Head = f 0 m.Head
         Tail =  mapi' 1 m.Tail         
      }

   let append n m =
      { n with Tail = n.Tail @ m}

   let addFirst n m =

      let tail = toList m
      {
         Head = n
         Tail = tail
      }

   let addList n m =
      { n with  Tail = n.Tail @ m }

   
       
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
      m |> NList.map f

type DomainMember = | DomainMember of Member
module DomainMember =
   let create factor name  = 
      Member.create factor name 
     |> DomainMember

let domainMemberToString (DomainMember m) = m
type Domain =
    {
      Name: DomainName
      Members:  DomainMember NList
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
          |> NList.map DomainMember
          |> fun members ->  DimensionWithDefault ((Member.create (Factor 1) (sprintf "total:%A" d) |> DefaultMember),{ Name = d ; Members = members })
  
   let fromListWithoutDefault f d m =
          m 
          |> Member.fromList f 
          |> NList.map DomainMember
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

type Span = Span of int
module Span =
   let add (Span span1) (Span span2)  = 
      span1 + span2
      |> Span
   let addint (Span span) i  = 
      span + i 
      |> Span
   let value (Span span) = span
   let increment (Span span) = 
      span + 1
      |> Span
   let decrement (Span span) = 
      span - 1
      |> Span
type Start = Start of int
module Start =
   let add (Start start1) (Start start2)  = 
      start1 + start2
      |> Start
   let addint(Start start) i = 
      start + i 
      |> Start
   let value (Start start) = start
   let increment (Start start) = 
      start + 1
      |> Span
   let decrement (Start start) = 
      start - 1
      |> Span


type Line = { Span: Span; Start: Start }

type HorizontalLine= HorizontalLine of Line

module HorizontalLine =
   let span (HorizontalLine line) = line.Span
   let start (HorizontalLine line) = line.Start
   let setSpan span (HorizontalLine line)   = HorizontalLine { line with Span = span}
   let setStart start (HorizontalLine line)    = HorizontalLine { line with Start = start}
   let incrementStart  (HorizontalLine line)  = HorizontalLine { line with Start = 1 |> Start.addint line.Start }
   let inrcementStartWithSpan (HorizontalLine line)  = HorizontalLine { line with Start = line.Span |> Span.value |> Start.addint line.Start }
type VerticalLine = VerticalLine of Line

module VerticalLine =
   let span (VerticalLine line) = line.Span
   let start (VerticalLine line) = line.Start
   let setSpan span (VerticalLine line)   = VerticalLine { line with Span = span}
   let setStart start (VerticalLine line)    = VerticalLine { line with Start = start}
   let incrementStart  (VerticalLine line) = VerticalLine { line with Start = Start.addint line.Start 1}
   let inrcementStartWithSpan (VerticalLine line)  = VerticalLine { line with Start = line.Span |> Span.value |> Start.addint line.Start }

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
         { VerticalLine = VerticalLine { Span = Span 0; Start = Start 0};  HorizontalLine = HorizontalLine {Span = totalSpan; Start = Start 1}}
      | Vertical ->
         { VerticalLine = VerticalLine  { Span = totalSpan; Start = Start 1}; HorizontalLine = HorizontalLine {Span = Span 0; Start = Start 0}}

   let incrementVerticalStart (area: Area)=
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine = v |> VerticalLine.inrcementStartWithSpan; HorizontalLine = h}
  
   let incrementHorizontalStart (area: Area)=
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine = v ; HorizontalLine = h |> HorizontalLine.incrementStart }            

   let setHorizontalStart start area =
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine = v ; HorizontalLine =  h |> HorizontalLine.setStart start }         

   let setVerticalStart start area =
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine =   v |> VerticalLine.setStart start ; HorizontalLine = h }  

   let setVerticalSpan span area =
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine =  v |> VerticalLine.setSpan span   ; HorizontalLine = h }  

   let setHorizontalSpan span area =
      let v, h = area.VerticalLine, area.HorizontalLine
      { VerticalLine = v ; HorizontalLine =  h |> HorizontalLine.setSpan span  }

   let verticalStart area =
      VerticalLine.start  area.VerticalLine

   let horizontalStart area = 
      HorizontalLine.start area.HorizontalLine

   let verticalSpan area =
      VerticalLine.span area.VerticalLine

   let horizontalSpan area =
      HorizontalLine.span area.HorizontalLine

   let total (direction: Direction) (Depth depth) (area: Area) =
      let verticalLine, horizontalLine = area.VerticalLine, area.HorizontalLine
      match direction with
      | Horizontal ->
         let newVertical = verticalLine |> VerticalLine.setSpan (Span depth)  
         let newHorizontal = 
            horizontalLine
            |> HorizontalLine.inrcementStartWithSpan 
            |> HorizontalLine.setSpan (Span 1)
         { VerticalLine = newVertical ; HorizontalLine =  newHorizontal}
      | Vertical ->
         let newHorizontal = horizontalLine |> HorizontalLine.setSpan  (Span depth)    
         let newVertical =
            verticalLine
            |> VerticalLine.inrcementStartWithSpan 
            |> VerticalLine.setSpan (Span 1) 
         { VerticalLine = newVertical ; HorizontalLine = newHorizontal}

let emptyArea = { VerticalLine = VerticalLine ({ Span = (Span 0); Start = (Start 0)}); HorizontalLine = HorizontalLine { Span = (Span 0); Start = (Start 0)}}  

type HeaderItem = {
   Area: Area
   Member : Member NList
}

type DomainHeader = DomainHeader of HeaderItem

module DomainHeader  =
   let create area l =
      (Member.create (Factor 1)) 
      >> (fun m  -> 
               {  
                  Area = area
                  Member = NList.create2 m l})

   let fromDimension (direction: Direction) depth (area: Area) dimension (pm:Member List)  =
      let members, defaultMember = Dimension.members dimension, Dimension.defaultMember dimension
      let calcSpan (Span span) =
         match defaultMember with
         | Some _ -> (span - 1)
         | None -> span 
         / NList.length members
         |> Span

      let calcStart ordinal (Start start) (Span span)  =
         start + span * ordinal
         |> Start

      let calcArea direction area ordinal   =
         match direction with
         | Horizontal ->
               let horizontalSpan = HorizontalLine.span area.HorizontalLine |> calcSpan
               let horizontalstart = calcStart ordinal (HorizontalLine.start area.HorizontalLine) horizontalSpan
               let verticalStart = Start.add (VerticalLine.start area.VerticalLine) (Start 1)    
               { HorizontalLine =  HorizontalLine { Span = horizontalSpan; Start = horizontalstart}; VerticalLine = VerticalLine { Span = (Span 1) ; Start = verticalStart}}
          | Vertical ->
               let verticalSpan = VerticalLine.span area.VerticalLine |> calcSpan
               let verticalStart = calcStart ordinal (VerticalLine.start area.VerticalLine) verticalSpan
               let horizontalStart =  Start.add (HorizontalLine.start area.HorizontalLine) (Start 1)
               { HorizontalLine = HorizontalLine { Span = Span 1; Start = horizontalStart}; VerticalLine = VerticalLine { Span = verticalSpan ; Start = verticalStart }}

      let memberHeaders = 
         members
         |> NList.mapi (fun i (DomainMember m) ->  m.Name |> create (calcArea direction area i) pm |> DomainHeader)
       
      let getLastHeader headers = 
         headers 
         |> NList.last 
         |> (fun (DomainHeader item) -> item.Area)

      let defaultMemberHeader =
         defaultMember
         |> Option.map (fun (DefaultMember md) ->  
            let area = Area.total direction depth (getLastHeader memberHeaders)
            (md.Name |> create area pm|> DomainHeader))

      memberHeaders, defaultMemberHeader 

module Option =
   let toList (a: 'a option) =
      match a with
      | Some v -> [v]   
      | None -> [] 

   let reverse m =
      let rec reverse' a =
         match a with
         | [] -> []
         | head :: tail ->
            reverse' tail @ [ head ]
      reverse' m

   let l = [ 1 ; 2 ; 3 ; 5]

let n = Option.reverse [ 1 ; 2 ; 3 ; 5]



type TableHeader = 
   | MemberHeader of DomainHeader NList
   | TotalHeader of DomainHeader NList

type Column = Column of Member NList

type ColumnHeader = 
   {
       IsTotal: Boolean
       Member: Member
       Area: Area
   }
let domainHeaderAsColumnHeader isTotal (DomainHeader d) =
   {
      Area = d.Area
      IsTotal = isTotal
      Member = d.Member.Head
   }
module TableHeader =
   let columns (acc: TableHeader) =
      match acc with
         | MemberHeader headers -> headers
         | TotalHeader headers -> headers
        |> (fun headers -> headers.Head)
        |> (fun (DomainHeader item)-> Column item.Member)

   let headers  (acc: TableHeader) =
       match acc with
       | MemberHeader headers -> headers |> NList.map (domainHeaderAsColumnHeader false)
       | TotalHeader headers -> headers |> NList.map (domainHeaderAsColumnHeader true)
       |> NList.toList
//todo: Simple header a non empty list
let addColumns direction depth dimension (headers: DomainHeader NList)  =
   let (DomainHeader header) = headers.Head
   let simples,total = DomainHeader.fromDimension direction depth header.Area dimension (header.Member |> NList.toList)
   // match header with
   let mapHeaders i headers = 
      if i = 0 then 
         headers |> NList.toList
      else 
         [] 
   // | Simple s -> 
   let v1 = simples |> NList.mapi (fun i m -> MemberHeader (NList.create2 m (mapHeaders i headers ))) 
   let v2 = total |> Option.toList |> List.map  (NList.create >> TotalHeader)
   NList.addList v1 v2

   // | Total (t,s) ->
   //    [ header ]

let addColumns' depth direction dimension (acc: TableHeader) =
   match acc with
   | TotalHeader _ -> NList.create acc
   | MemberHeader m -> addColumns direction depth dimension m
   |> NList.toList

let addDimension direction depth totalSpan (acc: TableHeader List) (dimension : Dimension) =
   match acc with
   | [] -> 
      let area = Area.init direction totalSpan
      let (members,total)=  DomainHeader.fromDimension direction depth area dimension []
      let v1 = members |> NList.map (NList.create >> MemberHeader)
      let v2 = total |> Option.toList |> List.map (NList.create >> TotalHeader)
      NList.addList v1 v2
      |> NList.toList
   | _ -> acc |> List.collect (addColumns' depth direction dimension)

let calculateSpanForDimensions dimensions =
   let rec calculataSpan dims =
      match dims with
      |[] -> 1
      | head :: tail ->
         match head with
         | DimensionWithDefault (_,m) ->  (NList.length m.Members) * (calculataSpan tail) + 1 
         | DimensionWithoutDefault (m)-> (NList.length m.Members) * (calculataSpan tail)

   calculataSpan dimensions 
   |> Span 

let getHeaders2 direction dimensions =
   // let totalSpan = calculateSpanForDimensions dimensions
   // let depthTotal = Depth dimensions.Length
   // dimensions |> List.fold (addDimension direction depthTotal totalSpan) []
   let rec fold depth span acc dimensions  =
      match dimensions with
      | [] -> acc
      | head :: tail -> 
         let (Depth d) = depth
         let nextDepth = (d - 1) |> Depth
         let state = addDimension direction depth span acc head
         fold nextDepth span state tail 
   let span = calculateSpanForDimensions dimensions
   fold (Depth dimensions.Length) span [] dimensions 

// let getHeaders2 direction dimensions =
//    let totalSpan = calculateSpanForDimensions dimensions
//    let depthTotal = Depth dimensions.Length
//    // dimensions |> List.fold (addDimension direction depthTotal totalSpan) []
//    let rec fold depth acc dimensions =
//       match dimensions with
//       | [] -> acc
//       | head :: tail -> 
//          let (Depth d) = depth
//          let nextDepth = (d - 1) |> Depth
//          let state = addDimension direction depth totalSpan acc head
//          fold nextDepth state tail
//    fold depthTotal [] dimensions 
let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") (NList.create2 "kv1" ["kv2" ]) 
let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien")  (NList.create2 "Sverige" ["Norge" ])
let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt")  (NList.create2 "Personbil" ["Lastbil" ]) 
let dim4 = Dimension.fromStringListWithDefault (DomainName "Produkt2")  (NList.create2 "Tung" ["Latt" ])


let width = calculateSpanForDimensions [ dim1 ; dim2; dim3 ]
let headers = getHeaders2 Direction.Horizontal [ dim1 ; dim2 ]


let columns = headers |> List.map TableHeader.columns
let structure = headers |> List.collect TableHeader.headers 
let header = headers |> List.rev |> List.head


type KonceptHeaderItem =
   | Abstract of AbstractKoncept
   | Value of ValueKoncept

type KonceptHeader = 
   {
      Area: Area
      Item: KonceptHeaderItem
   }

module KonceptHeader =
   let createAbstract area item =
      { Area = area ; Item = Abstract  item   }
   let createValue area item =
      { Area = area ; Item = Value  item   }
let headersForKontext (koncepts: DimensionalKoncept List)=
   let maxSpan = koncepts |> List.collect calcSpan |> List.max
   let area y x  = 
      { VerticalLine = VerticalLine { Start = y; Span = Span 1}; HorizontalLine = HorizontalLine { Start = x ; Span = Span (maxSpan - (Start.value x) + 1) } }
 
   let rec konceptHeader start (koncept: DimensionalKoncept) =
      match koncept with
      | DimensionalAbstract (ak, koncepts) -> 
            printfn "State %A" start
            [  
                KonceptHeader.createAbstract (area (Start 1) start) ak
            ] @ (koncepts |> List.collect (konceptHeader (Start.addint start 1)) )
      | DimensionalValue vk -> 
            printfn "State %A" start
            [
               KonceptHeader.createValue (area (Start 1) start) vk
                
            ] 
   koncepts |> List.collect (konceptHeader (Start 1))
   |> List.mapi (fun i (item: KonceptHeader) -> { item with Area = item.Area |> Area.setVerticalStart (Start (i + 1)) })

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
let cv name = DimensionalValue (name |> ValueKonceptName |> createValue )
let ca2 name =  ca name []
let empty = ca "e" []
let t = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [] )))
let t' = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [t ] )))
let t2 = ca "head" [ ca2 "first"  ; ca2 "second"; cv "third"]
let t3 = ca "head" [ t2 ]
let testKoncept = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue ]); ])
let testKoncept1 = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue;testKoncept ;testKoncept]); ])
let testKoncept2 = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue ; testKoncept1]); ])


let headers1 = headersForKontext [ testKoncept1 ; testKoncept2 ]

let rec calculateSpan (koncept: DimensionalKoncept) =
   match koncept with
   | DimensionalAbstract (_, koncepts) -> 
           if koncepts.IsEmpty then  1
                 
           else
              let rec span' koncepts =
                  match koncepts with
                  | [] -> 0
                  | head :: tail ->    
                     calculateSpan head
                     + span' tail 
              span' koncepts
   | DimensionalValue _ -> 1

// let calcSpan koncept =
//    let rec span state (koncept: DimensionalKoncept) =
//       let newState = state + 1
//       match koncept with
//       | DimensionalAbstract (_, koncepts) -> 
//            let rec span' state koncepts =
//                match koncepts with
//                | [] -> [ state ]
//                | head :: tail ->    
//                   span newState head
//                   @ span' newState tail
//            span' state koncepts
//       | DimensionalValue _ ->
//             [ newState ] 
//    span 0 koncept 
let calcSpan koncept =
   let rec span state (koncept: DimensionalKoncept) =
      let newState = state + 1
      match koncept with
      | DimensionalAbstract (_, koncepts) -> 
           let rec span' koncepts =
               match koncepts with
               | [] -> [ newState ]
               | head :: tail ->    
                  span newState head
                  @ span' tail
           span' newState koncepts
      | DimensionalValue _ ->
            [ newState ] 
   span 0 koncept 

let count = calcSpan t3

let setVerticalStart area =
      if area |> Area.verticalStart = Start 0 then 
         area |> Area.setVerticalStart (Start 1)
      else area
let setHorizontalStart area = 
      if area |> Area.horizontalStart = Start 0 then 
         area |> Area.setHorizontalStart (Start 1)
      else area
let setstart = setVerticalStart >> setHorizontalStart

let addSpanToStart (Start start) (Span span) = 
   start + span
   |> Start
let calculateSameLevelArea (direction: Direction) span area  =
         let newArea =
            match direction with
            | Horizontal  -> 
                //vertical start = span + area start  
               //add + 1 to horizontal
               //set vertical span
               let prevStart = Area.verticalStart area
               let prevSpan = Area.verticalSpan area
               area 
               |> Area.setVerticalStart (addSpanToStart prevStart prevSpan)
               |> Area.setVerticalSpan span
               |> Area.setHorizontalSpan (Span 1)
            | Vertical ->  
               let prevStart = Area.horizontalStart area
               let prevSpan = Area.horizontalSpan area
               area 
               |> Area.setHorizontalStart (addSpanToStart prevStart prevSpan)
               |> Area.setHorizontalSpan span
               |> Area.setVerticalSpan (Span 1)
         let newArea2 = newArea |> setstart   
         printfn "New Area: %A" newArea2
         newArea2
         
let calculateSubLevelArea (direction: Direction) area kontext =
   let span = kontext |> calculateSpan |> Span

   match direction with
   | Horizontal  -> 
      //add + 1 to horizontal
      //set vertical span

       area |> Area.incrementHorizontalStart |> Area.setVerticalSpan span 
   | Vertical ->  
       area |> Area.incrementVerticalStart |> Area.setHorizontalSpan span
          //calculate new area 
   |> setstart

let calculateValueArea direction area =
   match direction with
   | Horizontal  -> 
       area |> Area.incrementVerticalStart
   | Vertical ->  
       area |> Area.incrementHorizontalStart
          //calculate new area 
   |> setstart
let rec kontextHeadersAsTree (direction: Direction) (pa: Area) (kontexts: DimensionalKoncept list) =
   match kontexts with
   | [] -> []
   | head :: tail ->
      
      let rec kontextHeader (area: Area) (kontext: DimensionalKoncept) =    
          match kontext with
          | DimensionalAbstract (ak, koncepts) ->
               [  KonceptHeader.createAbstract area ak   ]
               @
               match koncepts with
               | [] -> []
               | head :: tail ->
                  //calculate pa för head
                  let area = calculateSubLevelArea direction area head
                  kontextHeader area head
                  //calculate pa for tail
                  @ kontextHeadersAsTree direction area tail
          | DimensionalValue vk ->
                [ KonceptHeader.createValue (calculateValueArea direction pa) vk ]
          
      
      let span = head |> calculateSpan |> Span
      let newArea =  calculateSameLevelArea direction span pa
      printfn "newArea %A Header %A" newArea head
      kontextHeader newArea head
      //calculate pa for tail
      @ kontextHeadersAsTree direction newArea tail

kontextHeadersAsTree Direction.Horizontal emptyArea [ t2 ; t2]
kontextHeadersAsTree Direction.Horizontal emptyArea [ t ; t]
kontextHeadersAsTree Direction.Horizontal emptyArea [ t ; t]


let kk = emptyArea |> setstart
// span t2
//  1 t2 


let dd = [ 1; 2 ; 3 ; 4]

let dd2 = dd @ [ 5 ]


List.append [ 1; 2 ; 3 ; 4] [ 5 ]
