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

   // let defaultMember dimension =
   //     match dimension with
   //       | DimensionWithDefault (d,_) ->  [ d ]
   //       | DimensionWithoutDefault (_) -> []

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


type ParentColumn = | ParentColumn of int * DomainMember List

type Axis =
   | DomainsOnly of Domain list
   | DomainsThenDimensionas of Domain list * DimensionalKoncept list * Domain list

module ParentColumn =
   let toValue (ParentColumn (col,dims)) = col,dims
   let addColumn (ParentColumn (col,dims)) = ParentColumn (col + 1,dims)
   let addMember m (ParentColumn (col,dims)) = ParentColumn (col + 1,dims @ [m])
   let toValueFromOption pc =
      match pc with
      | None -> 0, []
      | Some (ParentColumn (col, members)) -> col, members
   let addMemberToOption m pc = 
      toValueFromOption pc 
      |> ParentColumn
      |> addMember m
      |> Some


let xAxis (domainshead: Domain list) (dimensions: DimensionalKoncept list) (domainsrest: Domain list) =  NotImplementedException() |> raise


type XSpan = XSpan of int
type YSpan = YSpan of int
type XStart = XStart of int
type YStart = YStart of int

type HeaderItem = {
   XSpan: XSpan
   YSpan: YSpan
   XStart: XStart
   YStart: YStart
   Member : Member 
}

type Header = Header of (HeaderItem * Header List)

module HeaderItem  =
   let create  =
      (Member.create (Factor 1)) 
      >> (fun m -> 
               {  
                  XSpan = XSpan 1
                  YSpan = YSpan 1
                  XStart = XStart 1
                  YStart = YStart 1
                  Member = m})

   let setY (yspan: YSpan) hi  =
      { hi with YSpan = yspan }
     
   let fromDimensionOld headers yspan (dimension: Dimension)  =
      let memberHeaders = 
         Dimension.members dimension
         |> List.map (fun (DomainMember m) -> Header (create m.Name,headers))
      let defaultMemberHeaders =
         Dimension.defaultMember dimension
         |> Option.map (fun (DefaultMember md) -> Header (md.Name |> create |> setY yspan,[]))
      memberHeaders,  defaultMemberHeaders 


   let fromDimension (dimension: Dimension)  =
      let memberHeaders = 
         Dimension.members dimension
         |> List.map (fun (DomainMember m) -> Header (create m.Name,[]))
      let defaultMemberHeaders =
         Dimension.defaultMember dimension
         |> Option.map (fun (DefaultMember md) -> Header (md.Name |> create |> setY (YSpan 1),[]))
      memberHeaders , defaultMemberHeaders 

   let fromDimension2 (dimension: Dimension)  =
      let memberHeaders = 
         Dimension.members dimension
         |> List.map (fun (DomainMember m) -> Header (create m.Name,[]))
      let defaultMemberHeaders =
         Dimension.defaultMember dimension
         |> Option.map (fun (DefaultMember md) -> Header (md.Name |> create |> setY (YSpan 1),[]))
      memberHeaders , defaultMemberHeaders 

module Option =
   let toList (a: 'a option) =
      match a with
      | Some v -> [v]   
      | None -> [] 
module Header =
   let create (dimensions: Dimension list) =
      let rec create' (dimensions: Dimension list)  (parent: Header option)=
            match dimensions with
            | [ dimension ] -> 
                  let (state,d) = HeaderItem.fromDimension dimension 
                  match parent with
                  | Some p -> 
                     let (Header (item,headers)) = p
                     [ Header (item, headers @ state @ (d |> Option.toList)) ]
                  | None -> state @ (d |> Option.toList)
            | head :: tail -> 
               let state,d = HeaderItem.fromDimension head 
               let newState = 
                  state 
                  |> List.collect (Some >> (create' tail)) 
               //add to parent
               match parent with
               | Some p -> 
                     let (Header (item,headers)) = p
                     [ Header (item, headers @ newState @ (d |> Option.toList)) ]
               | None -> newState @ (d |> Option.toList)

            | [] -> []
      create' dimensions None

// let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2"; "kv3"; "kv4"] 
// let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"; "Danmark" ] 
// let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 

let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2" ] 
let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"] 
let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 
let dim4 = Dimension.fromStringListWithDefault (DomainName "Produkt2") [  "Tung"; "Lätt" ] 



type AccumulatedDimensions = | AccumulatedDimensions of Header List List

// let addDimensionsToHeaders (AccumulatedDimensions accHeaders) (dimension : Dimension) =
//   let mapHeaders headers =
//       headers |> List.map (fun header -> [ header ])
//   let headers, totals = HeaderItem.fromDimension dimension
//   match accHeaders with
//   | [] -> mapHeaders headers
//   | _ -> accHeaders |> List.collect (fun state -> (headers |> List.map (fun header->  header :: state))) 
//   @ mapHeaders totals 
//   |> AccumulatedDimensions

// let getHeaders dimensions =
//    dimensions |> List.fold addDimensionsToHeaders (AccumulatedDimensions [])

// getHeaders [ dim1 ; dim2; dim3  ]



type SimpleColumn = | SimpleColumn of Header
type TotalColumn = | TotalColumn of Header

type Columns = Columns of (SimpleColumn List * TotalColumn option)

module Columns =
   let fromDimension dimension =
      let s,t = HeaderItem.fromDimension dimension
      Columns (s |> List.map SimpleColumn, t |> Option.map TotalColumn)

type HeaderColumn = 
   | Simple of SimpleColumn List
   | Total of TotalColumn * SimpleColumn list
// module HeaderColumn =
//    let addHeader simpleColumn headerColumn =
//       match  with
//       | Simple headers -> 

// module HeaderColumn =
//    let add (acc: HeaderColumn List) (headers: HeaderColumn list) =
//       let rec add' accumulated =
//          match accumulated with
//          | [] -> []
//          | head :: tail -> 
//             match head with
//             | Simple simpleList ->  




let addColumns d (header:HeaderColumn)  =
   let (Columns (simples,total))= Columns.fromDimension d
   match header with
   | Simple s -> 
         let v1 = simples |> List.map (fun simple -> Simple (simple :: s)) 
         let v2 = total |> Option.toList |> List.map (fun t -> Total (t,s))
         v1 @ v2

   | Total (t,s) ->
      [ header ]


let addDimension (acc: HeaderColumn List) (dimension : Dimension) =
   match acc with
   | [] -> 
      let (Columns (simples,total))= Columns.fromDimension dimension
      let v1 = simples |> List.map (fun simple -> Simple [simple])
      let v2 = total |> Option.toList |> List.map (fun t -> Total (t,[]))
      v1 @ v2
   | _ -> acc |> List.collect (addColumns dimension)
 

            




// let addDimensional (accumulated: HeaderColumn List) (dimension : Dimension) =
//     let headers, totals = HeaderItem.fromDimension dimension
//     let mapHeaders headers =
//          headers |> List.map (fun header -> [ header ])
//     match accumalatedheaders with
//     | [] -> mapHeaders headers |> Simple @ 
//     | _ ->
//          let rec crossHeaders (ach: Header List List) (headers: Header list) =
//             match ach with
//             | [] -> []
//             | head :: tail -> 

//                let state = ach |> List.map (fun ah -> headers |> List.map (fun header -> header :: ah))
//                @ crossHeader tail headers
            
//          crossHeaders accumalatedheaders headers

     
let getHeaders2 dimensions =
   dimensions |> List.fold addDimension []
getHeaders2 [ dim1 ; dim2; dim3  ]
 
  
    
// type AccumulatedDimensionsState =
//    {
//          CurrentLevel: int
//          ParentCount: int
//          NumberOfTotals:int
//    }

// type AccumulatedDimensions = | AccumulatedDimensions of AccumulatedDimensionsState * Header List List

// module AccumulatedDimensions =
//    let state (AccumulatedDimensions (state, _)) = state
//    let headers (AccumulatedDimensions (_, headers)) = headers
//    let addTotals count (AccumulatedDimensions (state, headers)) =  
//          ({ state with NumberOfTotals = state.NumberOfTotals + count}, headers) 
//          |> AccumulatedDimensions
//    let addHeaders headers (AccumulatedDimensions (state, accHeaders)) = 
//          let mapHeaders headers =
//                headers |> List.map (fun header -> [ header ]) 
//          let stateHeaders =
//            match accHeaders with
//            | [] -> mapHeaders headers
//            | _ -> headers |> List.collect (fun header -> accHeaders |> List.map (fun accHeader ->  [ header ] @ accHeader )) 

//          (state,  headers |> List.collect (fun header -> accHeaders |> List.map (fun accHeader ->  [ header ] @ accHeader )) ) 
//          |> AccumulatedDimensions
//    let empty = ({CurrentLevel = 0; ParentCount = 0;NumberOfTotals = 0  }, []) |> AccumulatedDimensions    
// let rec calculateHeader acc (dimension : Dimension) =
//   let mapHeaders headers =
//       headers |> List.map (fun header -> [ header ])
//   let headers, totals = HeaderItem.fromDimension dimension
//   AccumulatedDimensions.addHeaders
//   @ mapHeaders totals 
//   |> AccumulatedDimensions

// let calculateHeaders dimensions =
//    dimensions |> List.fold calculateHeader (AccumulatedDimensions [])

getHeaders [ dim1 ; dim2  ]


let rec konceptHeader (dimensionalKoncept: DimensionalKoncept) =
   match dimensionalKoncept with
   | DimensionalAbstract (ak, koncepts) -> [Header (ak.Name |> abstractKonceptNameToString |> HeaderItem.create  ,[])] @ (koncepts |> List.collect konceptHeader)
   | DimensionalValue vk -> [Header (vk.Name |> valueKonceptNameToString |> HeaderItem.create  ,[])] 

let testKoncept = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue  ]); ])
[testKoncept; testKoncept] |> List.collect konceptHeader     



let calculateWidthForDimensions dimensions =
   let countMembers (dimension:Dimension) =
      match dimension with
      | DimensionWithDefault (_,m) ->  m.Members  |> List.length |> (+) 1
      | DimensionWithoutDefault (m)-> m.Members  |> List.length 

   dimensions |> List.fold (fun s m -> s + countMembers m) 0


let width = calculateWidthForDimensions [ dim1 ; dim2  ]
   // let rec columns (members: Member list) (headers: Header list) =
   //    match header with
   //    | header :: [] ->
   //    |
   // let rec columns (members: Member List) (Header (headerItem, headers))  =
   //       match headers with
   //       | [] -> yield (headerItem, members)
   //       | Header (item, m) :: tail ->
   //             let state = members @ [ item.Member ]
   //             yield! (m |> List.fold columns state)
   //             yield! tail |> List.fold columns members
            //   let state = columns ( members @ [ item.Member ]) m
            
            //   tail |> List.fold columns (members @ state) 


// type Row =
//    { 
//       Items: HeaderItem list
//       Child: Row option
//    }

// //|                  kv1               |                 kv2               |           kv3                     |              kv4                   |                total                |
// //|   Se    |     ne    |     total    |   Se    |     ne    |     total   |   Se    |     ne    |     total   |    Se    |     ne    |     total   |                                     | 

// let createParentColumn totalColumns dimension dimensions=
 
//     match dimension with
//     | DimensionWithDefault (d,m) -> 
//          let columnSpan = totalColumns / (m.Members.Length + 1 )
//          m.Members |> List. //add subdimensions
//          //default add span rowas many as thera are subdimensions
//     | DimensionWithoutDefault m -> 

// let  columner = 1
// let  rader = 2
// let domain offest (pc: ParentColumn Option) (dimensions: Dimension []) =
//     match dimensions with
//     | head :: [] ->

//     | head :: tail ->
    
    

   
   //



