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
         | DimensionWithDefault (d,_) ->  [ d ]
         | DimensionWithoutDefault (_) -> []


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
         |> List.map (fun (DefaultMember md) -> Header (md.Name |> create |> setY yspan,[]))
      memberHeaders,  defaultMemberHeaders 


   let fromDimension (dimension: Dimension)  =
      let memberHeaders = 
         Dimension.members dimension
         |> List.map (fun (DomainMember m) -> Header (create m.Name,[]))
      let defaultMemberHeaders =
         Dimension.defaultMember dimension
         |> List.map (fun (DefaultMember md) -> Header (md.Name |> create |> setY (YSpan 1),[]))
      memberHeaders , defaultMemberHeaders 

module Header =
   let create (dimensions: Dimension list) =
      let rec create' (dimensions: Dimension list)  (parent: Header option)=
            match dimensions with
            | [ dimension ] -> 
                  let (state,d) = HeaderItem.fromDimension dimension 
                  match parent with
                  | Some p -> 
                     let (Header (item,headers)) = p
                     [ Header (item, headers @ state @ d) ]
                  | None -> state @ d
            | head :: tail -> 
               let state,d = HeaderItem.fromDimension head 
               let newState = 
                  state 
                  |> List.collect (Some >> (create' tail)) 
               //add to parent
               match parent with
               | Some p -> 
                     let (Header (item,headers)) = p
                     [ Header (item, headers @ newState @ d) ]
               | None -> newState @ d

            | [] -> []
      create' dimensions None




// let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2"; "kv3"; "kv4"] 
// let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"; "Danmark" ] 
// let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 

let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2" ] 
let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"] 
let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 
let dim4 = Dimension.fromStringListWithDefault (DomainName "Produkt2") [  "Tung"; "Lätt" ] 




module Headers =

   // let addDimensions headers dim1 dim2 =

   
   let columns2 header =
      seq {
            let rec first (Header (item,headers)) =
               seq {
                     match headers with
                     | [] ->  
                           yield (item, [])
                     | _ ->
                         for h in headers do yield! second item ([]) h
                        //  for h in rest do yield! second item ([]) h
                     
                  }
            and second colItem  members (Header (item,headers)) =
               seq {
                    match headers with
                     | [] ->  
                           printfn "members: %A" (members @ [item.Member])
                           yield (colItem, members @ [item.Member])
                     | _->
                        for h in headers do yield! second colItem ([item.Member]) h
                 }
            first header
      }
      |> Seq.concat
      |> Seq.toList

   // let getColumns header =
   //       let rec first (Header (item,headers)) =
   //                match headers with
   //                | [] ->  
   //                       [(item,[])]
   //                | _ ->
   //                   let rec loop headers =
   //                      match headers with
   //                      | [] -> []
   //                      | head :: tail -> 
   //                         (second item ([]) head) @  loop tail 
   //                   loop headers
   //       and second colItem members (Header (item,headers)) =
   //               match headers with
   //                | [] ->  
   //                      printfn "members: %A" (members @ [item.Member])
   //                      [(colItem, members @ [item.Member])]
   //                | _->
   //                   let rec loop headers =
   //                      match headers with
   //                      | [] -> []
   //                      | head :: tail -> 
   //                         (second colItem ([item.Member]) head)  @  loop tail
   //                   loop headers
   //       first header

   // let columns h =
   //    let rec vierdo (parent: Header option) header  =
   //       let (Header (items,headers)) = header
   //       match headers with
   //       | [] ->  match parent with | Some p -> [p]| None -> []
   //       | [ Header (x,y) ] ->
   //          match parent with
   //          | None -> 
   //                let m =  Header (x,[]) |> Some
   //                let t = vierdo m
   //                y |> List.collect t
   //          | Some (Header (z,v)) ->
   //                let children = [Header (x,[])] @ v 
   //                let m = Header (z,children) |> Some
   //                y |> List.collect (vierdo m)
   //       | (Header (x,y)) :: tail -> 
   //             match parent with
   //             | None -> 
   //                   y |> List.map (vierdo (Some (Header x,[])))
   //             | Some (Header (z,v)) ->
   //                   y |> List.map (vierdo (Some (Header z,[Header (x,[])] @ v)))    
   //    vierdo None h

// Header ("Norge", (Header (a,))   
let rec vierdo header  =
    // Sverige [ kv1 [ persbil, lastbil, produkt]], kv2 ....
      printfn "%A" header
      let (Header (items,headers)) = header
      let rec doit headers =
         match headers with
         | [ Header (x,y)] ->
             match y with
             | [ g ] -> 
                  printfn "1"
                  vierdo  (Header (x, [ g]  @ [Header (items, [])]))
             | [] ->
                printfn "2"
                [ Header (x, [Header (items, [])])]
             |  g :: l ->  
                  printfn "3"
                  printfn "%A" g
                  vierdo  (Header (x,[ g ]  @ [Header (items,  l )]))
                 
         | [] -> [ header ]
         | Header (x,y) :: tail -> 
             match y with
             | [ g ] -> 
                  printfn "4"
                  vierdo  (Header (x, [ g ]  @ [Header (items, [])]))
             | [] ->
                printfn "5"
                [ Header (x, [Header (items, [])])]
             | g :: l -> 
               //g är kv1 [lastbil, .....]
               printfn "6" 
               vierdo  (Header (x, [ g ]  @ [Header (items,  l )]))
             @ doit tail
            
             
      doit headers
let headers = Header.create [ dim2 ; dim1 ; dim3; dim4  ]
let xy =    headers |> List.collect vierdo 

printfn "Starta skapa kolumner" 

// let t =  
//    List.collect (Headers.c) headers 


let dim11 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2" ] 
let dim12 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"] 
let dim13 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 
let dim14 = Dimension.fromStringListWithDefault (DomainName "Produkt2") [  "Tung"; "Lätt" ] 

type AccumulatedDimensions = | AccumulatedDimensions of Header List List


let rec addDimensionsToHeaders (AccumulatedDimensions accHeaders) (dimension : Dimension) =
  let mapHeaders headers =
      headers |> List.map (fun header -> [ header ])
  let headers, totals = HeaderItem.fromDimension dimension
  match accHeaders with
  | [] -> mapHeaders headers
  | _ -> headers |> List.collect (fun header -> accHeaders |> List.map (fun accHeader ->  [ header ] @ accHeader )) 
  @ mapHeaders totals 
  |> AccumulatedDimensions

let getHeaders dimensions =
   dimensions |> List.fold addDimensionsToHeaders (AccumulatedDimensions [])

getHeaders [ dim1 ; dim2  ]


let rec konceptHeader (dimensionalKoncept: DimensionalKoncept) =
   match dimensionalKoncept with
   | DimensionalAbstract (ak, koncepts) -> [Header (ak.Name |> abstractKonceptNameToString |> HeaderItem.create  ,[])] @ (koncepts |> List.collect konceptHeader)
   | DimensionalValue vk -> [Header (vk.Name |> valueKonceptNameToString |> HeaderItem.create  ,[])] 

let testKoncept = 
    DimensionalAbstract ("Abstract1"|> AbstractKonceptName |> createAbstract, [DimensionalAbstract ("Abstract2"|> AbstractKonceptName |> createAbstract , [ "Value1" |>  ValueKonceptName |> createValue |> DimensionalValue  ]); ])
[testKoncept; testKoncept] |> List.collect konceptHeader       
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



