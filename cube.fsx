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

   // let addDimension (header: Header option) (dimension: Dimension)  =
   //    let (Header (p,items)) = header
   //    let childHeaders = 
   //          Dimension.members dimension
   //          |> List.map (fun (DomainMember m) -> Header (HeaderItem.create m.Name,[]))
   //    //Här ska det submembes läggas till
          
   //    let defaultHeader =
   //       Dimension.defaultMember dimension
   //       |> List.map (fun (DefaultMember md) -> Header (md.Name |> HeaderItem.create |> HeaderItem.setY (YSpan 1) ,[]))
   //    Header (p,childHeaders @ defaultHeader)


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

            | [] -> ArgumentException() |> raise
      create' dimensions None
     // |> List.rev 

   // let create (headers: string list list) =
   //    let rec create'  (headers:Header List) (members: string list list) =
   //       match members with
   //       | [ head ]-> 
   //          let newHeaders = 
   //             head 
   //             |> List.map (HeaderItem.create >> (fun item -> Header (item,headers)))
    
   //          newHeaders

   //       | head :: tail -> 
   //          let state = 
   //             head 
   //             |> 
   //             List.map (HeaderItem.create >> (fun item -> Header (item,headers)))
            
   //          create' state tail 
   //       | [] -> []
   //    members 
   //    |> List.rev 
   //    |> create' []
   // let create1 (dimensions: Dimension list) =
   //    let rec fromDimension'  (headers:Header List) (dimensions: Dimension list) =
   //       match dimensions with
   //       | [ dimension ]-> 
   //          let headers, total =  dimension |> HeaderItem.fromDimension headers (YSpan 1) 
   //          headers @ total
   //       | dimension :: tail -> 
   //          //headers - total
   //          let headers, total = dimension |> HeaderItem.fromDimension headers (YSpan (tail.Length + 1))
            
   //          printfn "Headers: %A" headers 
   //          printfn "Total: %A" total
   //          (fromDimension' headers tail) @ total
   //          //
   //       | [] -> []
   //    dimensions 
   //   // |> List.rev 
   //    |> fromDimension' []


// let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1"; "kv2"; "kv3"; "kv4"] 
// let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"; "Norge"; "Danmark" ] 
// let dim3 = Dimension.fromStringListWithDefault (DomainName "Produkt") [  "Personbil"; "Lastbil" ] 

let dim1 = Dimension.fromStringListWithDefault (DomainName "Kvartal") [ "kv1" ] 
let dim2 = Dimension.fromStringListWithDefault (DomainName "Scandinavien") [  "Sverige"] 
let dim3 = Dimension.fromStringListWithoutDefault (DomainName "Produkt") [  "Personbil" ] 
let headers = Header.create [ dim2 ; dim1  ]

module Headers =
   // let expand (Header (item, headers)) =
   //    let rec first (Header (item, headers))  =
   //       match headers with
   //       | Header (x,y) :: tail -> 
   //             second ((item, [item.Member] )) y
               
   //       | [] -> ( item, [item.Member] )
   //    and second ((x,y):HeaderItem * Member List) (Header (item, headers)) =
   //       match headers with
   //       | Header (x,y) :: tail -> 
   //             second 
               
   //       | [] -> ( item, [item.Member] )

   
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

   let columns header =
            let rec first (Header (item,headers)) =
                     match headers with
                     | [] ->  
                            [(item, [item.Member])]
                     | _ ->
                        let rec loop headers =
                           match headers with
                           | [] -> []
                           | head :: tail -> 
                              (second ([item.Member]) head) @  loop tail 
                        loop headers
            and second  members (Header (item,headers)) =
                    match headers with
                     | [] ->  
                           printfn "members: %A" (members @ [item.Member])
                           [(item, members)]
                     | _->
                        let rec loop headers =
                           match headers with
                           | [] -> []
                           | head :: tail -> 
                              (second  (members @ [item.Member]) head)  @  loop tail
                        loop headers
            first header
      




printfn "Starta skapa kolumner" 

let t =  
   List.map (Headers.columns) headers 
   |> List.concat

//  let  =


      
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



