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

type Member =
   {
      Id: Guid
      Name: String
      Factor: int
   }
type DomainMember = | DomainMember of Member


let domainMemberToString (DomainMember name) = name
type Domain =
    {
            Name: DomainName
            Members:  DomainMember List
    }

type DefaultMember = | DefaultMember of Member  


type Dimension =
   | DimensionWithDefault of (DefaultMember*Domain)
   | DimensionWithoutDefault of Domain

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


type HeaderItem = {
   XSpan: int
   YSpan: int
   XStart: int
   YStart: int
   Member : Member 
}

type Header = Header of (HeaderItem * Header List)

module Member =
   let create name : Member =
      { Factor = 1; Id = Guid.NewGuid() ; Name = name}

module HeaderItem  =
   let create =
      Member.create 
      >> (fun m -> 
               {  
                  XSpan = 1
                  YSpan = 1
                  XStart = 1
                  YStart = 1
                  Member = m})


module Header =
   let rec create (members: string list list) (headers:Header List) =
      match members with
      | [ head ]-> 
         let newHeaders = 
            head 
            |> List.map (HeaderItem.create >> (fun item -> Header (item,headers)))
 
         newHeaders

      | head :: tail -> 
         let state = 
            head 
            |> 
            List.map (HeaderItem.create >> (fun item -> Header (item,headers)))
         
         create tail state
      | [] -> []

Header.create [ [ "kv1"; "kv2"; "kv3";"kv4"]; [ "Sverige"; "Norge"; "Danmark"]] []
       
         // let state = 
         //    head 
         //    |> List.map HeaderItem.create 
         //    |> List.map (fun item -> HeaderItem (item,headers))
         // state 
         // List.fold 

  

// type Headers =
//    {
//       Headers: Header List
//    }


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
   let columns header =
      seq {
            let rec first (Header (item,headers)) =
               seq {
                     match headers with
                     | [] ->  
                           yield (item, [])
                     | Header (im,hrs) :: rest ->
                         for h in hrs do yield! second item ([im.Member]) h
                         for h in rest do yield! second item ([]) h
                     
                  }
            and second colItem  members (Header (item,headers)) =
               seq {
                    match headers with
                     | [] ->  
                           printfn "members: %A" (members @ [item.Member])
                           yield (colItem, members @ [item.Member])
                     | Header (im,hrs) :: rest ->
                         for h in hrs do yield! second colItem (members @ [ im.Member ]) h
                         for h in rest do yield! second colItem (members) h
                 }
            first header
      }
      |> Seq.concat
      |> Seq.toList
   
   let columns2 header =
      seq {
            let rec first (Header (item,headers)) =
               seq {
                     match headers with
                     | [] ->  
                           yield (item, [])
                     | _ ->
                         for h in headers do yield! second item ([item.Member]) h
                        //  for h in rest do yield! second item ([]) h
                     
                  }
            and second colItem  members (Header (item,headers)) =
               seq {
                    match headers with
                     | [] ->  
                           printfn "members: %A" (members @ [item.Member])
                           yield (colItem, members @ [item.Member])
                     | _->
                        for h in headers do yield! second item ([item.Member]) h
                 }
            first header
      }
      |> Seq.concat
      |> Seq.toList

let x = Header.create [ [ "kv1"; "kv2"; "kv3";"kv4"]; [ "Sverige"; "Norge"; "Danmark"]; ["bilar" ; "båtar"]] []
let x' = Header.create [ [ "kv1"; "kv2"]; [ "Sverige"]] []
let x1 = Header.create [ [ "kv1"; "kv2"]; [ "Sverige"]] []   
let t =  List.map (Headers.columns) x1
//let v = t [] |> Seq.concat

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



