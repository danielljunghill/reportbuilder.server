
open System

type ValueKonceptId = ValueKonceptId of Guid
type ValueKonceptName = ValueKonceptName of String
type ValueKoncept =
    {
        Name: ValueKonceptName
        Id: ValueKonceptId
    }

type DomainName = DomainName of string
type DomainMember = DomainMember of string
type Domain =
    {
        Name: DomainName
        Members: DomainMember List
    }
module Domain =
    let create name members =
        {
            Name = DomainName name
            Members = members |> List.map DomainMember
        }

type DefaultMember = DefaultMember of string  
type Dimension =
     | DimensionWithDefault of DefaultMember * Domain
     | DimensionWithoutDefault of Domain

module Dimension =
    let createWithDefault domain =
        DimensionWithDefault (DefaultMember "Total", domain)
    let createWithoutDefault domain =
        DimensionWithoutDefault domain

type HyperDimension =
     | Opened of Dimension
     | Closed of Dimension


type HyperCubeName = | HyperCubeName of string
type HyperCubeId = HyperCubeId of Guid
type HyperCube =
    {
        Name: HyperCubeName
        Head: HyperDimension
        Tail: HyperDimension List
        Id: HyperCubeId
    }

type DimensionalAbstractKonceptId = DimensionalAbstractKonceptId of Guid
type DimensionalAbstractKonceptName = DimensionalAbstractKonceptName of string

type DimensionalAbstractKoncept =
    {
        Id : DimensionalAbstractKonceptId
        Name: DimensionalAbstractKonceptName
    }

type AbstractKonceptName = AbstractKonceptName of string
type AbstractKonceptId = AbstractKonceptId of Guid
type AbstractKoncept =
    {
        Name : AbstractKonceptName
        Id : AbstractKonceptId
    }

type DimensionalKoncept =
    | AbstractKoncept of AbstractKoncept * DimensionalKoncept List
    | ValueKoncept of ValueKoncept

 type Koncept =
    | Cube of HyperCube * DimensionalKoncept List
    | AbstractKoncept of AbstractKoncept * Koncept List
    | ValueKoncept of ValueKoncept

module ValueKoncept =
    
    let create name : ValueKoncept =
        {
            Id = Guid.NewGuid() |> ValueKonceptId 
            Name = ValueKonceptName name

        }
    let addToKoncept (vk: ValueKoncept) (parent: Koncept) =
        match parent with
        | Cube (cube,koncepts) -> (cube, koncepts @ [ vk |> DimensionalKoncept.ValueKoncept ]) |> Koncept.Cube |> Ok
        | AbstractKoncept (ak,koncepts) -> (ak, koncepts @ [ vk |> Koncept.ValueKoncept ]) |> Koncept.AbstractKoncept |> Ok
        | _ -> Error (sprintf "ValueKoncept koncept cannot be added to %A" parent)

     
    let addToDimensionalKoncept (vk: ValueKoncept) (parent: DimensionalKoncept) = 
        match parent with
        | DimensionalKoncept.AbstractKoncept (ak, d) -> (ak, d @ [ (vk |> DimensionalKoncept.ValueKoncept) ]) |> DimensionalKoncept.AbstractKoncept|> Ok
        | DimensionalKoncept.ValueKoncept _ -> Error (sprintf "ValueKoncept koncept cannot be added to %A" parent)

    // let addtoPage 
 module AbstractKoncept = 
    let create name : AbstractKoncept =
            {
                Id = Guid.NewGuid() |> AbstractKonceptId 
                Name = AbstractKonceptName name
            }

    let addToKoncept (abstractKoncept: AbstractKoncept) (parent: Koncept) =
        match parent with
        | Cube (cube,koncepts) -> (cube, koncepts @ [ DimensionalKoncept.AbstractKoncept (abstractKoncept,[]) ]) |> Koncept.Cube |> Ok
        | AbstractKoncept (ak,koncepts) -> (ak, koncepts @ [  Koncept.AbstractKoncept (abstractKoncept,[]) ]) |> Koncept.AbstractKoncept |> Ok
        | _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)
  
    let addToDimensionalKoncept (ak: AbstractKoncept) (parent: DimensionalKoncept) = 
        match parent with
        | DimensionalKoncept.AbstractKoncept (ak, koncepts) -> (ak, koncepts @ [  DimensionalKoncept.AbstractKoncept (ak, []) ]) |> DimensionalKoncept.AbstractKoncept |> Ok
        | DimensionalKoncept.ValueKoncept _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)

module HyperCube =

    let create name dimension =
        {
            Id = Guid.NewGuid() |> HyperCubeId
            Name = name |> HyperCubeName
            Head = dimension
            Tail = []
        }

    let addToKoncept hc (parent: Koncept) =
         match parent with
            | AbstractKoncept (ak,koncepts) -> (ak, koncepts @ [  Koncept.Cube (hc,[]) ]) |> Koncept.AbstractKoncept |> Ok
            | _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)

    let addDimension hc dimension =
        { hc with Tail = hc.Tail @ [ dimension ]}


module Result =    
    let join (r: Result<Result<_,_>,_>) =
        match r with
        | Result.Ok ri-> ri
        | Result.Error err -> Error err

module DimensionalKoncept =
        type ParentDimensionalKoncept= | ParentDimensionalKoncept of DimensionalKoncept

        let create = ValueKoncept.create >> DimensionalKoncept.ValueKoncept
        let add (koncept: DimensionalKoncept) (ParentDimensionalKoncept parent) =
            match koncept with
            | DimensionalKoncept.AbstractKoncept (ak,koncepts) -> 
                (ak, koncepts @ [ koncept])
                 |> DimensionalKoncept.AbstractKoncept|> Ok
            | _ -> Error "cannot add"
        let map f koncept =
            let rec map' parent koncept  =
                let fmap koncept =
                    match koncept with
                    | DimensionalKoncept.AbstractKoncept (ak, koncepts) ->
                        let newKoncept = (ak, []) |> DimensionalKoncept.AbstractKoncept |> Some |> Ok
                        let accKoncept = koncepts |> List.map f |> List.fold map' newKoncept 
                        let fn1 parent acc =
                            let fn2 acc p =
                                match p with
                                | None -> acc |> Ok
                                | Some parentKoncept -> 
                                    match acc with
                                    | Some childKoncept -> 
                                        parentKoncept
                                        |> ParentDimensionalKoncept 
                                        |> add childKoncept
                                    | None ->  parentKoncept |> Ok
                                    |> Result.map Some
                            Result.bind (fn2 acc) parent
                        accKoncept |> Result.bind (fn1 parent)
                    | DimensionalKoncept.ValueKoncept vk -> 
                        let fn parent =
                            match parent with
                            | Some p -> ValueKoncept.addToDimensionalKoncept vk p
                            | None ->  vk |> DimensionalKoncept.ValueKoncept |> Ok
                            |> Result.map Some
                        parent |> Result.bind fn
                Result.bind fmap koncept
            map' (Ok None) koncept
            |> Result.map (fun v -> match v with | Some vi -> Ok vi | None -> Error "Empty result from map")
            |> Result.join


module Koncept =
    let createAbstract name =
        (AbstractKoncept.create name, []) |> Koncept.AbstractKoncept
    let createValue =
        ValueKoncept.create >> Koncept.ValueKoncept
    type ParentKoncept = ParentKoncept of Koncept
    let add koncept (ParentKoncept parent) =
        match parent with
        | AbstractKoncept  (ak, koncepts) ->  (ak , koncepts @ [ koncept]) |> AbstractKoncept |> Ok
        | _ -> Error (sprintf "%A koncept cannot be added parent to %A" parent koncept )

    let iter (koncept: Koncept) =
        let rec map' (parent: Result<Koncept Option, String>) (koncept: Koncept) : Result<Koncept Option, String> =
            match koncept with
            | Koncept.AbstractKoncept (ak, koncepts) ->
                let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> Some |> Ok
                let accKoncept = koncepts |> List.fold map' newKoncept 
                let fn1 (parent: Result<Koncept option, string>) (acc:Koncept option) =
                    let fn2 (acc: Koncept option) (p: Koncept option)  =
                        match p with
                        | None -> acc |> Ok
                        | Some parentKoncept -> 
                            match acc with
                            | Some childKoncept -> 
                                parentKoncept
                                |> ParentKoncept 
                                |> add childKoncept
                            | None ->  parentKoncept |> Ok
                            |> Result.map Some
                    Result.bind (fn2 acc) parent
                accKoncept |> Result.bind (fn1 parent)
            | Koncept.ValueKoncept vk ->
                let fn (parent: Koncept option) =
                    match parent with
                    | Some p -> ValueKoncept.addToKoncept vk p
                    | None ->  vk |> Koncept.ValueKoncept |> Ok
                    |> Result.map Some
                parent |> Result.bind fn

            | Koncept.Cube (hc,koncepts)->
                Error "Handling of Cube not implemented"
        map' (Ok None) 
    


    let maybeAddKoncept' (child: Koncept option) (parent: ParentKoncept) =
      match child with
      | Some koncept ->
         add koncept parent
      | None -> 
         let (ParentKoncept pk) = parent
         Ok pk

    let maybeAddKoncept child  (parent: Result<Koncept option, string>) =
      let f = fun pk -> 
         match pk with
         | Some pk -> 
             maybeAddKoncept' child (pk |> ParentKoncept) 
             |> Result.map Some
         | None -> 
            match child with
            | Some ck -> ck |> Some |> Ok
            | None -> Ok None
      Result.bind f parent

   //  let maybeAddKoncept (child: Koncept Option) parent  =
   //      match child with
   //      | Some ck -> add ck parent
   //      | None -> 
   //          let (ParentKoncept pk) = parent
   //          Ok pk

   //  let maybeAddKoncept' parent child = 
   //      let t = (maybeAddKoncept child >> Result.map Some)
   //      Result.bind t parent
   // let maybeAddKoncept (child: Koncept Option) (parent: Koncept option)  =
   //      match child with
   //      | Some ck -> 
   //          add ck parent
   //      | None -> 
   //          let (ParentKoncept pk) = parent
   //          Ok pk
    //Option<Koncept> Option<Koncept>
      //  match parent with
      //  | None -> child |> Option.map Ok
      //  | Some pk -> 
      //       pk
      //       |> tryAddKoncept child 
            
         //   match child with
         //   | Some ck -> 
         //       pk
         //       |> add ck
         //   | None ->  
         //       let (ParentKoncept parent) = pk
         //       parent |> Ok
         //   |> Result.map Some

   //  let fn2 child parent = //Option<Koncept> Option<Koncept>
   //        match p with
   //        | None -> acc |> Ok
   //        | Some parentKoncept -> 
   //            match acc with
   //            | Some childKoncept -> 
   //                parentKoncept
   //                |> ParentKoncept 
   //                |> add childKoncept
   //            | None ->  parentKoncept |> Ok
   //            |> Result.map Some

   //  let tryAdd parent acc = //Result<Option<Koncept>,string> Option<Koncept>
   //    Result.bind (fn2 acc) parent

    let map (f: Koncept -> Result<_,_>) koncept =
        let rec map' (parent: Result<Koncept option, string>) koncept  =
            let fmap koncept =
                match koncept with
                | Koncept.AbstractKoncept (ak, koncepts) ->
                    let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> Some |> Ok
                    let accKoncept = koncepts |> List.map f |> List.fold map' newKoncept 
                    accKoncept
                    |> Result.bind (fun child ->  maybeAddKoncept child parent)
                | Koncept.ValueKoncept _ -> 
                    maybeAddKoncept (koncept |> Some) parent
                | Koncept.Cube _ ->
                    f koncept 
                    |> Result.map  Some
                    |> Result.bind (fun child ->  maybeAddKoncept child parent)
            Result.bind fmap koncept
        map' (Ok None) koncept
        |> Result.map (fun v -> match v with | Some vi -> Ok vi | None -> Error "Empty result from map")
        |> Result.join


     
//  open Koncept
//  open AbstractKoncept
//  open ValueKoncept

let a1 = 
    "Head abstract1"  
    |> Koncept.createAbstract 
    |> Koncept.ParentKoncept  
    |> Koncept.add ("Sub abstract2" |> Koncept.createAbstract)

let v1 = Koncept.createValue "First Values"
let added =
    a1
    |> Result.map Koncept.ParentKoncept 
    |> Result.bind (Koncept.add v1)

// let mapped = 
//     added 
//     |> Result.bind (Koncept.map )

let mapKoncept (koncept: Koncept) =
    match koncept with
    | Koncept.AbstractKoncept (ak,koncepts) ->
        if ak.Name = AbstractKonceptName "Sub abstract2" then
            Koncept.AbstractKoncept (ak, koncepts @ ([ "ett jävla value" |> Koncept.createValue ]))
        else
            koncept 
    | _ -> koncept
    |> Ok

let added2 = Koncept.map mapKoncept added
let added3 = Koncept.map mapKoncept added2

let optionKoncept (koncept: Koncept) (konceptAdd: Koncept option) =
    match konceptAdd with
    | Some k -> k
    | None -> koncept
    |> Ok

let mapCube (f:HyperCube -> DimensionalKoncept List-> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.Cube (hc,koncepts) ->
        f hc koncepts
        |> Result.bind (optionKoncept koncept)
    | _-> koncept |> Ok
   

let mapAbstractKoncept (f:AbstractKoncept -> Koncept List -> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.AbstractKoncept (ak,koncepts) ->
        f ak koncepts
        |> Result.bind (optionKoncept koncept) 
    | _-> koncept |> Ok
 

let mapValueKoncept (f:ValueKoncept -> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.ValueKoncept vk ->
        f vk
        |> Result.bind (optionKoncept koncept) 
    | _-> koncept |> Ok


let addCube  =
    let hyperDimension = 
        ["kv1"; "kv2"; "kv3";"kv4"]
        |> Domain.create "Kvartal" 
        |> Dimension.createWithDefault
        |> Closed
        |> HyperCube.create "Kvartal och annat"  

    let f ak koncepts =
        if ak.Name = AbstractKonceptName "Sub abstract2" then
            (ak, koncepts @ ([ Koncept.Cube (hyperDimension, []) ])) 
            |> Koncept.AbstractKoncept 
            |> Some
        else
            None
        |> Ok

    mapAbstractKoncept f 
    >> Result.mapError (fun err -> sprintf "%A" err)
    // |> Result.mapError (fun err -> sprintf "%A" err)


let added4 = Koncept.map addCube added3

let addDimensionalKoncept  =
    let dimKoncepts = [ DimensionalKoncept.create "Intäkter" ; DimensionalKoncept.create "Försäljning cyklar"; DimensionalKoncept.create "Bidrag" ]
    let f (hc: HyperCube) dimension =
        if hc.Name = HyperCubeName "Kvartal och annat" then
            (hc,dimKoncepts) |> Koncept.Cube |> Some
        else
            None 
        |> Ok
    mapCube f 

let added5 = Koncept.map addDimensionalKoncept added4
//add


let addValue  =
    let f (ak:AbstractKoncept) koncepts =
       if ak.Name = AbstractKonceptName "Head abstract1" then
            (ak, koncepts @ [ "dagen d" |> ValueKoncept.create |> Koncept.ValueKoncept]) |> Koncept.AbstractKoncept |> Some
       else
            None
       |> Ok
    mapAbstractKoncept f  
  

let added6 = Koncept.map addValue  added5        
// let addDimensionsToCube dimension koncept =
//     let f (hc: HyperCube) koncepts =
//         if hc.Name = "Kvartal och annat" then
//             hc, d
   
    //|> (Koncept.add v1)let added2 = v1 |> Koncept.ParentKoncept |> Koncept.add v1 

                // let f' 
                // let t =
                //     parent 
                //     |> Result.bind fn

                //     let innerFn (pi: Koncept option) 

                // Result.bind 
                // match parent with
                // | None -> accKoncept |> Ok
                // | Some p -> 
                //         match k with
                //         | Some k' -> 'k
                //         | None -> add 
               


// module 
   
// module HyperCube =

//     let addToKoncept (cube: HyperCube) (parent: Koncept) =
//         match parent with
//         | AbstractKoncept (ak,koncepts) -> (ak, koncepts @ [  Koncept.Cube (cube,[]) ]) |> Koncept.AbstractKoncept |> Ok
//         | _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)


//     let create hyperCubeName hyperDimension =
//             {
//                 Name = hyperCubeName
//                 Head = hyperDimension
//                 Tail = []
//                 Id =  Guid.NewGuid() |> HyperCubeId 
//             }
//     let addDimension dimension (cube: HyperCube) = { cube with Tail = List.append cube.Tail [ dimension ]}

//     let dimension (cube: HyperCube) = List.append [ cube.Head ] cube.Tail
// module Dimension =
//     let createWithDefault defaultmember domain = DimensionWithDefault (defaultmember,domain) 
//     let createWithoutDefault = DimensionWithoutDefault

// module Domain =
//     let empty name = { Name = name; Members = []}
//     let addMember m (domain: Domain) = { domain with Members = List.append domain.Members [ m ] }

// module HyperDimension =
//     let openedWithDefault defaultmember domain = DimensionWithDefault (defaultmember,domain) |> Opened
//     let closedWithDefault = DimensionWithoutDefault >> Closed
//     let openedWithoutDefault = DimensionWithoutDefault >> Opened

// module HyperCube =
//     let 

// module Koncept =



      
    // let add (ParentKoncept parent) koncept =
    //   match parent with
    //    | Value (ValueKoncept ki) ->  
    //         Koncepts (AbstractKoncept ki, [ koncept ])
    //    | Koncepts (ki,kl) ->
    //         Koncepts (ki , List.append kl [ koncept ]) 

    // ///copy koncept
    // let rec copy parent state =
    //     match state with
    //     | Koncepts (ak,koncepts) ->
    //         let newAk =  Koncepts (ak,[]) |> Some
    //         let acc = List.fold copy newAk koncepts 

    //         match parent with
    //         | None -> acc 
    //         | Some p -> 
    //               match acc with
    //               | None -> Some p
    //               | Some child -> Some (add (ParentKoncept p) child)
    //     | Value v ->  
    //         match parent with 
    //         | Some p -> Some (add (ParentKoncept p) state)
    //         | None -> Some state
    // // type DimensionalKonceptError = 
    // //     | OnlyAbstractKonceptCanHaveChildren of ValueKoncept
    // //     | NotImplemented of string

    // // type DimensionalParent =
    // //     | Cube of HyperCube
    // //     | AbstractKoncept of AbstractKoncept

    // // let add (v: DimensionalKoncept) (DimensionalParent parent) =
    // //     match parent with
    // //     | DimensionalParent.AbstractKoncept (a,dks) -> Ok (DimensionalKoncept.AbstractKoncept (a, dks @ [ DimensionalKoncept.ValueKoncept v ]))
    // //     | DimensionalParent.Cube c -> v |> OnlyAbstractKonceptCanHaveChildren |> Result.Error  

    // // let rec copy (parent:DimensionalKoncept option) (state: DimensionalKoncept) =
  
    // //         match state with
    // //         | DimensionalKoncept.AbstractKoncept (abstractKoncept,dimensionalKoncepts) ->
    // //             //todo: Copy Abstract koncept
    // //             let newAbstractKoncept =  DimensionalKoncept.AbstractKoncept (abstractKoncept,[]) |> Some
    // //             let acc = List.fold copy newAbstractKoncept dimensionalKoncepts 
    // //             match parent with
    // //             | None -> acc 
    // //             | Some p -> 
    // //                   match acc with
    // //                   | None ->  p |> Some   
    // //                   | Some child ->  (addValueKoncept (ParentKoncept p) child) 
    // //         | DimensionalKoncept.ValueKoncept v ->  
    // //             match parent with 
    // //             | Some p -> Some (addValueKoncept (ParentKoncept p) state)
    // //             | None -> Some State
             


    // // let map f k =
    // //       let rec map' (parent: Koncept option) state   =
    // //         match state with
    // //         | Koncepts (AbstractKoncept ki,koncepts) ->
    // //             let newAk = f ki |> AbstractKoncept       
    // //             let newKoncept =  newAk |>  abstractKonceptToKoncept |> Some
    // //             let acc = List.fold map' newKoncept koncepts 
                
    // //             match parent with
    // //               | None -> acc 
    // //               | Some p -> 
    // //                   match acc with
    // //                   | None -> Some p
    // //                   | Some child -> Some (add (ParentKoncept p) child)
    // //         | Value (ValueKoncept ki) ->  
    // //              let newValueKoncept = f ki |> ValueKoncept |> Value            
    // //              match parent with 
    // //               | Some p -> Some (add (ParentKoncept p) newValueKoncept)
    // //               | None -> Some newValueKoncept
    // //       map' None k