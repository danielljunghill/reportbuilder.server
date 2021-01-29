
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
        map' (Ok None) koncept
    
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

    let maybeAdd (koncept: Koncept option) (parent: ParentKoncept option)  =
            match parent with
            | None -> koncept |> Ok
            | Some (ParentKoncept pk) -> 
                match koncept with
                | Some child -> 
                    pk
                    |> ParentKoncept
                    |> add child
                    |> Result.map Some
                | None ->  
                  pk |> Some |> Ok
     
    let andThenMaybeAdd (parent: Result<ParentKoncept option, string>) (koncept:Koncept option) =
        Result.bind (maybeAdd koncept) parent
    
    let mapToParent (k:Result<Koncept option, string>) =
         k |> Result.map (fun r -> r |> Option.map ParentKoncept)

    let parentAsKoncept (parent: Result<ParentKoncept option, string>) =
        parent |> Result.map (fun r -> r |> Option.map (fun (ParentKoncept k) -> k))

    let rec recursiveMap f p k  =
            let fmap parent (koncept: Koncept option)=
               match koncept with
               | Some k ->
                   match k with
                   | Koncept.AbstractKoncept (ak, koncepts) ->
                       let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> ParentKoncept |> Some |> Ok
                       let accKoncept = koncepts |> List.map f |> List.fold (recursiveMap f) newKoncept |> parentAsKoncept
                       accKoncept |> Result.bind (andThenMaybeAdd parent)
                       
                   | Koncept.ValueKoncept (_) -> 
                     let newKoncept = f k
                     Result.bind (andThenMaybeAdd parent) newKoncept
                   | Koncept.Cube (_)->
                       let newKoncept = f k
                       Result.bind (andThenMaybeAdd parent) newKoncept
                   |> mapToParent
               | None -> parent

            Result.bind (fmap p) k

    let map f m =
      
        recursiveMap f (Ok None) (m |> Result.map Some)
        |> parentAsKoncept
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
    |> Result.map Some

let added2 = Koncept.map mapKoncept added
let added3 = Koncept.map mapKoncept added2


let optionDefault defaultValue b=
    match b with
    | Some k -> k
    | None -> defaultValue
    |> Ok

let fMap2 f a b koncept =
      f a b
      |> Result.bind (optionDefault koncept)
      |> Result.map Some
       
let fMap f a koncept =
      f a 
      |> Result.bind (optionDefault koncept)
      |> Result.map Some


type MapAction = 
   | Delete 
   | Map of Koncept
   | Ignore
   
let mCube (f:HyperCube -> DimensionalKoncept List-> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.Cube (hc,koncepts) ->
        f hc koncepts
      //   |> Result.bind (optionKoncept koncept)
    | _-> koncept |> Some |> Ok 


let mAbstractKoncept (f:AbstractKoncept -> Koncept List -> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.AbstractKoncept (ak,koncepts) ->
        f ak koncepts
        // |> Result.bind (optionKoncept koncept) 
    | _-> koncept |> Some |> Ok 
 

let mValueKoncept (f:ValueKoncept -> Result<Koncept option,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.ValueKoncept vk ->
        f vk
      //   |> Result.bind (optionKoncept koncept) 
    | _-> koncept |> Some |> Ok 


let mapAbstractKoncept (f:AbstractKoncept -> Koncept List -> Result<Koncept option,_>) (koncept: Koncept)  =
   mAbstractKoncept (fun a b -> (fMap2 f a b koncept)) koncept

let mapCube (f:HyperCube -> DimensionalKoncept List -> Result<Koncept option,_>) (koncept: Koncept)  =
   mCube (fun a b -> (fMap2 f a b koncept)) koncept

let mapValue (f:ValueKoncept -> Result<Koncept option,_>) (koncept: Koncept)  =
   mValueKoncept (fun a  -> (fMap f a koncept)) koncept

let deleteValue (f:ValueKoncept -> Result<Koncept option,_>) =
   mValueKoncept f
   
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
    >> Result.map Some
    >> Result.mapError (fun err -> sprintf "%A" err)
    // |> Result.mapError (fun err -> sprintf "%A" err)
let deleteCube (hk:HyperCube) koncepts = 
   if hk.Name = HyperCubeName "Kvartal och annat" then
      Ok None
   else
       Cube (hk,koncepts) |> Some |> Ok



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
    >> Result.mapError (fun err -> sprintf "%A" err)
    >> Result.map Some

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
    >> Result.map Some
let t = mapCube deleteCube
let added6 = Koncept.map addValue  added5    
let added7 = Koncept.map t  added6  
