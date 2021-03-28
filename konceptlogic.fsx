
open System
#load "Model.fs"

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
   let createAbstract name =
        (AbstractKoncept.create name, []) |> DimensionalKoncept.AbstractKoncept
   let createValue = ValueKoncept.create >> DimensionalKoncept.ValueKoncept
   let add (koncept: DimensionalKoncept) (ParentDimensionalKoncept parent) =
            match koncept with
            | DimensionalKoncept.AbstractKoncept (ak,koncepts) -> 
                (ak, koncepts @ [ koncept])
                 |> DimensionalKoncept.AbstractKoncept|> Ok
            | _ -> Error "cannot add"


   // let createValue =
   //      ValueKoncept.create >> DimensionalKoncept.ValueKoncept
 

   let maybeAdd (koncept: DimensionalKoncept option) (parent: ParentDimensionalKoncept option)  =
            match parent with
            | None -> koncept |> Ok
            | Some (ParentDimensionalKoncept pk) -> 
                match koncept with
                | Some child -> 
                    pk
                    |> ParentDimensionalKoncept
                    |> add child
                    |> Result.map Some
                | None ->  
                  pk |> Some |> Ok
     
   let andThenMaybeAddTo (parent: Result<ParentDimensionalKoncept option, string>) (koncept:DimensionalKoncept option) =
        Result.bind (maybeAdd koncept) parent
    
   let mapToParent (k:Result<DimensionalKoncept option, string>) =
         k |> Result.map (fun r -> r |> Option.map ParentDimensionalKoncept)

   let parentAsKoncept (parent: Result<ParentDimensionalKoncept option, string>) =
        parent |> Result.map (fun r -> r |> Option.map (fun (ParentDimensionalKoncept k) -> k))

   let rec recursiveMap f p k  =
            let fmap parent (koncept: DimensionalKoncept option)=
               match koncept with
               | Some k ->
                   match k with
                   | DimensionalKoncept.AbstractKoncept (ak, koncepts) ->
                       let newKoncept = (ak, []) |> DimensionalKoncept.AbstractKoncept |> ParentDimensionalKoncept |> Some |> Ok
                       let accKoncept = 
                           koncepts 
                           |> List.map f 
                           |> List.fold (recursiveMap f) newKoncept 
                           |> parentAsKoncept
                       accKoncept |> Result.bind (andThenMaybeAddTo parent)
                   | DimensionalKoncept.ValueKoncept (_) -> 
                     let newKoncept = f k
                     Result.bind (andThenMaybeAddTo parent) newKoncept
               | None -> parentAsKoncept parent
               |> mapToParent
            Result.bind (fmap p) k

   let map f m =
      
        recursiveMap f (Ok None) (m |> Result.map Some)
        |> parentAsKoncept
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

let rod defaultValue =
   Option.defaultValue defaultValue >> Ok
   
type MapAction<'a> = 
   | Delete 
   | NewValue of 'a
   | Ignore

type MapKonceptAction = MapAction<Koncept>

module MapKonceptAction =
   let asKonceptOption (koncept: Koncept) (action:MapKonceptAction) =
      match action with
      | Delete -> None
      | NewValue k -> Some k
      | Ignore -> Some koncept

let mapCube (f:HyperCube -> DimensionalKoncept List-> Result<MapKonceptAction,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.Cube (hc,koncepts) ->
        f hc koncepts 
        |> Result.map (MapKonceptAction.asKonceptOption koncept)
    | _-> koncept |> Some |> Ok 

let mapAbstractKoncept (f:AbstractKoncept -> Koncept List -> Result<MapKonceptAction,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.AbstractKoncept (ak,koncepts) ->
        f ak koncepts
        |> Result.map (MapKonceptAction.asKonceptOption koncept)
    | _-> koncept |> Some |> Ok 
 
let mapValueKoncept (f:ValueKoncept -> Result<MapKonceptAction,_>) (koncept: Koncept) =
    match koncept with
    | Koncept.ValueKoncept vk ->
        f vk
        |> Result.map (MapKonceptAction.asKonceptOption koncept)
    | _-> koncept |> Some |> Ok 
 
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
            |> MapKonceptAction.NewValue
        else
            MapKonceptAction.Ignore
        |> Ok
        
    mapAbstractKoncept f 
    >> Result.mapError (fun err -> sprintf "%A" err)

let deleteCube (hk:HyperCube) koncepts = 
   if hk.Name = HyperCubeName "Kvartal och annat" then
      Ok MapKonceptAction.Delete
   else
      Ok MapKonceptAction.Ignore

let added4 = Koncept.map addCube added3

let addDimensionalKoncept  =
    let dimKoncepts = [ DimensionalKoncept.createValue "Intäkter" ; DimensionalKoncept.createValue "Försäljning cyklar"; DimensionalKoncept.createValue "Bidrag" ]
    let f (hc: HyperCube) dimension =
        if hc.Name = HyperCubeName "Kvartal och annat" then
            (hc,dimension @ dimKoncepts) |> Koncept.Cube |> MapKonceptAction.NewValue
        else
            MapKonceptAction.Ignore 
        |> Ok
    mapCube f    
    >> Result.mapError (fun err -> sprintf "%A" err)

let added5 = Koncept.map addDimensionalKoncept added4

let addValue  =
    let f (ak:AbstractKoncept) koncepts =
       if ak.Name = AbstractKonceptName "Head abstract1" then
            (ak, koncepts @ [ "dagen d" |> ValueKoncept.create |> Koncept.ValueKoncept]) |> Koncept.AbstractKoncept |> MapKonceptAction.NewValue
       else
            MapKonceptAction.Ignore
       |> Ok
    mapAbstractKoncept f  

let t = mapCube deleteCube
let added6 = Koncept.map addValue  added5    
let added7 = Koncept.map t  added6  
