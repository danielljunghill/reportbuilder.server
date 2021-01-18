
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

type DefaultMember = DefaultMember of string  
type Dimension =
     | DimensionWithDefault of DefaultMember * Domain
     | DimensionWithoutDefault of Domain

type HyperDimension =
     | Opened of Dimension
     | Closed of Dimension


type HyperCubeName = string
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
        | DimensionalKoncept.AbstractKoncept (ak, d) -> (ak, d @ [ (vk |> DimensionalKoncept.ValueKoncept) ]) |> Ok
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
        | DimensionalKoncept.AbstractKoncept (ak, koncepts) -> (ak, koncepts @ [  DimensionalKoncept.AbstractKoncept (ak, []) ]) |> Ok
        | DimensionalKoncept.ValueKoncept _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)
    
module Koncept =

    let createAbstract name =
        (AbstractKoncept.create name, []) |> Koncept.AbstractKoncept
    let createValue =
        ValueKoncept.create >> Koncept.ValueKoncept
    type ParentKoncept = ParentKoncept of Koncept
    let add koncept (ParentKoncept parent) =
        match parent with
        | AbstractKoncept  (ak, koncepts) ->  (ak , koncepts @ [ koncept]) |> AbstractKoncept |> Ok
        | _ -> Error (sprintf "%A koncept cannot be added to %A" koncept parent)

    let rec map (parent: Result<Koncept Option, String>) (koncept: Koncept) : Result<Koncept Option, String> =
        match koncept with
        | Koncept.AbstractKoncept (ak, koncepts) ->
            let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> Some |> Ok
            let accKoncept = koncepts |> List.fold map newKoncept 
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

        | Koncept.Cube _ ->
            Error "Handling of Cube not implemented"

//  open Koncept
//  open AbstractKoncept
//  open ValueKoncept

let a1 = Koncept.createAbstract "Head abstract"
let v1 = Koncept.createValue "First Values"
let added = a1 |> Koncept.ParentKoncept |> Koncept.add v1
let added2 = v1 |> Koncept.ParentKoncept |> Koncept.add v1 

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