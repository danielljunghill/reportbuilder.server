open System

// type ValueKonceptId = | ValueKonceptId of Guid

// type DimensionalAbstractKonceptId = | AbstractKonceptId of Guid

// type HyperCubeId = | HyperCubeId of Guid

// type DimensionalKonceptId =
//     | ValueId of ValueKonceptId
//     | AbstractId of DimensionalAbstractKonceptId

// type HyperCube =
//     {
//         Koncepts: DimensionalKonceptId List
//         Id: HyperCubeId
//     }

// type AbstractKonceptId = | AbstractKonceptId of Guid

// type KonceptId =
//     | Value of ValueKonceptId
//     | Abstract of AbstractKonceptId
//     | HyperCube of HyperCubeId

// type AbstractKoncept =
//     {
//         Id: AbstractKonceptId
//         Name: string
//         Koncepts: KonceptId list
//     }

// type DimensionalAbstractKoncept =
//     {
//         Id : DimensionalAbstractKonceptId
//         Name: string
//         Koncepts: DimensionalKonceptId List
//     }

// type Koncepts =
//     {
//         Values: KonceptId List
//     }


module DataModel =

    type PageId = | PageId of Guid
    type Page =
        {
            Name: string
            Id: PageId
        }

    type AbstractKonceptId = AbstractKonceptId of Guid

       
    type AbstractKonceptParentId =
        | Page of PageId
        | Abstract of AbstractKonceptId

    type AbstractKoncept =
        {
            Id: AbstractKonceptId
            Name: string
            Parent: AbstractKonceptParentId
        }


    type HyperCubeId = HyperCubeId of Guid
    type HyperCube =
        {
            Id: HyperCubeId
            Parent: AbstractKonceptParentId
            Name: string
        }

    type DimensionalAbstractKonceptId = DimensionalAbstractKonceptId of Guid
    type DimensionalAbstractKonceptParentId =
        | Cube of HyperCubeId
        | DimensionalAbstract of DimensionalAbstractKonceptId

    type DimensionalAbstractKoncept =
        {
           Id: DimensionalAbstractKonceptId
           Name: string
           Parent: DimensionalAbstractKonceptParentId
        }

    type ValueKonceptParentId =
        | DimensionalParent of DimensionalAbstractKonceptParentId
        | AbstractParent of AbstractKonceptParentId

    type ValueKonceptId = ValueKonceptId of Guid
    type ValueKoncept =
        {
           Parent: ValueKonceptParentId
           Name: string
           Id: ValueKonceptId
        }

    module AbstractKoncept =
        let createId() = Guid.NewGuid() |> AbstractKonceptId
        let toAbstractKonceptParentId (a: AbstractKoncept) =
            Abstract a.Id
        let createWithPageParent name (page: Page) : AbstractKoncept =
            {
               Id = createId()
               Name = name
               Parent = Page page.Id
            }
        let create name (ak: AbstractKoncept) : AbstractKoncept =
            {
               Id = createId()
               Name = name
               Parent = Abstract ak.Id
            }


    module ValueKoncept =
        let createId() = Guid.NewGuid() |> ValueKonceptId
        let create name (a: AbstractKoncept) =
            { 
                Id = createId() 
                Parent = a |> AbstractKoncept.toAbstractKonceptParentId  |> AbstractParent
                Name = name
            } 
    module Page =
        let createId() =  Guid.NewGuid() |> PageId
        let toAbstractKonceptParentId (p: Page) =
            Page p.Id
        let create name = 
            { 
                Name = name
                Id = createId()
            }  

open System

let map = new Map<string,string List>([("damien",["damien"])])


let table = map|> Map.add "damien" ["Kuk"]
let r1 = table|> Map.tryFind "damien"
let r2 = map|> Map.tryFind "damien"

let add key (value: string) (table: Map<string,string List>) =
    match table|> Map.tryFind key with
    | Some v -> table |> Map.add key (v @ [value])
    | None -> table |> Map.add key [ value ]

let r3  = add "damien" "petoman" map 
let r4 = add "lars" "flatulist" r3
module Table =
    open DataModel
    type Table<'b,'a when 'a: comparison> = | Table of Map<'a,'b List>

    module Table =

        let add key value (table: Table<_,_>)=
            let (Table t) = table
            match t |> Map.tryFind key with
            | Some v -> t |> Map.add key (v @ [value])
            | None -> t |> Map.add key [ value ]
        let tryGet key (table: Table<_,_>)= 
            let (Table t) = table
            t|> Map.tryFind key 
        let remove key value (table: Table<_,_>) =
            let (Table t) = table
            match t |> Map.tryFind key with
            | Some valueList ->
                 match valueList  |> List.filter (fun v -> v = value) with
                 | [] -> t |> Map.remove key
                 | ls -> t |> Map.add key ls
            | None -> t
            |> Table
        let init<'value,'key when 'key: comparison>() =  new Map<'key,'value List>([]) |> Table 
        // let values (table: Table<_,_>) =
        //     let (Table t) = table 
        //     Map.toList t
        //     |> List.map (fun (a,b) -> a)
        //     |> List.concat
            //|> List.reduce

open Table
open DataModel



type TableValueKoncepts = 
    {
        Parents: Table<ValueKonceptId,ValueKonceptParentId>
        Values: Map<ValueKonceptId, ValueKoncept * ValueKonceptParentId>
    }



module TableValueKoncepts = 
    //     let add key value (table: TableValueKoncepts) =
    //         table |> Table.add key value
   let init = 
    {
        Parents= Table.init<ValueKonceptId,ValueKonceptParentId>()
        Values= new Map<ValueKonceptId, ValueKoncept * ValueKonceptParentId>(Seq.empty)
    }

    let addToPage (valueKoncept:ValueKoncept) (page:Page) (table:TableValueKoncepts) =
        let paentId = page.Id |> AbstractKonceptParentId.Page |> ValueKonceptParentId.AbstractParent 
        match table.Values.ContainsKey valueKoncept.Id with
        | true -> 
            let k, pid = table.Values.Item(valueKoncept.Id)
            Table.remove pid k.Id table.Parents
            |> Table.add paentId k.Id table.Parents
        | false ->

    



type TableAbstractKoncept = Table<AbstractKoncept, AbstractKonceptParentId>
module TableAbstractKoncept =
    let init = Table.init<AbstractKoncept,AbstractKonceptParentId>

    let addWithPageParent name page (table: TableAbstractKoncept) =
        let a = AbstractKoncept.createWithPageParent name page
        table |> Table.add a.Parent a 
        |> fun table -> a, table

    let addWithAbstractKonceptParent name ak (table: TableAbstractKoncept) =
        let a = AbstractKoncept.create name ak
        table 
        |> Table.add a.Parent a
        |> fun table -> a, table 


module Page =
    let addAbstractKoncept name page (table: TableAbstractKoncept) =
        let a = AbstractKoncept.create name page
        table |> Table.add a.Parent a 

let fp = Page.create "First"

let pg = [ Page.create "First"]

let tv = TableValueKoncepts.init()  

let ta = TableAbstractKoncept.init()

let u, t = TableAbstractKoncept.addWithPageParent "Head Abstract Koncept" fp ta

type Model =
    {
        Pages: Page List
        Abstracts: TableAbstractKoncept
        Koncepts: TableValueKoncepts     
    }

module Model =
    let init() =
        { 
            Pages = []
            Abstracts = TableAbstractKoncept.init()
            Koncepts = TableValueKoncepts.init()
        }

    let addPage (page:Page) (model:Model) =
        match model.Pages |> List.tryFind (fun p -> p.Id = page.Id) with
        | Some p ->  sprintf "Page %A cannot be added since it aldready exists" page |> Error
        | None -> { model with Pages = model.Pages @ [page]} |> Ok

    let addAbstractKoncept (child:AbstractKoncept) (parent: AbstractKoncept) = 



    // type ValueKonceptId = ValueKonceptId of Guid
    // type ValueKonceptName = KonceptName of String
    // type ValueKoncept =
    //     {
    //         Name: ValueKonceptName
    //         Id: ValueKonceptId
    //     }


    // type AbstractKonceptId = AbstractKonceptId of Guid
    // type AbstractKonceptName = AbstractKonceptName of String
    // type AbstractKoncept =
    //     {
    //         Name: ValueKonceptName
    //         Id: ValueKonceptId
    //     }

    // type Koncept =
    //     | Value of ValueKoncept
    //     | 




    // {
        //     Map: Map<ValueKonceptParentId,ValueKoncept List>
        // }
    // module MapValueKoncepts =
    //     let create values =
    //         { Map = new Map<ValueKonceptParentId,ValueKoncept List>(values) }
    //     module Page =
    //         let trygGet(pageId:PageId) (koncepts:MapValueKoncepts) =
    //              pageId |> Page |> AbstractParent |> koncepts.Map.TryFind
    //         let add v koncepts (koncepts:MapValueKoncepts) =

    //     module AbstractKoncept =
    //         let tryGet (abstractKonceptId:AbstractKonceptId) (koncepts:MapValueKoncepts) =
    //          abstractKonceptId |> Abstract |> AbstractParent |> koncepts.Map.TryFind


    // type MapAbstractKoncepts=
    //     {
    //         Map: Map<AbstractKonceptParentId,AbstractKoncept>
    //     }

    // type MapHyperCubes =
    //     {
    //        Map: Map<AbstractKonceptParentId,HyperCubeId>
    //     }

    // type MapDimensionalAbstractKoncept =
    //     {
    //         Map: Map<DimensionalAbstractKonceptParentId, DimensionalAbstractKoncept>
    //     }




















