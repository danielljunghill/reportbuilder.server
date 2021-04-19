open System
#load "Area.fs"
#load "Model.fs"

open Area
open Model
open Model.NList


type Indent = Indent of int
type  CubeRowContext =
    {
            Abstracts:  AbstractKoncept List
            Members:  Member List
    }

module CubeRowContext =
    let empty =
        {
            Abstracts = []
            Members = []
        }

    let addMembers members context  : CubeRowContext=
        { context with Members = context.Members @ members }

    let addAbstract ak context   =
        { context with Abstracts = context.Abstracts @ [ ak ] }

type CubeValueRow = CubeValueRow of (ValueKoncept*CubeRowContext)

// module CubeValueRow =
//     let create vk = (vk, CubeRowContext.empty) |> CubeValueRow
//     let 
type CubeAbstractRow = CubeAbstractRow of (AbstractKoncept*CubeRowContext)
module CubeAbstractRow =
    let create ak = CubeAbstractRow (ak,CubeRowContext.empty)

    let addAbstractKoncept (CubeAbstractRow (ak,context))  abstractKoncept =
        CubeAbstractRow (abstractKoncept, context |> CubeRowContext.addAbstract ak)

    let addValueKoncept (CubeAbstractRow (ak,context)) vk =
        let newContext = 
            context 
            |> CubeRowContext.addAbstract ak
        CubeValueRow (vk,newContext)
           
    let foldAbstract (state: CubeAbstractRow option) (koncept: AbstractKoncept) =
        match state with
        | Some row ->
             addAbstractKoncept row koncept
        | None ->
            create koncept
    
    let foldValue (state: CubeAbstractRow option) (koncept: ValueKoncept) =
        match state with
        | Some row ->
             addValueKoncept row koncept
        | None ->
            CubeValueRow (koncept, CubeRowContext.empty)


type CubeRow =
    | AbstractRow of CubeAbstractRow
    | ValueRow of CubeValueRow

module CubeRow =
    let abstractRow ak =
        (ak,CubeRowContext.empty)
        |> CubeAbstractRow
        |> AbstractRow

    let valueRow vk =
        (vk,CubeRowContext.empty)
        |> CubeAbstractRow
        |> AbstractRow
// type Members = Members of Member NList

type CubeHeader  =
    {
           Column: Start
           ColumnSpan: Span
           Row: Start
           RowSpan: Span
           Attributes:  String List
           Name: String
           IsSelected: bool
           NotImplementedException: Indent option
    }

let rec calculateSpanForKonceptTree (koncept: DimensionalKoncept) =
   match koncept with
   | DimensionalAbstract (_, koncepts) ->
           if koncepts.IsEmpty then  1

           else
              let rec calculateSpanForKoncepts koncepts =
                  match koncepts with
                  | [] -> 0
                  | head :: tail ->
                     calculateSpanForKonceptTree head
                     + calculateSpanForKoncepts tail
              calculateSpanForKoncepts koncepts
   | DimensionalValue _ -> 1




let createHeader (columSpan: HorizontalSpan)  (column: HorizontalStart) (rowSpan: VerticalSpan) (row: VerticalStart) () (name: String) =
    {
           Column = horizontalStartToStart column
           ColumnSpan = horizontalSpanToSpan columSpan
           Row =  verticalStartToStart row
           RowSpan = verticalSpanToSpan rowSpan
           Attributes = []
           Name = name
           IsSelected = false
           Indent = None
    }

let rec calculateCubeRowsForTree (parent: CubeAbstractRow option) (kontexts: DimensionalKoncept list) =
   match kontexts with
   | [] -> []
   | head :: tail ->

      let rec kontextHeader (state: CubeAbstractRow option) (kontext: DimensionalKoncept) =
          match kontext with
          | DimensionalAbstract (ak, koncepts) ->

               let abstractRow = ak |> CubeAbstractRow.foldAbstract state
            //    [ CubeRow.abstractRow ak ]
            //    @
               match koncepts with
               | [] -> [ abstractRow |> AbstractRow ]
               | head :: tail ->
                  //calculate pa för head
                //   let area = calculateSubLevelArea direction area head
                  kontextHeader (Some abstractRow) head
                  //calculate pa for tail
                  @ calculateCubeRowsForTree state tail
          | DimensionalValue vk ->
                [ 
                    vk 
                    |> CubeAbstractRow.foldValue state 
                    |> ValueRow   
                   ]

      kontextHeader parent head
      @ calculateCubeRowsForTree parent tail


module CubeHeader =
    let newAbstractChild  (ak: AbstractKoncept) (cubeHeader: CubeHeader) =
        //add columns + 1
        { cubeHeader with Name =  abstractKonceptNameToString ak.Name }

    let newAbstractSibling  (ak: AbstractKoncept) (cubeHeader: CubeHeader) =
        //add columns + 1
        { cubeHeader with Name =  abstractKonceptNameToString ak.Name; Row = Area.startIncrement cubeHeader.Row  }

    let newValueChild  (vk: ValueKoncept) (cubeHeader: CubeHeader) =
        //add columns + 1
        { cubeHeader with Name =  valueKonceptNameToString vk.Name }

    let newValueSibling  (vk: ValueKoncept) (cubeHeader: CubeHeader) =
        //add columns + 1
        { cubeHeader with Name =  valueKonceptNameToString vk.Name; Row = Area.startIncrement cubeHeader.Row  }

    let newHeader f defaultHeader (cubeHeader: CubeHeader option) =
        match cubeHeader with
        | Some header -> f defaultHeader cubeHeader
        | None -> defaultHeader

let rec calculateCubeHeaderForTree (parent: CubeHeader option) (kontexts: DimensionalKoncept list) =
   match kontexts with
   | [] -> []
   | head :: tail ->

      let rec kontextHeader (state: Start) (kontext: DimensionalKoncept) =
          match kontext with
          | DimensionalAbstract (ak, koncepts) ->

               let abstractRow = ak |> CubeAbstractRow.foldAbstract state
               [ CubeRow.abstractRow ak ]
               @
               match koncepts with
               | [] -> [ abstractRow |> AbstractRow ]
               | head :: tail ->
                  //calculate pa för head
                //   let area = calculateSubLevelArea direction area head
                  kontextHeader (Some abstractRow) head
                  //calculate pa for tail
                  @ calculateCubeRowsForTree state tail
          | DimensionalValue vk ->
                [ 
                    vk 
                    |> CubeAbstractRow.foldValue state 
                    |> ValueRow   
                   ]

      kontextHeader parent head
      @ calculateCubeRowsForTree parent tail

let ca name koncepts =  DimensionalAbstract ((name|> createAbstractKoncept), koncepts)
let ca1 name koncept =  DimensionalAbstract ((name|> createAbstractKoncept), [ koncept ])
let cv name = DimensionalValue (name |> createValueKoncept )
let ca2 name =  ca name []
let empty = ca "e" []
let t = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [] )))
let t' = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [t ] )))
let t2 = ca "head" [ ca2 "first"; ca2 "second"; cv "third"]
let t3 = ca "head" [ t2 ]
let testKoncept = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept  , [ "Value1" |> createValueKoncept |> DimensionalValue ]); ])
let testKoncept1 = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept , [ "Value1" |>  createValueKoncept |> DimensionalValue;testKoncept ;testKoncept]); ])
let testKoncept2 = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept , [ "Value1" |>  createValueKoncept |> DimensionalValue ; testKoncept1]); ])


let rows1 =  calculateCubeRowsForTree None [ testKoncept ; testKoncept2 ;[] |> ca "Abstract1X"  ] 
// let rec calculateKontextAsTree  (parent: CubeAbstractRow option) (kontexts: DimensionalKoncept list) =
//    match kontexts with
//    | [] -> []
//    | head :: tail ->

//       let rec kontextHeader (parent: CubeAbstractRow option) (kontext: DimensionalKoncept) =
//           match kontext with
//           | DimensionalAbstract (ak, koncepts) ->

//                addParent
//                [ CubeRow.abstractRow ak ]
//                @
//                match koncepts with
//                | [] -> []
//                | head :: tail ->
//                   //calculate pa för head
//                 //   let area = calculateSubLevelArea direction area head
//                   kontextHeader  head
//                   //calculate pa for tail
//                   @ calculateKontextAsTree  tail
//           | DimensionalValue vk ->
//                 [ createValueRow (calculateValueArea direction area) vk ]


//       let span = head |> calculateSpanForKonceptTree |> Span
//       let newArea =  createAreaForKoncept direction span pa
//       printfn "newArea %A Header %A" newArea head
//       kontextHeader newArea head
//       //calculate pa for tail
//       @ calculateKontextAsTree direction newArea tail
