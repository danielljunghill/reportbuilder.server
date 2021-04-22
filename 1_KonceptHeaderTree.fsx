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

type Row = Row of int
type Column = Column of int
type ColSpan = ColSpan of int
type RowSpan = RowSpan of int
type Orientation =
    | RowColumn
    | ColumnRow
type Area =
    {
        Row: Row
        Column: Column
        ColSpan: ColSpan
        RowSpan : RowSpan
        Orientation: Orientation
    }
type CubeHeader  =
    {
           Column: Start
           ColumnSpan: Span
           Row: Start
           RowSpan: Span
           Attributes:  String List
           Name: String
           IsSelected: bool
           Indent: Indent option
    }


let rowSpanTree  =
    let rec rowSpan (koncept: DimensionalKoncept) =
       match koncept with
       | DimensionalAbstract (_, koncepts) ->
               if koncepts.IsEmpty then  1
               else
                  let rec rowSpan' koncepts =
                      match koncepts with
                      | [] -> 0
                      | head :: tail ->
                         rowSpan head
                         + rowSpan' tail
                  rowSpan' koncepts
       | DimensionalValue _ -> 1
    rowSpan >> RowSpan



let colSpanTree  =
      let rec recCalcForOne stateOne koncept =      
          let newStateOne = stateOne + 1
          match koncept with
             | DimensionalAbstract (_,childKoncepts) ->
                  //  recCalcForMany: Int -> List DimensionalKoncept -> List Int
                  let rec recCalcForMany stateMany koncepts =
                      match koncepts with
                         | [] -> [ stateMany ]
                         | head :: tail ->  (recCalcForOne stateMany head) @ recCalcForMany stateMany tail                
                  recCalcForMany newStateOne childKoncepts
             | DimensionalValue _ -> [ newStateOne ] 
      (recCalcForOne 0) >>  List.max >> ColSpan
     

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

//Korrekt cube rows
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


let rec calculateRowSpan2 (kontexts: DimensionalKoncept list) =
   match kontexts with
   | [] -> 0
   | head :: tail ->

      let rec kontextHeader (kontext: DimensionalKoncept) =
          match kontext with
          | DimensionalAbstract (ak, koncepts) ->

            //    [ CubeRow.abstractRow ak ]
            //    @
               match koncepts with
               | [] -> 1 
               | head :: tail ->
                  //calculate pa för head
                //   let area = calculateSubLevelArea direction area head
                  kontextHeader  head
                  //calculate pa for tail
                  + (calculateRowSpan2 tail)
          | DimensionalValue vk ->
                 1 

      kontextHeader  head
      + calculateRowSpan2 tail


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


let rows1 =  calculateCubeRowsForTree None [ testKoncept1 ] 
List.length rows1
//korrekt

let areaOne =
    {
        Row = Row 1
        Column = Column 1
        ColSpan = ColSpan 1
        RowSpan = RowSpan  1
        Orientation = RowColumn
    }

let rows = calculateRowSpan2 [  cv "Bajskorv" ]
let colSpan = colSpanTree empty 
let rowSpan = "Bajskorv" |> cv |> rowSpanTree   
let colSpanInt (ColSpan span) = span
let incCol (Column column) =
    column + 1
    |> Column

let incRow (Row row) =
    row + 1
    |> Row

let addRowSpanToRow (RowSpan span) (Row row) =
    span + row
    |> Row
type CubeHeaderNew =
    {
        Area: Area
        Name: String
    }

let cubeHeadersForTree (kontexts: DimensionalKoncept list) =
    let maxColSpan =
        kontexts
        |> List.map colSpanTree
        |> List.maxBy colSpanInt

    let rec cubeHeaders (area: Area) (kontexts: DimensionalKoncept list) = 
       match kontexts with
       | [] -> []
       | head :: tail ->
            let rowSpan = rowSpanTree head
            let rec cubeHeaders' area (kontexts: DimensionalKoncept) =
                //rowspan for kontext
                
                let abstractKontextArea : Area = { area with RowSpan = rowSpan}
                match kontexts with
                | DimensionalAbstract (ak, koncepts) ->
                   [{ 
                       Area = abstractKontextArea 
                       Name = abstractKonceptNameToString ak.Name 
                   }]
                   @
                   match koncepts with
                   | [] -> []
                   | head :: tail ->
                        cubeHeaders' ({ area with Column = incCol area.Column }) head
                        @ cubeHeaders ({ area with Column = incCol area.Column ; Row = incRow area.Row }) tail

                | DimensionalValue vk ->
                   [{ 
                       Area = area 
                       Name = valueKonceptNameToString vk.Name 
                   }]

            cubeHeaders' area head 
            @ cubeHeaders ({area with Row = area.Row |> addRowSpanToRow rowSpan}) tail
    cubeHeaders areaOne kontexts

let maxColSpan = cubeHeadersForTree [ testKoncept ; testKoncept1 ; testKoncept2 ]


// let rec calculateCubeHeaderForTree (row: Start) (column: Start) (span: Span) (kontexts: DimensionalKoncept list) =


//    match kontexts with
//    | [] -> []
//    | head :: tail ->

//       let rec cubeHeader (row: Start) (column: Start) (spanRest: Span) (kontext: DimensionalKoncept) =
//           match kontext with
//           | DimensionalAbstract (ak, koncepts) ->

               
//                //skall ha row som ovan och rowspan beroende på om det finns underligganden
//                //skall ha colspan = 1 om inga barn annasrs= barn
//                [ createCubeHeader]           
//                @
//                match koncepts with
//                | [] -> []
//                | head :: tail ->
//                   //calculate pa för head
//                 //   let area = calculateSubLevelArea direction area head
//                   cubeHeader (newRow row) (newColumn column) newSpan head
//                   //calculate pa for tail
//                   @ calculateCubeRowsForTree state tail
//           | DimensionalValue vk ->
//                 [ 
//                     vk 
//                     |> CubeAbstractRow.foldValue state 
//                     |> ValueRow   
//                    ]

//       kontextHeader parent head
//       @ calculateCubeRowsForTree parent tail

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
let vv = [ 7 ; 4; 9; 12; 3; 5; 1; 55; 2; 2]

let rec order state1 m =
    match m with
    | [] -> state1
    | head :: tail ->
        let rec orderOne state n =
            match n with
            | [] -> state @ [ head ]
            | x :: rest -> 
                if head < x then state @ [ head ] @ n else orderOne (state @ [ x ]) rest
        let newState = orderOne [] state1
        printfn "%A" newState
        order newState tail
let tk: int list = []
let vs: int list = order tk vv