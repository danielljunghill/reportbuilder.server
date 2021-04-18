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
           indent: Indent option
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




let rec calculateKontextAsTree  (parent: CubeAbstractRow option) (kontexts: DimensionalKoncept list) =
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
                  @ calculateKontextAsTree state tail
          | DimensionalValue vk ->
                [ 
                    vk 
                    |> CubeAbstractRow.foldValue state 
                    |> ValueRow
                   ]


      let span = head |> calculateSpanForKonceptTree |> Span
      let newArea =  createAreaForKoncept direction span pa
      printfn "newArea %A Header %A" newArea head
      kontextHeader newArea head
      //calculate pa for tail
      @ calculateKontextAsTree direction newArea tail



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
