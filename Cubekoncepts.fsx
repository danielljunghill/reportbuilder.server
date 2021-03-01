open System
#load "Model.fs"
#load "Area.fs"
open Area
open Model
open Model.NList


// calculateSpan: DimensionalKoncept -> List Int
let calculateSpan dimKoncept =
    //   recCalcForOne: Int -> DimensionalKoncept -> List Int
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
    recCalcForOne 0 dimKoncept

type KonceptRowItem =
   | AbstractRow of AbstractKoncept
   | ValueRow of ValueKoncept

type  KonceptRow = 
   {
         Area: Area
         Item: KonceptRowItem
   }

// createAbstractRow: Area -> AbstractKoncept -> KonceptRow
let createAbstractRow area item =
   {
            Area = area
            Item = AbstractRow item  
   }
// createValueRow: Area -> ValueKoncept -> KonceptRow
let createValueRow area item =
      {
            Area = area
            Item = ValueRow item  
      }



let calculateRows (koncepts: DimensionalKoncept List)=
   let vs1 = 1 |> Start |> VerticalStart
   let hs1 = 1 |> Start |> HorizontalStart
   let maxSpan = 
    koncepts 
    |> List.collect calculateSpan 
    |> List.max

   let area vStart hStart  = 
      { 
          HorizontalStart =  hStart
          HorizontalSpan = (maxSpan - (Area.horizontalStartToInt hStart) + 1) |> Span |> HorizontalSpan
          VerticalSpan = 1 |> Span |> VerticalSpan
          VerticalStart = vStart
      }
        //   VerticalLine { Start = y; Span = Span 1}; HorizontalLine = HorizontalLine { Start = x ; Span = Span (maxSpan - (Start.value x) + 1) } }
 
   let rec konceptHeader hStart (koncept: DimensionalKoncept) =
      match koncept with
      | DimensionalAbstract (ak, koncepts) -> 
            [  
                createAbstractRow (area vs1 hStart) ak
            ] @ (koncepts |> List.collect (konceptHeader (Area.horizontalStartMap startIncrement hStart)) )
      | DimensionalValue vk -> 
            [
              createValueRow (area vs1 hStart) vk
                
            ] 
   koncepts |> List.collect (konceptHeader hs1)
   |> List.mapi (fun i (item: KonceptRow) -> { item with Area = item.Area |> Area.setVerticalStart (Start (i + 1)) })

let rec depth (dimensionalKoncept: DimensionalKoncept) =
   match dimensionalKoncept with
   | DimensionalAbstract (_, koncepts) -> 
        let rec calc koncepts =
           match koncepts with
           | [] -> [1]
           | head :: tail ->
               depth head @ calc tail
        calc koncepts
   | DimensionalValue _ -> [ 1 ]



let ca name koncepts =  DimensionalAbstract ((name|> createAbstractKoncept), koncepts)
let ca1 name koncept =  DimensionalAbstract ((name|> createAbstractKoncept), [ koncept ])
let cv name = DimensionalValue (name |> createValueKoncept )
let ca2 name =  ca name []
let empty = ca "e" []
let t = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [] )))
let t' = ca1 "1" (ca1 "2" (ca1 "3" (ca "4" [t ] )))
let t2 = ca "head" [ ca2 "first"  ; ca2 "second"; cv "third"]
let t3 = ca "head" [ t2 ]
let testKoncept = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept , [ "Value1" |> createValueKoncept |> DimensionalValue ]); ])
let testKoncept1 = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept , [ "Value1" |>  createValueKoncept |> DimensionalValue;testKoncept ;testKoncept]); ])
let testKoncept2 = 
    DimensionalAbstract ("Abstract1"|> createAbstractKoncept, [DimensionalAbstract ("Abstract2"|> createAbstractKoncept , [ "Value1" |>  createValueKoncept |> DimensionalValue ; testKoncept1]); ])



let rows = calculateRows [testKoncept]