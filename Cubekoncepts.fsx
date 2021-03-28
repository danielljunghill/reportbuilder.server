open System
#load "Model.fs"
#load "Area.fs"
open Area
open Model
open Model.NList


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

// calculateSpan: DimensionalKoncept -> List Int
let calculateSpanForIndentedKoncepts dimKoncept =
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



let calculateIndentedRows (koncepts: DimensionalKoncept List)=
   let vs1 = 1 |> Start |> VerticalStart
   let hs1 = 1 |> Start |> HorizontalStart
   let maxSpan = 
    koncepts 
    |> List.collect calculateSpanForIndentedKoncepts 
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

// let rec depth (dimensionalKoncept: DimensionalKoncept) =
//    match dimensionalKoncept with
//    | DimensionalAbstract (_, koncepts) -> 
//         let rec calc koncepts =
//            match koncepts with
//            | [] -> [1]
//            | head :: tail ->
//                depth head @ calc tail
//         calc koncepts
//    | DimensionalValue _ -> [ 1 ]



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


let verticalStartAtLeastOne area =
      if area |> Area.verticalStart = Start 0 then 
         area |> Area.setVerticalStart (Start 1)
      else area
let horizontalStartAtLeastOne area = 
      if area |> Area.horizontalStart = Start 0 then 
         area |> Area.setHorizontalStart (Start 1)
      else area
let startAtLeastOne = verticalStartAtLeastOne >> horizontalStartAtLeastOne

let createAreaForKoncept (direction: Direction) span area  =
         let newArea =
            match direction with
            | Horizontal  -> 
                //vertical start = span + area start  
               //add + 1 to horizontal
               //set vertical span
               let prevStart = Area.verticalStart area
               let prevSpan = Area.verticalSpan area
               area 
               |> Area.setVerticalStart (startSpan (fun a b -> (a + b) |> Start) prevStart prevSpan)
               |> Area.setVerticalSpan span
               |> Area.setHorizontalSpan (Span 1)
            | Vertical ->  
               let prevStart = Area.horizontalStart area
               let prevSpan = Area.horizontalSpan area
               area 
               |> Area.setHorizontalStart (startSpan (fun a b -> (a + b) |> Start) prevStart prevSpan)
               |> Area.setHorizontalSpan span
               |> Area.setVerticalSpan (Span 1)
         let newArea2 = newArea |> startAtLeastOne   
         printfn "New Area: %A" newArea2
         newArea2
         
let calculateSubLevelArea (direction: Direction) area kontext =
   let span = kontext |> calculateSpanForKonceptTree |> Span

   match direction with
   | Horizontal  -> 
      //add + 1 to horizontal
      //set vertical span

       area |> Area.incrementHorizontalStart |> Area.setVerticalSpan span 
   | Vertical ->  
       area |> Area.incrementVerticalStart |> Area.setHorizontalSpan span
          //calculate new area 
   |> startAtLeastOne

let calculateValueArea direction area =
   match direction with
   | Horizontal  -> 
       area |> Area.incrementVerticalStart
   | Vertical ->  
       area |> Area.incrementHorizontalStart
          //calculate new area 
   |> startAtLeastOne


let rec calculateKontextAsTree (direction: Direction) (pa: Area) (kontexts: DimensionalKoncept list) =
   match kontexts with
   | [] -> []
   | head :: tail ->
      
      let rec kontextHeader (area: Area) (kontext: DimensionalKoncept) =    
          match kontext with
          | DimensionalAbstract (ak, koncepts) ->
               [  createAbstractRow area ak   ]
               @
               match koncepts with
               | [] -> []
               | head :: tail ->
                  //calculate pa för head
                  let area = calculateSubLevelArea direction area head
                  kontextHeader area head
                  //calculate pa for tail
                  @ calculateKontextAsTree direction area tail
          | DimensionalValue vk ->
                [ createValueRow (calculateValueArea direction area) vk ]
          
      
      let span = head |> calculateSpanForKonceptTree |> Span
      let newArea =  createAreaForKoncept direction span pa
      printfn "newArea %A Header %A" newArea head
      kontextHeader newArea head
      //calculate pa for tail
      @ calculateKontextAsTree direction newArea tail



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



let rows = calculateIndentedRows [testKoncept]

let rowsTree = calculateKontextAsTree Direction.Horizontal Area.emptyArea [testKoncept1]

let count = calculateSpanForKonceptTree testKoncept

type CubeRowsOffset = CubeRowsOffset of Offset
type CubeRows =
    {
        Rows: KonceptRow List
        Offset: CubeRowsOffset
    }

let indentetRows dimensionals =
    {
        Rows = calculateIndentedRows dimensionals
        Offset =
            { 
                 VerticalStart = 0 |> Start |> VerticalStart 
                 HorizontalStart = 1 |> Start |> HorizontalStart 
            } |> CubeRowsOffset
    }

