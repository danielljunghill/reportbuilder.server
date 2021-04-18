open System
#load "Area.fs"
#load "Model.fs"

open Area
open Model
open Model.NList


// type Members = Members of Member NList  

// type MemberHeader = MemberHeader of (Member NList)
// Identity for Row or Column with Koncept
type KonceptValuePath =
   { 
      Value: ValueKoncept
      Abstracts: AbstractKoncept List
   }

type KonceptPath =
   | AbstractPath of  AbstractKoncept NList 
   | ValuePath of KonceptValuePath
 

type KonceptIdentity =
   {
      KonceptPath: KonceptPath    
      Members: Member List
   }

type MemberIdentity = MemberIdentity of (Member NList)
   
type HeaderIdentifier =
   | Factor of Factor
   | Sequence of SequenceId

type HeaderName = HeaderName of String

type Attribute = Attribute of String

type Header =
   {
      Span: Span
      Start: Start
      Depth: Depth
      Name: String
      Attributes: Attribute List 
      Identifiers: HeaderIdentifier NList
   }


// let axis = 
module KonceptMemberIdentity =
      open Area
      let fromAbstract (ak: AbstractKoncept)  =
         {
            KonceptPath = ak  |> NList.create |> AbstractPath
            Members = []
         }

      let fromValue (vk: ValueKoncept) =
         {
            KonceptPath = { Value = vk; Abstracts = [] } |> ValuePath
            Members = []
         }

      let addMembers (members: Member NList) (identity: KonceptIdentity) =
         { identity with Members = NList.toList members @ identity.Members}

      let addAbstractKoncept (ak: AbstractKoncept) (identity: KonceptIdentity) =
         let newPath =
               match identity.KonceptPath with
               | AbstractPath path -> 
                  NList.addList path [ ak ] 
                  |> AbstractPath
               | ValuePath path -> 
                  { path with Abstracts = [ ak ] @ path.Abstracts } 
                  |> ValuePath

         { identity with KonceptPath = newPath }
   
      let calculateSpan dimKoncept =
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

      let createIdentities (koncepts: DimensionalKoncept List) =
         let rec first koncept =
               match koncept with
               | DimensionalAbstract (ak, koncepts) -> 
                     [ 
                       fromAbstract ak 
                     ] @ (koncepts |> List.collect (rest ak))
               | DimensionalValue vk -> [ fromValue vk ]
         and rest (parent:AbstractKoncept) koncept =
               match koncept with
               | DimensionalAbstract (ak, koncepts) -> 
                     [ 
                        ak
                        |> fromAbstract  
                        |> addAbstractKoncept parent
                     ] @ (koncepts |> List.collect (rest ak))
               | DimensionalValue vk ->
                   [ 
                      fromValue vk
                      |> addAbstractKoncept parent
                   ]
         koncepts |> List.collect first

      let createHeaders  (koncepts: DimensionalKoncept List) =
         let incrementDepth (Depth depth) = 
            depth + 1
            |> Depth
         let incrementStart (Start start) =
            start + 1
            |> Start
         let calcSpan = 
            koncepts 
            |> List.collect calculateSpan 
            |> List.max  
         let rec createHeaders' depth start koncept =
               match koncept with
               | DimensionalAbstract (ak, koncepts) -> 
                     [ 
                        { Start = start ; Span = Span 1 ; Depth = depth; Identifiers = ak.SequenceId |> Sequence |> NList.create  ; Attributes = [] ; Name = abstractKonceptNameToString ak.Name}
                     ] @ (koncepts |> List.collect (createHeaders' (incrementDepth depth) (incrementStart start)))
               | DimensionalValue vk ->  [{ Start = start ; Span = Span 1 ; Depth = depth; Identifiers = vk.Factor |> Factor |> NList.create  ; Attributes = []; Name = valueKonceptNameToString vk.Name}]
   
         koncepts |> List.collect (createHeaders' (Depth 1) (Start 1))


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






// let rows = calculateSpan testKoncept
let identities = KonceptMemberIdentity.createIdentities [ testKoncept2]
let headers = KonceptMemberIdentity.createHeaders [ testKoncept2]
let max = headers |> List.maxBy (fun tblHeader ->  tblHeader.Depth |> depthInt)
max.Depth
// let indentetRows dimensionals =
//     
//         Rows = calculateIndentedRows dimensionals
//         Offset =
//             { 
//                  VerticalStart = 0 |> Start |> VerticalStart 
//                  HorizontalStart = 1 |> Start |> HorizontalStart 
//             } |> CubeRowsOffset
//     }


