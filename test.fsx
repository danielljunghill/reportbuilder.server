

#load "koncept.fs"
#load "Hypercube.fs"

open System


open ReportDefinition
open Koncepts
open Dimensions
// open Koncept.AbstractKoncept
// open Koncept.ValueKoncept
let mix f a b = f b a

let nameToVk = ValueKoncept.create >> valueKonceptToKoncept
let nameToAk = AbstractKoncept.create >> abstractKonceptToKoncept

let v1 = add (ParentKoncept (nameToAk "First Abstract" )) (nameToVk "First Value")
let v2 = add (ParentKoncept (nameToAk "Second Abstract" )) (nameToVk "Second Value")
let top = add (ParentKoncept (nameToAk "Top Abstract" )) (nameToVk "Top Value")

let add2 = mix add
let r = top 
        |> ParentKoncept
        |> add2 v1
        |> ParentKoncept
        |> add2 v2

let rCopy = copy None r

let r2 = map (fun ki -> { ki with Name = "Bajskorv"}) r


type DimensionalValue =
  {
      Koncept: ValueKoncept
      Dimensions: HyperDimension List
  }

type DimensionalKoncept =
    | Cube of HyperCube * Koncept List
    | Abstract of AbstractKoncept * DimensionalKoncept List
    | Value of ValueKoncept


// -- type SelectedKoncept = SelectedKoncept Koncept


 
// selectSingleKoncept: KonceptInformation -> (KonceptInformation -> KonceptInformation)
// selectSingleKoncept info =
//     let
//        selectInfo: KonceptInformation -> KonceptInformation -> KonceptInformation
//        selectInfo sk ki =   { ki | selected = (sk.id == ki.id) }  
//     in
//        selectInfo info  


// select: KonceptInformation -> Koncept -> Maybe Koncept
// select ki = mapInfo (selectSingleKoncept ki) 


// type alias DimensionalValue =
//   {
//         koncept: ValueKoncept
//       , dimensions: List HyperDimension
//   }

// type DimensionalKoncept =
//     Cube (HyperCube, List Koncept)
//     |  Abstract (AbstractKoncept, List DimensionalKoncept)
//     |  SingleValue ValueKoncept