namespace ReportDefinition
module DimensionalKoncept =

    open Koncepts
    open Dimensions

    type DimensionalValue =
      {
          Koncept: ValueKoncept
          Dimensions: HyperDimension List
      }

    type DimensionalKoncept =
        | Cube of HyperCube * Koncept List
        | Abstract of AbstractKoncept * DimensionalKoncept List
        | Value of ValueKoncept


    
