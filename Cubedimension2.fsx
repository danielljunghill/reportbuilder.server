// open System
// #load "Model.fs"
// #load "Area.fs"
// open Area
// open Model
// open Model.NList

// type  DimensionHeaderItem =
//    {

//          Area: Area
//          Members: Member NList 
//    }

// type DimensionHeader = DimensionHeader of DimensionHeaderItem

// let itemFromDimensionalHeader (DimensionHeader item) = item

// // createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
// let createDimensionHeaderItem area members m  =
//    {
//             Area = area
//             Members = NList.create2 m members

//    }

// // createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
// let createDimensionHeader a pm m  =
//    createDimensionHeaderItem a pm m 
//    |> DimensionHeader


// // --calculate span for dimensions  
// // -- Total span di
// // calculateSpan: Dimension -> Span -> Span
// let calculateSpan dimension (Span span)   =
//    match dimension with
//     |  DimensionWithDefault (_, domain) -> span / ((domain.Members |> NList.length) + 1) |> Span
//     |  DimensionWithoutDefault (domain) -> span / (domain.Members |> NList.length) |> Span

// // calculateStart: Int -> Span -> Start -> Start
// let calculateStart ordinal (Span span) (Start start)  =
//          start + span * ordinal
//          |> Start

// // calculateArea: Dimension -> Direction -> Area -> Int -> Area
// let calculateArea dimension direction area ordinal =
//    match direction with
//      | Horizontal ->
//             let hSpan = 
//                area.HorizontalSpan 
//                |> horizontalSpanToSpan 
//                |> calculateSpan dimension
//                |> HorizontalSpan

//             let hStart = 
//                area.HorizontalStart 
//                |> horizontalStartToStart
//                |> calculateStart ordinal  (hSpan |> horizontalSpanToSpan)
//                |> HorizontalStart

//             let vStart = 
//                area.VerticalStart 
//                |> verticalStartMap startIncrement 
//             { 
//                      VerticalStart = vStart
//                      VerticalSpan = 1 |> Span |> VerticalSpan
//                      HorizontalStart = hStart
//                      HorizontalSpan = hSpan
                 
//             }
//       | Vertical ->
//             let vSpan = 
//                area.VerticalSpan
//                |> verticalSpanToSpan
//                |> calculateSpan dimension
//                |> VerticalSpan

//             let vStart = 
//                area.VerticalStart
//                |> verticalStartToStart
//                |> calculateStart ordinal (vSpan |> verticalSpanToSpan)
//                |> VerticalStart

//             let hStart = 
//                area.HorizontalStart
//                |> horizontalStartMap startIncrement

//             { 
//                 VerticalStart = vStart
//                 VerticalSpan = vSpan
//                 HorizontalStart = hStart
//                 HorizontalSpan = 1 |> Span |> HorizontalSpan

//             }

// // fromDimension: Direction -> Area -> Dimension -> List Member -> NList DimensionHeader
// let fromDimension direction area dimension parentMembers =              
//     //   let
//     //      members: NList Member
//     let members = 
//             dimension
//             |> dimensionMembers 
//             |> NList.map (fun (DomainMember m) -> m)
         
//     // let  defaultMembers: List Member
//     let defaultMembers = 
//             dimension
//             |> memberDefault 
//             |> Option.map (fun (DefaultMember m) -> m)
//             |> Option.toList
            
//         //  memberHeaders: NList DimensionHeader
//     let memberHeaders = 
//             defaultMembers
//             |> NList.append members
//             |> NList.mapi (fun i m ->  m |> createDimensionHeader (calculateArea dimension direction area i) parentMembers)             
//       in
//          memberHeaders

// type TableHeader  =   
//    | MemberHeader of (DimensionHeader NList)
//    | TotalHeader of (DimensionHeader NList)


// let tableHeaderAsMembers tableHeader =
//     let dimensionalHeaders =
//          match tableHeader with
//          | MemberHeader hs -> hs
//          | TotalHeader hs -> hs
//     //Head holds all members for tableHeader
//     dimensionalHeaders
//     |> (fun h  -> h.Head)
//     |> (fun (DimensionHeader item)-> item.Members)


// // dimensionAsTableHeader: Direction -> Dimension -> NList DimensionHeader -> NList TableHeader
// let dimensionAsTableHeader direction dimension dimensionHeaders =
//       //all dimension headers have the same members
//       // 
//       let header = dimensionHeaders.Head

//       let columnHeaders (DimensionHeader item)  = 
//             let area = item.Area
//             let members = item.Members |> NList.toList        
//             fromDimension direction area dimension members

//       let mapHeaders i hs =
//          if i = 0 then
//             hs |> NList.toList
//          else 
//             []
//       header
//       |> columnHeaders 
//       |> NList.mapi (fun i m -> MemberHeader (NList.create2 m (mapHeaders i dimensionHeaders)))

// // addDimensionToTableHeader: Direction -> Dimension -> TableHeader -> List TableHeader
// let addDimensionToTableHeader direction dimension tableHeader =

//       let result =
//          match tableHeader with
//             | TotalHeader dimensionHeaders -> dimensionAsTableHeader direction dimension dimensionHeaders
//             | MemberHeader dimensionHeaders -> dimensionAsTableHeader direction dimension dimensionHeaders  
//       result |> NList.toList

// let addDimensionToTableHeaders direction span dimension acc =
//       //Area for first 
//       let initArea d totalSpan =
//             match d with
//               | Horizontal ->
//                   {
//                         VerticalSpan =  0 |> Span |> VerticalSpan
//                         VerticalStart = 0 |> Start |> VerticalStart
//                         HorizontalSpan = totalSpan |> HorizontalSpan
//                         HorizontalStart = 1 |> Start |> HorizontalStart
//                   }

//               | Vertical ->
//                   {
//                         VerticalSpan = totalSpan |> VerticalSpan
//                         VerticalStart =  1 |> Start |> VerticalStart
//                         HorizontalSpan = 0 |> Span |> HorizontalSpan
//                         HorizontalStart =  0 |> Start|> HorizontalStart
//                   }

   
//       match acc with
//          [] -> 
//                let area = initArea direction span
//                let mt = fromDimension direction area dimension []        
//                let v1 =  
//                     mt
//                      |> NList.map (NList.create >> MemberHeader)
//                      |> NList.toList
//                v1 
//          | _ -> acc |> List.collect (addDimensionToTableHeader direction dimension)

// // calculateSpanForDimensions: List Dimension -> Span
// let calculateSpanForDimensions dimensions =
//     let rec recCalculateSpan d =
//          match d with
//             | [] -> 1
//             | head :: tail ->
//                match head with
//                  | DimensionWithDefault (_,m) ->  ((NList.length m.Members) + 1) * (recCalculateSpan tail)
//                  | DimensionWithoutDefault (m)-> (NList.length m.Members) * (recCalculateSpan tail)
//     dimensions
//       |> recCalculateSpan 
//       |> Span
                  

// let calculateTableHeaders direction dimensions =
//       let rec recFold span acc dims  =
//          match dims with
//             | [] -> acc
//             | head :: tail ->   
//                let state = addDimensionToTableHeaders direction span head acc          
//                recFold span state tail 
//         //  totalSpanForDimensions : Span
//       let totalSpanForDimensions = calculateSpanForDimensions dimensions    
//       dimensions
//          |> recFold totalSpanForDimensions []  

// type CubeColumn = CubeColumn of (Member NList)
// let cubeColumnMembers (CubeColumn members) = members

// type  CubeColumnHeader =
//    {
//         IsTotal:bool
//         Area: Area
//         Member: Member
//         IsSelected: bool
//    }

// // tableHeaderToDimensionColumnHeader: List Member -> TableHeader -> List CubeColumnHeader
// let tableHeaderToDimensionColumnHeader selection tableHeader   =
      

//     let createDimensionColumnHeader selectedMembers (isTotal:bool) (DimensionHeader d) =
        
//              let selectedFactors = selectedMembers |> List.map (fun v -> v.Factor)
//             //    filteredMembers: List Member
//              let filteredMembers =
//                   d.Members 
//                   |> NList.toList
//                   |> List.filter (fun m-> (List.contains m.Factor selectedFactors))     
        

//              {
//                    IsTotal = isTotal
//                    Area = d.Area
//                    Member = d.Members.Head  
//                    IsSelected = ((List.length filteredMembers) = (NList.length d.Members))
//              }
   
      
//         //  ch: NList CubeColumnHeader
//     let ch =
//             match tableHeader with
//                | MemberHeader h -> h |> NList.map (createDimensionColumnHeader selection false )
//                | TotalHeader h -> h |> NList.map (createDimensionColumnHeader selection true)
      
//     ch |> NList.toList


// // dimensionColumns: List TableHeader -> List CubeColumn
// let dimensionColumns headers = 
//   headers 
//   |> List.map (tableHeaderAsMembers >> CubeColumn)


// // dimensionColumnHeaders: List Member  -> List TableHeader ->  List CubeColumnHeader 
// let dimensionColumnHeaders selection headers  =
//    headers
//    |> List.collect (tableHeaderToDimensionColumnHeader selection)

// type CubeRowOffset = CubeRowOffset of Offset

// // cubeRowOffsetToOffset: CubeRowOffset -> Offset
// let cubeRowOffsetToOffset (CubeRowOffset offset) = offset

// // ----
// // -- CubeColumns holds offset, columns for cube and headers for cube
// // ----
// type  CubeColumns = 
//    {
//          Offset: CubeRowOffset
//          Columns: CubeColumn List
//          Headers: CubeColumnHeader List
//    }


// // calculateCubeColumns:  Direction -> List Member ->  List Dimension -> CubeColumns  
// let calculateCubeColumns direction selection dimensions =
   
//     let tableHeaders = calculateTableHeaders direction dimensions
       
//     let offset =
//          match direction with
//             | Horizontal -> 
//                dimensions 
//                |> List.length 
//                |> Start 
//                |> VerticalStart
//                |> Area.addVerticalStartToOffset Area.emptyOffset  
//             | Vertical -> 
//                dimensions 
//                |> List.length 
//                |> Start 
//                |> HorizontalStart 
//                |> Area.addHorizontalStartToOffset Area.emptyOffset  
    
//     {
//         Columns = dimensionColumns tableHeaders
//         Headers = dimensionColumnHeaders selection tableHeaders  
//         Offset = offset |>  CubeRowOffset
//     }



// let dim1 = Dimension.createWithDefault "Kvartal" "kv1" [ "kv2" ]
// let dim3 = Dimension.createWithDefault "Produkt" "Personbil" [ "Lastbil" ]

// //let result = calculateCubeColumns Vertical [] [dim1; dim3] 
// let result = calculateTableHeaders Vertical [dim1; dim3] 



open System
#load "Model.fs"
#load "Area.fs"
open Area
open Model
open Model.NList

type  DimensionHeaderItem =
   {

         Area: Area
         Members: Member NList 
   }

type DimensionHeader = DimensionHeader of DimensionHeaderItem

let itemFromDimensionalHeader (DimensionHeader item) = item

// createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
let createDimensionHeaderItem area members m  =
   {
            Area = area
            Members = NList.create2 m members

   }

// createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
let createDimensionHeader a pm m  =
   createDimensionHeaderItem a pm m 
   |> DimensionHeader


// --calculate span for dimensions  
// -- Total span di
// calculateSpan: Dimension -> Span -> Span
let calculateSpan dimension (Span span)   =
   match dimension with
    |  DimensionWithDefault (_, domain) -> span / ((domain.Members |> NList.length) + 1) |> Span
    |  DimensionWithoutDefault (domain) -> span / (domain.Members |> NList.length) |> Span

// calculateStart: Int -> Span -> Start -> Start
let calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

// calculateArea: Dimension -> Direction -> Area -> Int -> Area
let calculateArea dimension direction area ordinal =
   match direction with
     | Horizontal ->
            let hSpan = 
               area.HorizontalSpan 
               |> horizontalSpanToSpan 
               |> calculateSpan dimension
               |> HorizontalSpan

            let hStart = 
               area.HorizontalStart 
               |> horizontalStartToStart
               |> calculateStart ordinal  (hSpan |> horizontalSpanToSpan)
               |> HorizontalStart

            let vStart = 
               area.VerticalStart 
               |> verticalStartMap startIncrement 
            { 
                     VerticalStart = vStart
                     VerticalSpan = 1 |> Span |> VerticalSpan
                     HorizontalStart = hStart
                     HorizontalSpan = hSpan
                 
            }
      | Vertical ->
            let vSpan = 
               area.VerticalSpan
               |> verticalSpanToSpan
               |> calculateSpan dimension
               |> VerticalSpan

            let vStart = 
               area.VerticalStart
               |> verticalStartToStart
               |> calculateStart ordinal (vSpan |> verticalSpanToSpan)
               |> VerticalStart

            let hStart = 
               area.HorizontalStart
               |> horizontalStartMap startIncrement

            { 
                VerticalStart = vStart
                VerticalSpan = vSpan
                HorizontalStart = hStart
                HorizontalSpan = 1 |> Span |> HorizontalSpan

            }

// fromDimension: Direction -> Area -> Dimension -> List Member -> NList DimensionHeader
let fromDimension direction area dimension parentMembers =              
    //   let
    //      members: NList Member
    let members = 
            dimension
            |> dimensionMembers 
            |> NList.map (fun (DomainMember m) -> m)
         
    // let  defaultMembers: List Member
    let defaultMembers = 
            dimension
            |> memberDefault 
            |> Option.map (fun (DefaultMember m) -> m)
            |> Option.toList
            
        //  memberHeaders: NList DimensionHeader
    let memberHeaders = 
            defaultMembers
            |> NList.append members
            |> NList.mapi (fun i m ->  m |> createDimensionHeader (calculateArea dimension direction area i) parentMembers)             
      in
         memberHeaders

type TableHeader  =   
   | MemberHeader of DimensionHeader
   | TotalHeader of DimensionHeader


let tableHeaderAsMembers tableHeader =
    let dimensionalHeaders =
         match tableHeader with
         | MemberHeader hs -> hs
         | TotalHeader hs -> hs
    //Head holds all members for tableHeader
    dimensionalHeaders
    |> (fun h  -> h)
    |> (fun (DimensionHeader item)-> item.Members)


// dimensionAsTableHeader: Direction -> Dimension -> NList DimensionHeader -> NList TableHeader
let dimensionAsTableHeader direction dimension dimensionHeaders =
      //all dimension headers have the same members
      // 
      let header = dimensionHeaders

      let columnHeaders (DimensionHeader item)  = 
            let area = item.Area
            let members = item.Members |> NList.toList        
            fromDimension direction area dimension members

      let mapHeaders i hs =
         if i = 0 then
            hs |> NList.toList
         else 
            []
      header
      |> columnHeaders 
      |> NList.mapi (fun i m -> MemberHeader m)

// addDimensionToTableHeader: Direction -> Dimension -> TableHeader -> List TableHeader
let addDimensionToTableHeader direction dimension tableHeader =

      let result =
         match tableHeader with
            | TotalHeader dimensionHeaders -> dimensionAsTableHeader direction dimension dimensionHeaders
            | MemberHeader dimensionHeaders -> dimensionAsTableHeader direction dimension dimensionHeaders  
      result |> NList.toList

let addDimensionToTableHeaders direction span dimension acc =
      //Area for first 
      let initArea d totalSpan =
            match d with
              | Horizontal ->
                  {
                        VerticalSpan =  0 |> Span |> VerticalSpan
                        VerticalStart = 0 |> Start |> VerticalStart
                        HorizontalSpan = totalSpan |> HorizontalSpan
                        HorizontalStart = 1 |> Start |> HorizontalStart
                  }

              | Vertical ->
                  {
                        VerticalSpan = totalSpan |> VerticalSpan
                        VerticalStart =  1 |> Start |> VerticalStart
                        HorizontalSpan = 0 |> Span |> HorizontalSpan
                        HorizontalStart =  0 |> Start|> HorizontalStart
                  }

   
      match acc with
         [] -> 
               let area = initArea direction span
               let mt = fromDimension direction area dimension []        
               let v1 =  
                    mt
                     |> NList.map MemberHeader
                     |> NList.toList
               v1 
         | _ -> acc |> List.collect (addDimensionToTableHeader direction dimension)

// calculateSpanForDimensions: List Dimension -> Span
let calculateSpanForDimensions dimensions =
    let rec recCalculateSpan d =
         match d with
            | [] -> 1
            | head :: tail ->
               match head with
                 | DimensionWithDefault (_,m) ->  ((NList.length m.Members) + 1) * (recCalculateSpan tail)
                 | DimensionWithoutDefault (m)-> (NList.length m.Members) * (recCalculateSpan tail)
    dimensions
      |> recCalculateSpan 
      |> Span
                  

let calculateTableHeaders direction dimensions =
      let rec recFold span acc dims  =
         match dims with
            | [] -> acc
            | head :: tail ->   
               let state = addDimensionToTableHeaders direction span head acc          
               recFold span state tail 
        //  totalSpanForDimensions : Span
      let totalSpanForDimensions = calculateSpanForDimensions dimensions    
      dimensions
         |> recFold totalSpanForDimensions []  

type CubeColumn = CubeColumn of (Member NList)
let cubeColumnMembers (CubeColumn members) = members

type  CubeColumnHeader =
   {
        IsTotal:bool
        Area: Area
        Member: Member
        IsSelected: bool
   }

// tableHeaderToDimensionColumnHeader: List Member -> TableHeader -> List CubeColumnHeader
let tableHeaderToDimensionColumnHeader selection tableHeader   =
      

    let createDimensionColumnHeader selectedMembers (isTotal:bool) (DimensionHeader d) =
        
             let selectedFactors = selectedMembers |> List.map (fun v -> v.Factor)
            //    filteredMembers: List Member
             let filteredMembers =
                  d.Members 
                  |> NList.toList
                  |> List.filter (fun m-> (List.contains m.Factor selectedFactors))     
        

             {
                   IsTotal = isTotal
                   Area = d.Area
                   Member = d.Members.Head  
                   IsSelected = ((List.length filteredMembers) = (NList.length d.Members))
             }
   
      
        //  ch: NList CubeColumnHeader
    let ch =
            match tableHeader with
               | MemberHeader h -> h |> createDimensionColumnHeader selection false 
               | TotalHeader h -> h |> createDimensionColumnHeader selection true
      
    ch 


// dimensionColumns: List TableHeader -> List CubeColumn
let dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> CubeColumn)


// dimensionColumnHeaders: List Member  -> List TableHeader ->  List CubeColumnHeader 
let dimensionColumnHeaders selection headers  =
   headers
   |> List.map (tableHeaderToDimensionColumnHeader selection)   

type CubeRowOffset = CubeRowOffset of Offset

// cubeRowOffsetToOffset: CubeRowOffset -> Offset
let cubeRowOffsetToOffset (CubeRowOffset offset) = offset

// ----
// -- CubeColumns holds offset, columns for cube and headers for cube
// ----
type  CubeColumns = 
   {
         Offset: CubeRowOffset
         Columns: CubeColumn List
         Headers: CubeColumnHeader List
   }


// calculateCubeColumns:  Direction -> List Member ->  List Dimension -> CubeColumns  
let calculateCubeColumns direction selection dimensions =
   
    let tableHeaders = calculateTableHeaders direction dimensions
       
    let offset =
         match direction with
            | Horizontal -> 
               dimensions 
               |> List.length 
               |> Start 
               |> VerticalStart
               |> Area.addVerticalStartToOffset Area.emptyOffset  
            | Vertical -> 
               dimensions 
               |> List.length 
               |> Start 
               |> HorizontalStart 
               |> Area.addHorizontalStartToOffset Area.emptyOffset  
    
    {
        Columns = dimensionColumns tableHeaders
        Headers = dimensionColumnHeaders selection tableHeaders  
        Offset = offset |>  CubeRowOffset
    }



let dim1 = Dimension.createWithDefault "Kvartal" "kv1" [ "kv2"; "kv3"; "kv4" ]
let dim3 = Dimension.createWithDefault "Produkt" "Personbil" [ "Lastbil" ]

//let result = calculateCubeColumns Vertical [] [dim1; dim3] 
let result = calculateTableHeaders Horizontal [dim1; dim3] 
let columns = calculateCubeColumns Horizontal [] [dim1; dim3] 








   





   