open System
#load "Model.fs"
#load "Area.fs"
open Area
open Model
open Model.NList

type  DimensionHeaderItem =
   {

         Area: Area
         Member: Member NList
   }

type DimensionHeader = DimensionHeader of DimensionHeaderItem
module DimensionHeader =

//itemFromDimensionalHeader
    let value (DimensionHeader item) = item

    //createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
    let createItem a pm m  =
       {
                Area = a
                Member = NList.create2 m pm
       }


    // createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
    let createHeader a pm m  =
       createItem a pm m 
       |> DimensionHeader

   

// calculateSpan: Dimension -> Span -> Span
let calculateSpan (dimension:Dimension) (Span span)   =
   match dimension with
      | DimensionWithDefault (_, domain) -> (span - 1) / (domain.Members |> NList.length) 
      | DimensionWithoutDefault (domain) -> span / (domain.Members |> NList.length) 
   |> Span

// calculateStart: Int -> Span -> Start -> Start
let calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

//  Dimension -> Direction -> Area -> Int -> Area
let calculateArea dimension direction (area: Area) ordinal =
   match direction with
        | Horizontal ->
            let hSpan = 
               area.HorizontalSpan 
               |> Area.horizontalSpanToSpan 
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
                 HorizontalSpan = hSpan }
        |  Vertical ->
            let vSpan = 
               area.VerticalSpan
               |> Area.verticalSpanToSpan
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

// calculateDefaultArea: Direction -> Depth -> Area -> Area
let calculateDefaultArea direction (Depth depth) (area: Area) =
      match direction with
        | Horizontal -> 

               {
                    VerticalStart = area.VerticalStart
                    VerticalSpan = VerticalSpan (Span depth)
                    HorizontalSpan = HorizontalSpan (Span 1)
                    HorizontalStart = 
                      area.HorizontalStart  
                      |> horizontalStartMap ((spanStart (fun a b -> a + b) (area.HorizontalSpan |> horizontalSpanToSpan)) >> Start)
               }

         | Vertical ->
               {
                     HorizontalStart = area.HorizontalStart
                     HorizontalSpan = HorizontalSpan (Span depth)
                     VerticalSpan = VerticalSpan  (Span 1)
                     VerticalStart = 
                        area.VerticalStart  
                        |> verticalStartMap ((spanStart (fun a b -> a + b) (area.VerticalSpan |> verticalSpanToSpan)) >> Start)
               }


// fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> (NList DimensionHeader, Maybe DimensionHeader )
let fromDimension direction depth area dimension parentMembers =
                
      
        //  dms: NList DomainMember
         let dms = Model.dimensionMembers dimension
        //  dm: Maybe DefaultMember
         let dm = memberDefault dimension

         let memberHeaders = 
            dms
            |> NList.mapi
               (fun i (DomainMember m) ->  
                     m
                     |> DimensionHeader.createHeader (calculateArea dimension direction area i) parentMembers) 
               
         // todo: gör om till lista som inte kan vara tom
        //  getLastArea: NList DimensionHeader -> Area 
         let getLastArea headers = 
            headers 
            |> NList.last
            |> (fun (DimensionHeader item) -> item.Area)

         // defaultMemberHeader: Maybe DimensionHeader
         let defaultMemberHeader =
            dm
            |> Option.map (fun (DefaultMember md) ->  
               md 
               |> DimensionHeader.createHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)) parentMembers)
      
         (memberHeaders, defaultMemberHeader)


// -- type MemberHeader = MemberHeader DimensionHeader
// -- type DefaultHeader = DefaultHeader DimensionHeader
type TableHeader  = 
   | MemberHeader of DimensionHeader NList
   | TotalHeader of DimensionHeader NList

// tableHeaderAsMembers: TableHeader -> NList Member
let tableHeaderAsMembers acc =
//    let 
//       headers: NList DimensionHeader
//       headers =
         match acc with
            | MemberHeader hs -> hs
            | TotalHeader hs -> hs
         |> (fun h -> h.Head)    
         |> (fun (DimensionHeader item)-> item.Member)



// -- accumulatedHeaders: TableHeader -> List DimensionHeader
// -- accumulatedHeaders acc =
// --    case acc of
// --       MemberHeader headers -> headers |> NList.toList
// --       TotalHeader headers -> headers |> NList.toList
      


// dimensionAsTableHeader: Direction -> Depth -> Dimension -> NList DimensionHeader -> NList TableHeader
let dimensionAsTableHeader direction depth dimension headers =

      let header = headers.Head

    //   columnHeaders: DimensionHeader -> (NList DimensionHeader, Maybe DimensionHeader)
      let columnHeaders (DimensionHeader item)  = 
            let area = item.Area
            let members = item.Member |> NList.toList
         
            fromDimension direction depth area dimension members

    //   mapHeaders: Int -> NList a -> List a
      let mapHeaders i hs =
         if i = 0 then
            hs |> NList.toList
         else 
            []

    //   result: (NList DimensionHeader, Maybe DimensionHeader)
      let first,second = columnHeaders header
    //   v1: NList TableHeader  
      let v1 =  
            first
            |> NList.mapi (fun i m -> MemberHeader (NList.create2 m (mapHeaders i headers))) 

      let v2 = 
         second
         |> Model.Option.asList
         |> List.map (NList.create >> TotalHeader)
   
      NList.addList v1 v2

// addDimensionToTableHeader: Direction -> Depth -> Dimension -> TableHeader -> List TableHeader
let addDimensionToTableHeader direction depth dimension acc =
     match acc with
        | TotalHeader _ -> NList.create acc
        | MemberHeader dimensionHeaders -> dimensionAsTableHeader direction depth dimension dimensionHeaders
     |>
        NList.toList

// addDimensionToTableHeaders: Direction -> Depth -> Span -> Dimension -> List TableHeader -> List TableHeader
let addDimensionToTableHeaders direction depth span dimension acc =

    //   initArea: Direction -> Span -> Area
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
        | [] -> 
               let area = initArea direction span
               let members, total = fromDimension direction depth area dimension []  
               let memberHeaders =  
                    members
                    |> NList.map (NList.create >> MemberHeader)
               let totalHeader = 
                  total
                  |> Option.asList
                  |> List.map (NList.create >> TotalHeader)       
               NList.addList memberHeaders totalHeader
               |> NList.toList
        | _ -> acc |> List.collect (addDimensionToTableHeader direction depth dimension)

// calculateSpanForDimensions: List Dimension -> Span
let calculateSpanForDimensions dimensions =
    let rec recCalculateSpan d =
         match d with
          |  [] -> 1
          |  head :: tail ->
               match head with
                |   DimensionWithDefault (_,m) ->  (NList.length m.Members) * (recCalculateSpan tail) + 1 
                |   DimensionWithoutDefault (m)-> (NList.length m.Members) * (recCalculateSpan tail)

    dimensions
      |> recCalculateSpan 
      |> Span
                  

// calculateTableHeaders: Direction -> (List Dimension) -> List TableHeader
let calculateTableHeaders direction dimensions =
//    let
    //   calculateNextDepth: Depth -> Depth
    let calculateNextDepth (Depth d) = (d - 1) |> Depth
    //   recFold: Span -> Depth -> List TableHeader -> List Dimension ->  List TableHeader 
    let rec recFold span depth acc dims  =
         match dims with
           | [] -> acc
           | head :: tail ->                             
                let nextDepth = calculateNextDepth depth
                let state = addDimensionToTableHeaders direction depth span head acc 
             
                recFold span nextDepth state tail 
    let totalSpanForDimensions = calculateSpanForDimensions dimensions
      
    dimensions
    |> recFold totalSpanForDimensions (dimensions |> List.length |> Depth) []  

type DimensionColumn = DimensionColumn of Member NList

type  DimensionColumns = DimensionColumns  of DimensionColumn List

type  DimensionColumnHeader =
   {
         IsTotal: bool
         Area: Area
         Member: Member
   }

// tableHeaderToDimensionColumnHeader: TableHeader -> List DimensionColumnHeader
let tableHeaderToDimensionColumnHeader tableHeader =
    let createDimensionColumnHeader isTotal (DimensionHeader d) =
         {
               IsTotal = isTotal
               Area = d.Area
               Member = d.Member.Head  
         }

    match tableHeader with
    | MemberHeader h -> h |> NList.map (createDimensionColumnHeader false)
    | TotalHeader h -> h |> NList.map (createDimensionColumnHeader true)
    |> NList.toList


// dimensionColumns: List TableHeader -> DimensionColumns
let dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> DimensionColumn)
  |> DimensionColumns


// dimensionColumnHeaders: List TableHeader -> List DimensionColumnHeader 
let dimensionColumnHeaders headers =
   headers
   |> List.collect tableHeaderToDimensionColumnHeader


let dim1 = Dimension.createWithDefault "Kvartal" "kv1" ["kv2" ]
let dim3 = Dimension.createWithDefault "Produkt" "Personbil" ["Lastbil" ]
let dim4 = Dimension.createWithDefault "Produkt2" "Tung" ["Latt" ]
let dim2 = Dimension.createWithDefault "Scandinavien"  "Sverige" ["Norge" ]


let width = calculateSpanForDimensions [ dim1 ; dim2 ]
let headers = calculateTableHeaders Direction.Horizontal [ dim1 ; dim2 ]


// let columns = headers |> List.map TableHeader.columns
// let structure = headers |> List.collect TableHeader.headers 
// let header = headers |> List.rev |> List.head