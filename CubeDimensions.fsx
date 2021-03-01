open System
#load "Model.fs"
#load "Area.fs"

type  DimensionHeaderItem =
   {

         Area: Area
         Member: NList Member 
   }

type DimensionHeader = DimensionHeader of DimensionHeaderItem
itemFromDimensionalHeader: DimensionHeader -> DimensionHeaderItem
itemFromDimensionalHeader (DimensionHeader item) = item

createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
createDimensionHeaderItem a pm m  =
   {
            area = a
         ,  member = NList.create2 m pm

   }

createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
createDimensionHeader a pm m  =
   createDimensionHeaderItem a pm m 
   |> DimensionHeader

calculateSpan: Dimension -> Span -> Span
calculateSpan dimension (Span span)   =
   case dimension of
      DimensionWithDefault (_, domain) -> (span - 1) // (domain.members |> NList.length) |> Span
      DimensionWithoutDefault (domain) -> span // (domain.members |> NList.length) |> Span
   

calculateStart: Int -> Span -> Start -> Start
calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

calculateArea: Dimension -> Direction -> Area -> Int -> Area
calculateArea dimension direction area ordinal =
   case direction of
      Horizontal ->
         let
            hSpan: HorizontalSpan
            hSpan = 
               area.horizontalSpan 
               |> horizontalSpanToSpan 
               |> calculateSpan dimension
               |> HorizontalSpan
            hStart: HorizontalStart
            hStart = 
               area.horizontalStart 
               |> horizontalStartToStart
               |> calculateStart ordinal  (hSpan |> horizontalSpanToSpan)
               |> HorizontalStart
            vStart: VerticalStart
            vStart = 
               area.verticalStart 
               |> verticalStartMap startIncrement --Lines.startAdd (Lines.verticalStart area.verticalLine) (Start 1)    
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = 1 |> Span |> VerticalSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = hSpan
                 
               --    horizontalLine = HorizontalLine (createLine hStart hSpan)  -- HorizontalLine    { span = horizontalSpan; start = horizontalstart} 
               -- ,  verticalLine = VerticalLine (createLine vStart (Span 1))  --VerticalLine { span =  ; start = verticalStart}
            }
      Vertical ->
         let
            vSpan: VerticalSpan
            vSpan = 
               area.verticalSpan
               |> verticalSpanToSpan
               |> calculateSpan dimension
               |> VerticalSpan
            vStart: VerticalStart
            vStart = 
               area.verticalStart
               |> verticalStartToStart
               |> calculateStart ordinal (vSpan |> verticalSpanToSpan)
               |> VerticalStart
            hStart: HorizontalStart 
            hStart = 
               area.horizontalStart
               |> horizontalStartMap startIncrement
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = vSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = 1 |> Span |> HorizontalSpan
                 
               --    horizontalLine = HorizontalLine (createLine hStart (Span 1)) -- { Span = Span 1; Start = horizontalStart}
               -- ,  verticalLine = VerticalLine (createLine vStart vSpan)--{ Span = verticalSpan ; Start = verticalStart }
            }

calculateDefaultArea: Direction -> Depth -> Area -> Area
calculateDefaultArea direction (Depth depth) area =
   -- let
   --    vl: VerticalLine
   --    vl = area.verticalLine
   --    hl: HorizontalLine
   --    hl = area.horizontalLine
   -- in
      case direction of
         Horizontal -> 
            -- let

               {
                     verticalStart = area.verticalStart
                  ,  verticalSpan = VerticalSpan (Span depth)
                  ,  horizontalSpan = HorizontalSpan (Span 1)
                  ,  horizontalStart = 
                     area.horizontalStart  
                     |> horizontalStartMap ((spanStart (\a b -> a + b) (area.horizontalSpan |> horizontalSpanToSpan)) >> Start)
               }
            --    newVl: VerticalLine 
            --    newVl =
            --       vl
            --       |> verticalSetSpan (Span depth)
            --    newHl: HorizontalLine
            --    newHl =
            --       hl
            --       |> horizontalIncrementStartWithSpan 
            --       |> horizontalSetSpan (Span 1)
            -- in
            --    { 
            --          verticalLine = newVl
            --       ,  horizontalLine = newHl
            --    }
         Vertical ->
               {
                     horizontalStart = area.horizontalStart
                  ,  horizontalSpan = HorizontalSpan (Span depth)
                  ,  verticalSpan = VerticalSpan  (Span 1)
                  ,  verticalStart = 
                     area.verticalStart  
                     |> verticalStartMap ((spanStart (\a b -> a + b) (area.verticalSpan |> verticalSpanToSpan)) >> Start)
               }
            -- let
            --    newHl: HorizontalLine 
            --    newHl =
            --       hl
            --       |> horizontalSetSpan (Span depth)
            --    newVl: VerticalLine
            --    newVl =
            --       vl
            --       |> verticalIncrementStartWithSpan 
            --       |> verticalSetSpan (Span 1)
            -- in
            --    { 
            --          verticalLine = newVl
            --       ,  horizontalLine = newHl
            --    }

fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> (NList DimensionHeader, Maybe DimensionHeader )
fromDimension direction depth area dimension parentMembers =
                
      let
         dms: NList DomainMember
         dms = dimensionMembers dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: NList DimensionHeader
         memberHeaders = 
            dms
            |> NList.mapi
               (\ i (DomainMember m) ->  
                     m
                     |> createDimensionHeader (calculateArea dimension direction area i) parentMembers) 
               
         -- todo: gör om till lista som inte kan vara tom
         getLastArea: NList DimensionHeader -> Area 
         getLastArea headers = 
            headers 
            |> NList.last
            |> (\ (DimensionHeader item) -> item.area)

         defaultMemberHeader: Maybe DimensionHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               md 
               |> createDimensionHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)) parentMembers)
      in
         (memberHeaders, defaultMemberHeader)


-- type MemberHeader = MemberHeader DimensionHeader
-- type DefaultHeader = DefaultHeader DimensionHeader
type TableHeader  = 
   MemberHeader (NList DimensionHeader)
   | TotalHeader (NList DimensionHeader)

tableHeaderAsMembers: TableHeader -> NList Member
tableHeaderAsMembers acc =
   let 
      headers: NList DimensionHeader
      headers =
         case acc of
            MemberHeader hs -> hs
            TotalHeader hs -> hs
   in 
     headers
     |> (\ h -> h.head)
     |> (\ (DimensionHeader item)-> item.member)


-- accumulatedHeaders: TableHeader -> List DimensionHeader
-- accumulatedHeaders acc =
--    case acc of
--       MemberHeader headers -> headers |> NList.toList
--       TotalHeader headers -> headers |> NList.toList
      


dimensionAsTableHeader: Direction -> Depth -> Dimension -> NList DimensionHeader -> NList TableHeader
dimensionAsTableHeader direction depth dimension headers =
   let 

      header: DimensionHeader
      header = headers.head

      columnHeaders: DimensionHeader -> (NList DimensionHeader, Maybe DimensionHeader)
      columnHeaders (DimensionHeader item)  = 
         let
            area: Area 
            area = item.area
            members: List Member
            members = item.member |> NList.toList
         in 
            fromDimension direction depth area dimension members

      mapHeaders: Int -> NList a -> List a
      mapHeaders i hs =
         if i == 0 then
            hs |> NList.toList
         else 
            []

      result: (NList DimensionHeader, Maybe DimensionHeader)
      result = columnHeaders header
      v1: NList TableHeader  
      v1 =  (first result)
            |> NList.mapi (\ i m -> MemberHeader (NList.create2 m (mapHeaders i headers))) 
      v2: List TableHeader 
      v2 = 
         (second result)
         |> Lists.maybeAsList
         |> List.map (NList.create >> TotalHeader)
   in
      NList.addList v1 v2

addDimensionToTableHeader: Direction -> Depth -> Dimension -> TableHeader -> List TableHeader
addDimensionToTableHeader direction depth dimension acc =
   let
      result: NList TableHeader
      result =
         case acc of
            TotalHeader _ -> NList.create acc
            MemberHeader dimensionHeaders -> dimensionAsTableHeader direction depth dimension dimensionHeaders
   in
      result |> NList.toList

addDimensionToTableHeaders: Direction -> Depth -> Span -> Dimension -> List TableHeader -> List TableHeader
addDimensionToTableHeaders direction depth span dimension acc =
   let 
      initArea: Direction -> Span -> Area
      initArea d totalSpan =
            case d of
               Horizontal ->
                  {
                        verticalSpan =  0 |> Span |> VerticalSpan
                     ,  verticalStart = 0 |> Start |> VerticalStart
                     ,  horizontalSpan = totalSpan |> HorizontalSpan
                     ,  horizontalStart = 1 |> Start |> HorizontalStart
                  }
                  -- { verticalLine = VerticalLine { span = Span 0, start = Start 0},  horizontalLine = HorizontalLine {span = totalSpan, start = Start 1}}
               Vertical ->
                  {
                        verticalSpan = totalSpan |> VerticalSpan
                     ,  verticalStart =  1 |> Start |> VerticalStart
                     ,  horizontalSpan = 0 |> Span |> HorizontalSpan
                     ,  horizontalStart =  0 |> Start|> HorizontalStart
                  }
                  -- { verticalLine = VerticalLine  { span = totalSpan, start = Start 1}, horizontalLine = HorizontalLine {span = Span 0, start = Start 0}}
   in
      case acc of
         [] -> 
            let 
               area: Area 
               area = initArea direction span
               mt: (NList DimensionHeader, Maybe DimensionHeader)
               mt = fromDimension direction depth area dimension []
               v1: NList TableHeader  
               v1 =  (first mt)
                     |> NList.map (NList.create >> MemberHeader)
               v2: List TableHeader 
               v2 = 
                  (second mt)
                  |> Lists.maybeAsList
                  |> List.map (NList.create >> TotalHeader)
            in
               NList.addList v1 v2
               |> NList.toList
         _ -> acc |> Lists.collect (addDimensionToTableHeader direction depth dimension)

calculateSpanForDimensions: List Dimension -> Span
calculateSpanForDimensions dimensions =
   let
      recCalculateSpan: List Dimension -> Int
      recCalculateSpan d =
         case d of
            [] -> 1
            head :: tail ->
               case head of
                  DimensionWithDefault (_,m) ->  (NList.length m.members) * (recCalculateSpan tail) + 1 
                  DimensionWithoutDefault (m)-> (NList.length m.members) * (recCalculateSpan tail)
   in
      dimensions
      |> recCalculateSpan 
      |> Span
                  

calculateTableHeaders: Direction -> (List Dimension) -> List TableHeader
calculateTableHeaders direction dimensions =
   let
      calculateNextDepth: Depth -> Depth
      calculateNextDepth (Depth d) = (d - 1) |> Depth
      recFold: Span -> Depth -> List TableHeader -> List Dimension ->  List TableHeader 
      recFold span depth acc dims  =
         case dims of
            [] -> acc
            head :: tail ->   
               let
                  nextDepth: Depth 
                  nextDepth = calculateNextDepth depth
                  state: List TableHeader 
                  state = addDimensionToTableHeaders direction depth span head acc 
               in
                  recFold span nextDepth state tail 
   in
      let
         totalSpanForDimensions : Span
         totalSpanForDimensions = calculateSpanForDimensions dimensions
      in
         dimensions
         |> recFold totalSpanForDimensions (dimensions |> List.length |> Depth) []  

type DimensionColumn = DimensionColumn (NList Member)

type  DimensionColumns = DimensionColumns (List DimensionColumn)

type alias DimensionColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  member: Member
   }

tableHeaderToDimensionColumnHeader: TableHeader -> List DimensionColumnHeader
tableHeaderToDimensionColumnHeader tableHeader =
   let   
      createDimensionColumnHeader: Bool -> DimensionHeader -> DimensionColumnHeader
      createDimensionColumnHeader isTotal (DimensionHeader d) =
         {
               isTotal = isTotal
            ,  area = d.area
            ,  member = d.member.head  
         }
   in
      let
         ch: NList DimensionColumnHeader
         ch =
            case tableHeader of
               MemberHeader h -> h |> NList.map (createDimensionColumnHeader False)
               TotalHeader h -> h |> NList.map (createDimensionColumnHeader True)
      in
   
         ch |> NList.toList


dimensionColumns: List TableHeader -> DimensionColumns
dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> DimensionColumn)
  |> DimensionColumns


dimensionColumnHeaders: List TableHeader -> List DimensionColumnHeader 
dimensionColumnHeaders headers =
   headers
   |> Lists.collect tableHeaderToDimensionColumnHeader