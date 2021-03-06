module Area 
   open System
   type Span = Span of int

   let spanInt (Span span) = span

   let spanAdd (Span span1) (Span span2) = Span (span1 + span2)

   let spanMap f (Span span) =
      f span
      |> Span

   let spanIncrement = spanMap (fun i -> i + 1) 

   type VerticalSpan = VerticalSpan of Span

   let verticalSpanMap f (VerticalSpan span) =
      span   
      |> f 
      |> VerticalSpan

   let verticalSpanToSpan (VerticalSpan span) = span

   let verticalSpanToInt = verticalSpanToSpan >> spanInt

   let intToVerticalSpan = Span >> VerticalSpan

   type HorizontalSpan = HorizontalSpan of Span

   let horizontalSpanMap f (HorizontalSpan span) =
      span   
      |> f 
      |> HorizontalSpan

   let horizontalSpanToSpan (HorizontalSpan span) = span

   let horizontalSpanToInt = horizontalSpanToSpan >> spanInt

   let intToHorizontalSpan = Span >> HorizontalSpan

   type Start = Start of int

   let startInt (Start start) = start

   let startMap f (Start start) =
      f start
      |> Start

   let startIncrement = startMap (fun i -> i + 1) 
     
   let startAdd (Start start1) (Start start2) = Start (start1 + start2)

   let startSpan f (Start start) (Span span) =
      f start span

   let spanStart f  (Span span) (Start start)=
      f start span

   type VerticalStart = VerticalStart of Start

   let verticalStartMap f (VerticalStart start) =
      start   
      |> f 
      |> VerticalStart

   let verticalStartToStart (VerticalStart start) = start  

   let verticalStartToInt = verticalStartToStart >> startInt

   let intToVerticalStart = Start >> VerticalStart

   type HorizontalStart = HorizontalStart of Start

   let horizontalStartMap f (HorizontalStart start) =
      start   
      |> f 
      |> HorizontalStart

   let horizontalStartToStart (HorizontalStart start) = start


   let horizontalStartToInt = horizontalStartToStart >> startInt

   let intToHorizontalStart = Start >> HorizontalStart

   type Area =
      {
            HorizontalStart: HorizontalStart
            HorizontalSpan: HorizontalSpan
            VerticalStart: VerticalStart
            VerticalSpan: VerticalSpan
      }
   type Direction =
      Vertical
      | Horizontal



   let incrementVerticalStart area =
      {
          area with VerticalStart = area.VerticalStart |> (verticalStartMap startIncrement)
      }


   let incrementHorizontalStart area =
      {
          area with HorizontalStart = area.HorizontalStart |> (horizontalStartMap startIncrement)
      }


   let setHorizontalStart start area =
      {
         area with HorizontalStart = HorizontalStart start
      }


   let setVerticalStart start area =
      {
         area with VerticalStart = VerticalStart start
      }


   let setHorizontalSpan span area =
      {
         area with HorizontalSpan = HorizontalSpan span
      }


   let setVerticalSpan span area =
      {
         area with VerticalSpan = VerticalSpan span
      }

   let verticalStart (area: Area) =
      area.VerticalStart |> verticalStartToStart


   let horizontalStart (area: Area) =
      area.HorizontalStart |> horizontalStartToStart


   let verticalSpan (area: Area)  =
      area.VerticalSpan |> verticalSpanToSpan


   let horizontalSpan (area: Area)  =
      area.HorizontalSpan |> horizontalSpanToSpan

   let verticalStartAdd (VerticalStart start1) (VerticalStart start2) =
      start1 
      |> startAdd start2
      |> VerticalStart

   let horizontalStartAdd (HorizontalStart start1) (HorizontalStart start2) =
      start1 
      |> startAdd start2
      |> HorizontalStart

   let emptyArea =  
       {    
            HorizontalStart = intToHorizontalStart 0
            HorizontalSpan = intToHorizontalSpan 0
            VerticalStart = intToVerticalStart 0
            VerticalSpan = intToVerticalSpan 0
       }


   type Offset = 
      {
         VerticalStart: VerticalStart
         HorizontalStart: HorizontalStart
      }
   
   let emptyOffset = 
      { 
         VerticalStart = 0 |> Start |>  VerticalStart
         HorizontalStart = 0 |> Start |> HorizontalStart }

   let offsetAddVertical (offset: Offset)  (start: VerticalStart) =
        { offset with   VerticalStart = verticalStartAdd offset.VerticalStart start }
           
   let offsetAddHorizontal (offset: Offset)  (start: HorizontalStart) =
       { offset with   HorizontalStart = horizontalStartAdd offset.HorizontalStart start }


   let offset (offset: Offset) (area: Area) =
        { area with VerticalStart = area.VerticalStart  |> verticalStartAdd offset.VerticalStart }
        |> (fun state ->  { state with HorizontalStart = state.HorizontalStart  |> horizontalStartAdd offset.HorizontalStart })

   let addVerticalStartToOffset offset vStart    =

         let startOffest = offset.VerticalStart |> verticalStartToStart   

         let start = vStart |> verticalStartToStart  
      
         { offset with  VerticalStart = start |> startAdd startOffest |> VerticalStart }

   let addHorizontalStartToOffset offset hStart  =
      
 
         let startOffest = offset.HorizontalStart |> horizontalStartToStart   

         let start = hStart |> horizontalStartToStart  
      
         { offset  with HorizontalStart = start |> startAdd startOffest |> HorizontalStart }

   type Depth = Depth of int
   let depthInt (Depth depth) = depth