module Model
   open System

   module Option =
      let asList a =
         match a with
          | Some v -> [v]
          | None -> []
   type NList<'a> =
      {
            Head: 'a
            Tail: 'a List 
      }
   module NList =

      let create a =
         {
               Head = a
               Tail = []
         }

      let create2 m l =
         {
               Head = m
               Tail = l
         }

      let toList m = 
        [ m.Head ] @ m.Tail

      let length m =
         1 + (m.Tail |> List.length)
        

      let last m =
         match m.Tail |> List.rev with
         | [] -> m.Head
         | head :: _ -> head

      let map f m =
         {
               Head = f m.Head
               Tail = m.Tail |> List.map f
         }
         
      let mapi f m =
         let rec mapi' i m =
            match m with
            | [] -> []
            | head :: tail -> [ f i head ] @ mapi' (i + 1) tail
         {
            Head = f 0 m.Head
            Tail =  mapi' 1 m.Tail         
         }

      let append n m =
         { n with Tail = n.Tail @ m}

      let addFirst n m =

         let tail = toList m
         {
            Head = n
            Tail = tail
         }

      let addList n m =
         { n with  Tail = n.Tail @ m }
         
//   module Koncepts.Model exposing (..)
// import Id exposing (..)
// import Id
// import NList exposing (..)
// -- import Events.Custom exposing (onClickStopPropagation)

   type Id = Id of Guid
   module Id =
      let create() = Guid.NewGuid() |> Id

   type ValueKonceptId = ValueKonceptId  of Id
   type ValueKonceptName = ValueKonceptName of String

   
   let valueKonceptNameToString (ValueKonceptName name) =
      name

   type  ValueKoncept =
       {
              Name: ValueKonceptName
              Id: ValueKonceptId
              Selected: bool
       }
   type AbstractKonceptName = AbstractKonceptName of String


   let abstractKonceptNameToString (AbstractKonceptName name) =
      name
   type AbstractKonceptId = AbstractKonceptId of Id
   type  AbstractKoncept =
       {
              Name : AbstractKonceptName
              Id : AbstractKonceptId
              Selected: bool
       }
       

   type DomainName = DomainName of String

   let domainNameToString (DomainName name) = name

   type Factor = Factor of int
   type  Member =
      {
            Id : Id
            Name: String
            Factor: Factor
      }

   // createMember: Factor -> String -> Member
   let createMember factor name =
      {
            Id = Id.create()
            Factor = factor
            Name = name
      }
   type DomainMember = DomainMember of Member


   let createDomainMember factor name =
      createMember factor name 
      |> DomainMember

   
   let domainMemberToString (DomainMember m) = m.Name
   type Domain =
       {
            Name: DomainName
            Members:  DomainMember NList
       }

   type DefaultMember = DefaultMember of Member  
   // createDefaultMember: Factor -> String -> DefaultMember
   let createDefaultMember factor name =
      createMember factor name 
      |> DefaultMember

   type Dimension =
        | DimensionWithDefault of (DefaultMember * Domain)
        | DimensionWithoutDefault of Domain

   module Dimension =
      let createDomain name first rest : Domain =
            { 
                  Name = DomainName name; 
                  Members =    
                     NList.create2 first rest 
                     |> NList.map (fun s -> s |> createMember (Factor 1) |>DomainMember )
             }

      let createWithDefault name first rest =
          let dm = createDefaultMember (Factor 1) (sprintf "Total:%s" name)
          DimensionWithDefault (dm,createDomain name first rest)
      
      let createWithoutDefault name first rest =
         DimensionWithoutDefault (createDomain name first rest)
          
   // dimensionMembers: Dimension -> NList DomainMember
   let dimensionMembers dimension =
      match dimension with 
         | DimensionWithDefault (_,m) -> m.Members
         | DimensionWithoutDefault m -> m.Members

   // memberDefault: Dimension -> Maybe DefaultMember
   let memberDefault dimension =
      match dimension with 
         | DimensionWithDefault (d,_) -> Some d
         | DimensionWithoutDefault _ -> None
      
   type HyperDimension =
        | Opened of Dimension
        | Closed of Dimension

   // hyperDimensionAsDimension: HyperDimension -> Dimension
   let hyperDimensionAsDimension hyperDimension =
       match hyperDimension with
          | Opened dimension -> dimension
          | Closed dimension -> dimension

   type HyperCubeName = HyperCubeName of String

   // hyperCubeNameToString: HyperCubeName -> String
   let hyperCubeNameToString (HyperCubeName name) = name
   type HyperCubeId = HyperCubeId of Id
   type  HyperCube =
       {
               Name: HyperCubeName
               Dimensions: HyperDimension NList
               Id: HyperCubeId
       }


   type DimensionalKoncept =
       | DimensionalAbstract  of (AbstractKoncept *  DimensionalKoncept List) 
       | DimensionalValue of ValueKoncept

   type Koncept =
       | Cube  of HyperCube *  DimensionalKoncept List 
       | Abstract  of AbstractKoncept * Koncept List
       | Value of ValueKoncept

   // createValueKonceptWithSelection: Bool -> String -> ValueKoncept   
   let createValueKonceptWithSelection selected name : ValueKoncept =
           {
                Id = Id.create() |> ValueKonceptId 
                Name = name |> ValueKonceptName
                Selected = selected }

   // createAbstractKonceptWithSelection: Bool -> String -> AbstractKoncept
   let createAbstractKonceptWithSelection selected name  =  
           {
               Id = Id.create() |> AbstractKonceptId 
               Name = AbstractKonceptName name   
               Selected = selected               
           } 

   // createValueKoncept: String -> ValueKoncept   
   let createValueKoncept = createValueKonceptWithSelection false


   // createAbstractKoncept: String -> AbstractKoncept
   let createAbstractKoncept = createAbstractKonceptWithSelection false


   type ModelAction<'a> = 
      Delete 
      | MapValue of 'a
      | Ignore         