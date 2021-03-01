
module Model
   open System
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
         
   type ValueKonceptId = ValueKonceptId of Guid

   type ValueKonceptName = ValueKonceptName of String

   let valueKonceptNameToString (ValueKonceptName name) = name
      
   type  ValueKoncept =
       {
               Name: ValueKonceptName
               Id: ValueKonceptId
               Selected: Boolean
       }
   type AbstractKonceptName = AbstractKonceptName of String

   let abstractKonceptNameToString (AbstractKonceptName name) = name
      
   type AbstractKonceptId = AbstractKonceptId of Guid

   type  AbstractKoncept =
       {
               Name : AbstractKonceptName
               Id : AbstractKonceptId
               Selected: Boolean
       }

   let createAbstract name : AbstractKoncept=
      { Name = name ; Id = Guid.NewGuid() |> AbstractKonceptId; Selected = false}

   let createValue name : ValueKoncept =
      { Name = name ; Id = Guid.NewGuid() |> ValueKonceptId; Selected = false}

   type DomainName = | DomainName of String

   let domainNameToString (DomainName name) = name

   type Factor = | Factor of int
   type Member =
      {
         Id: Guid
         Name: String
         Factor: Factor
      }

   module Member =
      let create factor name  =
         { Id = Guid.NewGuid(); Name = name ; Factor = factor}

      let fromList (f: 'a -> Member)  m = 
         m |> NList.map f

   type DomainMember = | DomainMember of Member
   module DomainMember =
      let create factor name  = 
         Member.create factor name 
        |> DomainMember

   let domainMemberToString (DomainMember m) = m
   type Domain =
       {
         Name: DomainName
         Members:  DomainMember NList
       }

   type DefaultMember = | DefaultMember of Member  
   module DefaultMember =
      let create factor name  = 
         Member.create factor name 
        |> DefaultMember

   type Dimension =
      | DimensionWithDefault of (DefaultMember*Domain)
      | DimensionWithoutDefault of Domain

   module Dimension =
      let members dimension =
         match dimension with
         | DimensionWithDefault (_,d) -> d.Members
         | DimensionWithoutDefault (d) -> d.Members

      let defaultMember dimension =
          match dimension with
            | DimensionWithDefault (d,_) ->  Some d 
            | DimensionWithoutDefault (_) -> None


      let fromListWithDefault f d m =
             m 
             |> Member.fromList f 
             |> NList.map DomainMember
             |> fun members ->  DimensionWithDefault ((Member.create (Factor 1) (sprintf "total:%A" d) |> DefaultMember),{ Name = d ; Members = members })
     
      let fromListWithoutDefault f d m =
             m 
             |> Member.fromList f 
             |> NList.map DomainMember
             |> fun members ->  DimensionWithoutDefault ({ Name = d ; Members = members })

      let fromStringListWithDefault =
            fromListWithDefault (Member.create (Factor 1))

      let fromStringListWithoutDefault =
            fromListWithoutDefault (Member.create (Factor 1))

   type HyperDimension =
        | Opened of Dimension
        | Closed of Dimension

   type HyperCubeName = | HyperCubeName of String

   let hyperCubeNameToString (HyperCubeName name) = name

   type HyperCubeId = HyperCubeId of Guid

   type  HyperCube =
       {
              Name: HyperCubeName
              Head: HyperDimension
              Tail:  HyperDimension List
              List: HyperCubeId
       }

   type DimensionalKoncept =
       | DimensionalAbstract  of (AbstractKoncept*  DimensionalKoncept List) 
       | DimensionalValue of ValueKoncept

   type Koncept =
       Cube  of (HyperCube * DimensionalKoncept List)
       | Abstract  of (AbstractKoncept *  Koncept List)
       | Value of ValueKoncept