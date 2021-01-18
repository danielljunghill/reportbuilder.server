namespace ReportDefinition
module Koncepts =
    open System
    type Id = Guid
    type  KonceptInformation = {
        Name: String
        Id: Id
        Selected: Boolean
        } 

    type KI = KonceptInformation
    type AbstractKoncept = | AbstractKoncept of KI  
    module AbstractKoncept =
        let create name =
            AbstractKoncept { Name = name; Id = Guid.NewGuid(); Selected = false}
        let toKonceptInformaiton (AbstractKoncept ki) = 1
  
    type ValueKoncept = | ValueKoncept of KI
    module ValueKoncept =
        let create name = ValueKoncept { Name = name; Id = Guid.NewGuid(); Selected = false}
        let toKonceptInformaiton (ValueKoncept ki) = ki


    type Koncept   = 
       Value of ValueKoncept
       | Koncepts of (AbstractKoncept * Koncept List)
      
    let valueKonceptToKoncept = Value 
    let abstractKonceptToKoncept ak = (ak, List.empty) |> Koncepts


    let toKonceptInformaiton koncept =
      match koncept with
      | Koncepts (AbstractKoncept ki,_) -> ki
      | Value (ValueKoncept ki) -> ki
         
    type ParentKoncept = | ParentKoncept of Koncept

    let add (ParentKoncept parent) koncept =
      match parent with
       | Value (ValueKoncept ki) ->  
            Koncepts (AbstractKoncept ki, [ koncept ])
       | Koncepts (ki,kl) ->
            Koncepts (ki , List.append kl [ koncept ]) 

    ///copy koncept
    let rec copy parent state =
        match state with
        | Koncepts (ak,koncepts) ->
            let newAk =  Koncepts (ak,[]) |> Some
            let acc = List.fold copy newAk koncepts 

            match parent with
            | None -> acc 
            | Some p -> 
                  match acc with
                  | None -> Some p
                  | Some child -> Some (add (ParentKoncept p) child)
        | Value v ->  
            match parent with 
            | Some p -> Some (add (ParentKoncept p) state)
            | None -> Some state


    let map f k =
          let rec map' (parent: Koncept option) state   =
            match state with
            | Koncepts (AbstractKoncept ki,koncepts) ->
                let newAk = f ki |> AbstractKoncept       
                let newKoncept =  newAk |>  abstractKonceptToKoncept |> Some
                let acc = List.fold map' newKoncept koncepts 
                
                match parent with
                  | None -> acc 
                  | Some p -> 
                      match acc with
                      | None -> Some p
                      | Some child -> Some (add (ParentKoncept p) child)
            | Value (ValueKoncept ki) ->  
                 let newValueKoncept = f ki |> ValueKoncept |> Value            
                 match parent with 
                  | Some p -> Some (add (ParentKoncept p) newValueKoncept)
                  | None -> Some newValueKoncept
          map' None k



   