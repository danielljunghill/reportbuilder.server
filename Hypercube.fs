namespace ReportDefinition
module Dimensions =
    open System
    type DomainName = DomainName of string
    type DomainMember = DomainMember of string
    type DefaultMember = DefaultMember of string  

    type Domain =
        {
            Name: DomainName
            Members: DomainMember List
        }

    module Domain =
        let empty name = { Name = name; Members = []}
        let addMember m (domain: Domain) = { domain with Members = List.append domain.Members [ m ] }


    type Dimension =
         | DimensionWithDefault of DefaultMember * Domain
         | DimensionWithoutDefault of Domain

    module Dimension =
        let createWithDefault defaultmember domain = DimensionWithDefault (defaultmember,domain) 
        let createWithoutDefault = DimensionWithoutDefault
        
    type HyperDimension =
         | Opened of Dimension
         | Closed of Dimension

    module HyperDimension =
        let openedWithDefault defaultmember domain = DimensionWithDefault (defaultmember,domain) |> Opened
        let closedWithDefault = DimensionWithoutDefault >> Closed
        let openedWithoutDefault = DimensionWithoutDefault >> Opened
    type HyperCubeName = string

    type HyperCube =
        {
                Name: HyperCubeName
                Head: HyperDimension
                Tail: HyperDimension List
        }

    module HyperCube =
        let create hyperCubeName hyperDimension =
                {
                    Name = hyperCubeName
                    Head = hyperDimension
                    Tail = []
                }

        let addDimension dimension (cube: HyperCube) = { cube with Tail = List.append cube.Tail [ dimension ]}
        let dimension (cube: HyperCube) = List.append [ cube.Head ] cube.Tail
