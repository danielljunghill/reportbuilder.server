open System
#load "model.fs"

open Model 
type Prime = { Numbers: int NList }

type PrimeNumberGenerator = PrimeNumberGenerator of (Prime -> Prime)


let init =
    {
        Numbers = NList.create 2
    }


let tryGetLastNumber numbers =
    match numbers with
     |   [] -> None
     |   head :: tail -> Some head


let rec trytNumberAsPrime n numbers =
        match numbers with
        |   [] ->   
                n
                |> Some 
        |    head :: tail ->
                if (n % head) = 0 then None
                else trytNumberAsPrime n tail

List.fold

let generatePrime prime  =
    let numbers = prime.Numbers |> NList.toList
    match tryGetLastNumber numbers with
        | None -> { Numbers = NList.create 2}
        | Some nr -> 
    
                let rec getNextPrimeNumber nr  =
                    match trytNumberAsPrime nr numbers with
                       | Some pn -> { prime with Numbers = NList.addFirst pn prime.Numbers}
                       | None -> getNextPrimeNumber (nr + 1) 
         
                getNextPrimeNumber (nr + 1)

let first = generatePrime init

type PrimeResult<'a> =
    {
       Prime: Prime
       Value: 'a
    }

type Factor = Factor of int
module Factor =
    let fromPrime (prime: Prime)  =
          Factor prime.Numbers.Head

type Factored<'a> = 
    {
        Factor: Factor
        Value: 'a
    }

// let createFactored p a =
//     let newPrime = generatePrime p
//     { 
//             Factor = Factor.fromPrime newPrime
//             Value = a 
//      },  newPrime
   
type ValueKoncept = 
    {
        Name: String
        DataType: String
        Factor: Factor
    }



type Member=
     {
        Name: String
        Factor: Factor
    }


let createMember prime name =
    let newPrime = generatePrime prime
    newPrime,
    {
        Name = name
        Factor = Factor.fromPrime prime
    }

let t (p1,a1) (p2,a2) =
    (p2, a1 :: a2)
let createMembers prime (m: String List) =
    let rec create p h =
        match h with
        | [] -> (p,[])
        | head :: tail ->
            let (np,nm) = createMember p head
            t (np,nm) (create np tail)

    create prime m

let prime, m = createMembers init [ "1"; "2"; "3"; "4"]
       
let prime2 ,m2 = createMembers prime [ "Norge"; "Sverige"]
let prime3 ,m3 = createMembers prime2 [ "Kv1"; "Kv2"; "kv3"; "kv4"]
let prime4 ,m4 = createMembers prime3 [ "Kv1"; "Kv2"; "kv3"; "kv4"]
let prime5 ,m5 = createMembers prime4 [ "2001"; "2002"; "2003"; "2004"]
let prime6 ,m6 = createMembers prime5 [ "region1"; "region2"; "region3"; "region4"]
let prime7 ,m7 = createMembers prime5 [ "region1"; "region2"; "region3"; "region4";"region1"; "region2"; "region3"; "region4";"region1"; "region2"; "region3"; "region4";"region1"; "region2"; "region3"; "region4";"region1"; "region2"; "region3"; "region4"]
let p = generatePrime prime7
let p1 = generatePrime p
let p2 = generatePrime init
let p3 = generatePrime p2


// type Prime =
//      { Numbers: int List}

// type Primed<'a,'b> = Primed of (Prime -> 'a -> ('b * Prime))

// type PrimeGenerator = 
            
        
