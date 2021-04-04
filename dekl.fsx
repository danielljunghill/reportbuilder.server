open System

type Transaction =
    { 
        Instrument: string
        Price: decimal
        TimeStamp: DateTime
        Amount: int
    }

type Trade = 
    | Buy of Transaction
    | Sell of Transaction

type Holding =
    {
        Id: string
        Trades: Trade List
    }

type Holdings