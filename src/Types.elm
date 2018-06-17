module Types exposing (..)

import Random exposing (Seed)
import Array exposing (..)


type Msg
    = NewRandom Int
    | ShowNewSanta
    | ShowRecipient
    

type PersonOfPair
    = First
     | Second

type alias Flags =
    { randomSeed : Float
    , people : List Person
    }


type CurrentUserState
    = NewSanta
    | SantaAndReciever
    | Finished
    | Start


type alias Person =
    { id : Int
    , firstName : String
    , surname : String
    }


type alias Pair =
    { firstPerson : Person
    , secondPerson : Person
    }


type alias Model =
    { people : List Person
    , nextPairs : List Pair
    , currentPair : Maybe Pair
    , previousPairs : List Pair
    , prohibitedPairIds : List ( Int, Int )
    , valid : Bool
    , randomSeed : Random.Seed
    , currentUserState : CurrentUserState
    , debug : Bool
    }
