module States exposing (..)

import Types exposing (..)
import Update exposing (shuffleUntilValid)
import Random exposing (Seed)


defaultModel : Flags -> ( Model, Cmd Msg )
defaultModel flags =
    ( shuffleUntilValid (initialModel flags), Cmd.none )


initialModel : Flags -> Model
initialModel flags =
    { people = flags.people
    , prohibitedPairIds = [ ( 4, 3 ) ]
    , nextPairs = []
    , currentPair = Maybe.Nothing
    , previousPairs = []
    , valid = False
    , randomSeed = Random.initialSeed (round flags.randomSeed)
    , currentUserState = Start
    , debug = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
