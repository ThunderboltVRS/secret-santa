module Update exposing (..)

import Types exposing (..)
import Random exposing (..)
import RandomUtil exposing (shuffle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom number ->
            ( shuffleUntilValid { model | randomSeed = Random.initialSeed number }, Cmd.none )

        ShowNewSanta ->
            ( shiftPair (showNewSanta model), Cmd.none )

        ShowRecipient ->
            ( showRecipient model, Cmd.none )


showNewSanta : Model -> Model
showNewSanta model =
    case model.currentUserState of
        NewSanta ->
            model

        SantaAndReciever ->
            case model.nextPairs of
                [] ->
                    { model | currentUserState = Finished }

                a ->
                    { model | currentUserState = NewSanta }

        Finished ->
            model

        Start ->
            { model | currentUserState = NewSanta }


showRecipient : Model -> Model
showRecipient model =
    case model.currentUserState of
        NewSanta ->
            { model | currentUserState = SantaAndReciever }

        SantaAndReciever ->
            model

        Finished ->
            model

        Start ->
            model


pairEqual : Pair -> Bool
pairEqual pair =
    pair.firstPerson.id == pair.secondPerson.id


prohibited : ( Int, Int ) -> Pair -> Bool
prohibited tupleA pairB =
    let
        tupleB =
            ( pairB.firstPerson.id, pairB.secondPerson.id )
    in
        tupleA == tupleB


validModel : Model -> Bool
validModel model =
    validPairs model


allPairsCreated : Model -> Bool
allPairsCreated model =
    List.length model.nextPairs >= List.length model.people


validPairs : Model -> Bool
validPairs model =
    List.all (\p -> validPair model p) (model.nextPairs)


validPair : Model -> Pair -> Bool
validPair model pair =
    not (pairEqual pair || pairProhibited model pair)


pairProhibited : Model -> Pair -> Bool
pairProhibited model pair =
    if (List.isEmpty model.prohibitedPairIds) then
        False
    else
        List.any (\p -> prohibited p pair) model.prohibitedPairIds


shuffleUntilValid : Model -> Model
shuffleUntilValid model =
    let
        shuffleResult =
            shufflePeople (model.randomSeed) model.people

        newModel =
            { model | nextPairs = Tuple.first shuffleResult, randomSeed = Tuple.second shuffleResult }
    in
        if (allPairsCreated model && validModel newModel) then
            { newModel | valid = True }
        else
            shuffleUntilValid newModel


shufflePeople : Random.Seed -> List Person -> ( List Pair, Random.Seed )
shufflePeople seed people =
    let
        first =
            shuffle people

        second =
            shuffle people

        tuple1 =
            (Random.step first seed)

        newSeed =
            Tuple.second tuple1

        tuple2 =
            (Random.step second newSeed)

        newPairs =
            createPairs (tuple1 |> Tuple.first) (tuple2 |> Tuple.first) []
    in
        ( newPairs, (tuple2 |> Tuple.second) )


createPairs : List Person -> List Person -> List Pair -> List Pair
createPairs first second current =
    if (List.isEmpty first || List.isEmpty second) then
        current
    else
        case (List.head first) of
            Just f ->
                case (List.head second) of
                    Just s ->
                        createPairs (getTail first) (getTail second) (List.append [ (createPair f s) ] current)

                    Nothing ->
                        []

            Nothing ->
                []


getTail : List a -> List a
getTail list =
    case (List.tail list) of
        Just l ->
            l

        Nothing ->
            []


createPair : Person -> Person -> Pair
createPair personA personB =
    { firstPerson = personA
    , secondPerson = personB
    }


shiftPair : Model -> Model
shiftPair model =
    let
        pair =
            List.head model.nextPairs

        oldPair =
            model.currentPair
    in
        case oldPair of
            Just op ->
                { model
                    | nextPairs = getTail model.nextPairs
                    , currentPair = pair
                    , previousPairs = List.append model.previousPairs [ op ]
                }
            Nothing ->
                 { model
                    | nextPairs = getTail model.nextPairs
                    , currentPair = List.head model.nextPairs
                }
