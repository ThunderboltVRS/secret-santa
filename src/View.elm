module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)
import Array exposing (..)


view : Model -> Html Msg
view model =
    display model


stylesheet : String -> Html a
stylesheet location =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" location
            ]

        children =
            []
    in
        node tag attrs children


display : Model -> Html Msg
display model =
    div [ align "center", class "fond", attribute "style" "padding-top:350px;" ]
        [ stylesheet "../UI/CSS/SnowScene.css"
        , stylesheet "../UI/CSS/Main.css"
        , div [ class "contener_home_one" ]
            [ div [ class "fireplace" ]
                [ text " " ]
            , div [ class "fireplace_top" ]
                [ text " " ]
            , div [ class "triangle" ]
                [ text " " ]
            , div [ class "parallelogram" ]
                [ text " " ]
            , div [ class "door" ]
                [ text " " ]
            , div [ class "window_one" ]
                [ text " " ]
            , div [ class "window_two" ]
                [ text " " ]
            , div [ class "home_base" ]
                [ text " " ]
            , div [ class "christmas_tree" ]
                []
            , div [ class "christmas_tree", attribute "style" "left:-140px;" ]
                []
            , div [ class "mountain_one" ]
                [ div [ class "sub_mountain_one" ]
                    [ text " " ]
                ]
            , div [ class "mountain_two" ]
                [ div [ class "sub_mountain_two" ]
                    [ text " " ]
                ]
            , div [ class "lutz" ]
                [ div [ class "lutin_pom" ]
                    [ text " " ]
                , div [ class "lutin_top" ]
                    [ text " " ]
                , div [ class "lutin_head" ]
                    [ text " " ]
                , div [ class "lutin_arm1" ]
                    [ text " " ]
                , div [ class "lutin_arm2" ]
                    [ text " " ]
                , div [ class "lutin_body" ]
                    [ text " " ]
                , div [ class "lutin_jb1" ]
                    [ text " " ]
                , div [ class "lutin_jb2" ]
                    [ text " " ]
                ]
            ]
        , div [ class "contener_snow" ]
            [ div [ class "snowflakes" ]
                [ div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                , div [ class "snowflake" ]
                    [ text " " ]
                ]
            ]
        , div [ attribute "style" " width:500px;height:9px; background-color:#ffffff; border-radius:5px;" ]
            [ text " " ]
        , div [ attribute "style" "padding-top:10px;" ]
            (if (model.debug) then
                List.append [ testTable model ] (interactionArea model)
             else
                interactionArea model
            )
        ]


testTable : Model -> Html a
testTable model =
    table []
        (List.map
            (\p ->
                tr []
                    [ td []
                        [ text (fullName p.firstPerson) ]
                    , td []
                        [ text (fullName p.secondPerson) ]
                    ]
            )
            model.nextPairs
        )


interactionArea : Model -> List (Html Msg)
interactionArea model =
    case model.currentUserState of
        NewSanta ->
            -- Show Santa + Receiver + Show
            [ div [] [ Html.text (getName First model.currentPair) ]
            , button [ class "button", onClick ShowRecipient ] [ text "Show Who I Buy For" ]
            ]

        SantaAndReciever ->
            -- Show Santa + Receiver + Next
            [ div [] [ Html.text (getName First model.currentPair) ]
            , div [] [ Html.text "Buys for:" ]
            , div [] [ Html.text (getName Second model.currentPair) ]
            , button [ class "button", onClick ShowNewSanta ] [ text "Next Secret Santa" ]
            ]

        Finished ->
            [ div [] [ Html.text "Finished" ] ]

        Start ->
            [ button [ class "button", onClick ShowNewSanta ] [ text "Start" ] ]


fullName : { a | firstName : String, surname : String } -> String
fullName { firstName, surname } =
    surname
        |> (++) " "
        |> (++) (firstName)


getName : PersonOfPair -> Maybe (Pair) -> String
getName personOfPair pair =
    case pair of
        Just p ->
            case personOfPair of
                First ->
                    fullName p.firstPerson

                Second ->
                    fullName p.secondPerson

        Nothing -> ""
