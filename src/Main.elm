module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Seksjon.Utdanning as Utdanning



---- MODEL ----


type Model
    = Intro
    | Seksjonsvalg SeksjonsvalgState
    | UtdanningSekjson Utdanning.Steg


type Seksjonsforslag
    = Utdanningforslag


type SeksjonsvalgState
    = Forslag Seksjonsforslag
    | AlleSeksjoner


init : ( Model, Cmd Msg )
init =
    ( Intro, Cmd.none )



---- UPDATE ----


type Msg
    = IntroduksjonFerdig
    | UtdanningSeksjonValgt
    | UtdanningSeksjonValgtIkkeNå
    | UtdanningsMsg UtdanningsMsg
    | IngenUtdanning


type UtdanningsMsg
    = UtdanningsnivåValgt Utdanning.Utdanningsnivå
    | UtdanningsretningOppdatert String
    | UtdanningsretningLagret
    | UtdanningsStartMånedOppdatert String
    | UtdanningsStartÅrOppdatert String
    | UtdanningsStartValgt
    | UtdanningAvsluttetValgt
    | UtdanningIkkeAvsluttetValgt
    | UtdanningsSluttMånedOppdatert String
    | UtdanningsSluttÅrOppdatert String
    | UtdanningsSluttValgt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IntroduksjonFerdig ->
            ( Seksjonsvalg <| Forslag <| Utdanningforslag, Cmd.none )

        UtdanningSeksjonValgt ->
            ( UtdanningSekjson <| Utdanning.init, Cmd.none )

        UtdanningSeksjonValgtIkkeNå ->
            ( Seksjonsvalg <| AlleSeksjoner, Cmd.none )

        IngenUtdanning ->
            ( Seksjonsvalg <| AlleSeksjoner, Cmd.none )

        UtdanningsMsg utdanningsMsg ->
            updateUtdanning utdanningsMsg model


updateUtdanning : UtdanningsMsg -> Model -> ( Model, Cmd Msg )
updateUtdanning utdanningsMsg model =
    case model of
        UtdanningSekjson steg ->
            case utdanningsMsg of
                UtdanningsnivåValgt utdanningsnivå ->
                    ( Utdanning.velgUtdanningsnivå steg utdanningsnivå
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsretningOppdatert s ->
                    ( Utdanning.oppdaterRetningInput steg s
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsretningLagret ->
                    ( Utdanning.lagreUtdanningsretning steg
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsStartMånedOppdatert s ->
                    ( Utdanning.oppdaterStartMåned steg s
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsStartÅrOppdatert s ->
                    ( Utdanning.oppdaterStartÅr steg s
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsStartValgt ->
                    ( Utdanning.velgUtdanningStart steg
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningAvsluttetValgt ->
                    ( Utdanning.utdanningAvsluttet steg
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningIkkeAvsluttetValgt ->
                    ( Utdanning.utdanningIkkeAvsluttet steg
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsSluttMånedOppdatert s ->
                    ( Utdanning.oppdaterSluttMåned steg s
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsSluttÅrOppdatert s ->
                    ( Utdanning.oppdaterSluttÅr steg s
                        |> UtdanningSekjson
                    , Cmd.none
                    )

                UtdanningsSluttValgt ->
                    ( Utdanning.velgUtdanningSlutt steg
                        |> UtdanningSekjson
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Intro ->
            div []
                [ p [] <| [ text "Hei {navn}! Jeg heter CeVerin, og jeg kan hjelpe deg å fylle ut CV-en din 😊" ]
                , p [] <| [ text "Passer det å fylle ut litt av CV-en din nå?" ]
                , button [ onClick IntroduksjonFerdig ] [ text "Ja, det passer" ]
                , a [ href "https://google.com" ] [ text "Nei, ta meg til CV-en min" ]
                ]

        Seksjonsvalg state ->
            viewSeksjonsvalg state

        UtdanningSekjson steg ->
            viewUtdanningseksjon steg


viewSeksjonsvalg : SeksjonsvalgState -> Html Msg
viewSeksjonsvalg seksjonsvalgState =
    case seksjonsvalgState of
        Forslag seksjonsforslag ->
            div []
                [ p [] <| [ text "Jeg ser at du ikke har fylt ut noen utdanning, skal vi starte med å fylle ut det?" ]
                , button [ onClick UtdanningSeksjonValgt ] [ text "Ja, la oss fylle ut" ]
                , button [ onClick UtdanningSeksjonValgtIkkeNå ] [ text "Ikke nå" ]
                , button [ onClick IngenUtdanning ] [ text "Jeg har ingen utdanning" ]
                ]

        AlleSeksjoner ->
            div []
                [ p [] <| [ text "Jeg har dessverre bare støtte for å legge inn utdanning akkurat nå, beklager det." ]
                ]


viewUtdanningseksjon : Utdanning.Steg -> Html Msg
viewUtdanningseksjon steg =
    case Utdanning.utfyllingssteg steg of
        Utdanning.Nivå ->
            viewUtdanningsnivå

        Utdanning.Retning info ->
            viewUtdanningsretning info

        Utdanning.StartTid info ->
            viewUtdanningStartTid info

        Utdanning.SluttTid info ->
            viewUtdanningSluttTid info

        Utdanning.Oppsummering info ->
            viewOppsummering info


viewUtdanningsnivå : Html Msg
viewUtdanningsnivå =
    div []
        [ p [] <| [ text "La oss starte med den siste utdanningen du tok eller fortsatt holder på med." ]
        , p [] <| [ text "Hvilket utdanningsnivå var utdanningen?" ]
        , p [] <| [ utdanningsnivåButton Utdanning.Grunnskole ]
        , p [] <| [ utdanningsnivåButton Utdanning.Folkehøyskole ]
        , p [] <| [ utdanningsnivåButton Utdanning.VideregåendeEllerYrkesskole ]
        , p [] <| [ utdanningsnivåButton Utdanning.Fagskole ]
        , p [] <| [ utdanningsnivåButton Utdanning.Bachelor ]
        , p [] <| [ utdanningsnivåButton Utdanning.Master ]
        , p [] <| [ utdanningsnivåButton Utdanning.PhD ]
        ]


utdanningsnivåButton : Utdanning.Utdanningsnivå -> Html Msg
utdanningsnivåButton nivå =
    button [ onClick <| UtdanningsMsg <| UtdanningsnivåValgt nivå ] [ text <| Utdanning.utdanningsNivåToString nivå ]


viewUtdanningsretning : Utdanning.RetningInfo -> Html Msg
viewUtdanningsretning retningInfo =
    div []
        [ p []
            [ retningInfo
                |> Utdanning.utdanningsnivåFraRetning
                |> Utdanning.utdanningsNivåToString
                |> (++) "Du valgte "
                |> text
            ]
        , p [] [ text "Hvilken utdanningsretning var utdanningen?" ]
        , p []
            [ input
                [ value <| Utdanning.inputFraRetning retningInfo
                , onInput (UtdanningsMsg << UtdanningsretningOppdatert)
                , placeholder "F.eks. \"Informatikk\" eller \"Psykologi\""
                ]
                []
            , button [ onClick <| UtdanningsMsg UtdanningsretningLagret ] [ text "Velg" ]
            ]
        ]


viewUtdanningStartTid : Utdanning.StartTidInfo -> Html Msg
viewUtdanningStartTid startTidInfo =
    div []
        [ text "Når startet du utdanningen?"
        , select [ onInput <| (UtdanningsMsg << UtdanningsStartMånedOppdatert) ]
            [ option [ value "1", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "1" ] [ text "Januar" ]
            , option [ value "2", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "2" ] [ text "Februar" ]
            , option [ value "3", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "3" ] [ text "Mars" ]
            , option [ value "4", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "4" ] [ text "April" ]
            , option [ value "5", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "5" ] [ text "Mai" ]
            , option [ value "6", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "6" ] [ text "Juni" ]
            , option [ value "7", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "7" ] [ text "Juli" ]
            , option [ value "8", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "8" ] [ text "August" ]
            , option [ value "9", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "9" ] [ text "September" ]
            , option [ value "10", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "10" ] [ text "Oktober" ]
            , option [ value "11", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "11" ] [ text "November" ]
            , option [ value "12", selected <| Utdanning.startMonthFraStartTidInfo startTidInfo == "12" ] [ text "Desember" ]
            ]
        , input [ value <| Utdanning.startYearFraStartTidInfo startTidInfo, onInput <| (UtdanningsMsg << UtdanningsStartÅrOppdatert) ] []
        , button [ onClick <| UtdanningsMsg UtdanningsStartValgt ] [ text "Videre" ]
        ]


viewUtdanningSluttTid : Utdanning.SluttTidInfo -> Html Msg
viewUtdanningSluttTid sluttTidInfo =
    case Utdanning.utdanningSluttTid sluttTidInfo of
        Utdanning.IkkeSvart ->
            div []
                [ text "Er du ferdig med utdanningen?"
                , button [ onClick <| UtdanningsMsg UtdanningAvsluttetValgt ] [ text "Ja" ]
                , button [ onClick <| UtdanningsMsg UtdanningIkkeAvsluttetValgt ] [ text "Nei, ikke ennå" ]
                ]

        Utdanning.RedigererUtdanningFerdig info ->
            div []
                [ text "Når gjorde du ferdig med utdanningen din?"
                , select [ onInput <| (UtdanningsMsg << UtdanningsSluttMånedOppdatert) ]
                    [ option [ value "1", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "1" ] [ text "Januar" ]
                    , option [ value "2", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "2" ] [ text "Februar" ]
                    , option [ value "3", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "3" ] [ text "Mars" ]
                    , option [ value "4", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "4" ] [ text "April" ]
                    , option [ value "5", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "5" ] [ text "Mai" ]
                    , option [ value "6", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "6" ] [ text "Juni" ]
                    , option [ value "7", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "7" ] [ text "Juli" ]
                    , option [ value "8", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "8" ] [ text "August" ]
                    , option [ value "9", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "9" ] [ text "September" ]
                    , option [ value "10", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "10" ] [ text "Oktober" ]
                    , option [ value "11", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "11" ] [ text "November" ]
                    , option [ value "12", selected <| Utdanning.monthFraRedigererUtdanningFerdig info == "12" ] [ text "Desember" ]
                    ]
                , input [ value <| Utdanning.yearFraRedigererUtdanningFerdig info, onInput <| (UtdanningsMsg << UtdanningsSluttÅrOppdatert) ] []
                , button [ onClick <| UtdanningsMsg UtdanningsSluttValgt ] [ text "Videre" ]
                ]


viewOppsummering : Utdanning.OppsummeringInfo -> Html Msg
viewOppsummering oppsummeringInfo =
    div []
        [ text "Du har skrevet inn følgende, har du lyst til å lagre dette i CVen din?"
        , oppsummeringInfo
            |> Utdanning.utdanningsnivåFraOppsummering
            |> Utdanning.utdanningsNivåToString
            |> text
            |> List.singleton
            |> p []
        , oppsummeringInfo
            |> Utdanning.utdanningsretningFraOppsummering
            |> text
            |> List.singleton
            |> p []
        , Utdanning.startMånedFraOppsummering oppsummeringInfo
            ++ " "
            ++ Utdanning.startÅrFraOppsummering oppsummeringInfo
            |> text
            |> List.singleton
            |> p []
        , case Utdanning.sluttFraOppsummering oppsummeringInfo of
            Utdanning.Pågår ->
                text "Pågår"
                    |> List.singleton
                    |> p []

            Utdanning.Ferdig { month, year } ->
                text (month ++ " " ++ year)
                    |> List.singleton
                    |> p []
        , p []
            [ button [] [ text "Lagre" ]
            , button [] [ text "Avbryt" ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
