module Seksjon.Utdanning exposing
    ( OppsummeringInfo
    , RetningInfo
    , SluttTidInfo
    , StartTidInfo
    , Steg
    , UtdanningSlutt(..)
    , UtdanningSluttRedigering(..)
    , Utdanningsnivå(..)
    , Utfyllingssteg(..)
    , init
    , inputFraRetning
    , lagreUtdanningsretning
    , monthFraRedigererUtdanningFerdig
    , oppdaterRetningInput
    , oppdaterSluttMåned
    , oppdaterSluttÅr
    , oppdaterStartMåned
    , oppdaterStartÅr
    , sluttFraOppsummering
    , startMonthFraStartTidInfo
    , startMånedFraOppsummering
    , startYearFraStartTidInfo
    , startÅrFraOppsummering
    , utdanningAvsluttet
    , utdanningIkkeAvsluttet
    , utdanningSluttTid
    , utdanningsNivåToString
    , utdanningsnivåFraOppsummering
    , utdanningsnivåFraRetning
    , utdanningsretningFraOppsummering
    , utfyllingssteg
    , velgUtdanningSlutt
    , velgUtdanningStart
    , velgUtdanningsnivå
    , yearFraRedigererUtdanningFerdig
    )


type Steg
    = Steg Steginfo


type alias Steginfo =
    { steg : Utfyllingssteg
    }


type Utdanningsnivå
    = Grunnskole
    | Folkehøyskole
    | VideregåendeEllerYrkesskole
    | Fagskole
    | Bachelor
    | Master
    | PhD


utdanningsNivåToString : Utdanningsnivå -> String
utdanningsNivåToString utdanningsnivå =
    case utdanningsnivå of
        Grunnskole ->
            "Grunnskole"

        Folkehøyskole ->
            "Folkehøyskole"

        VideregåendeEllerYrkesskole ->
            "Videregående/Yrkesskole"

        Fagskole ->
            "Fagskole"

        Bachelor ->
            "Høyere utdanning, 1-4 år"

        Master ->
            "Høyere utdanning, 4+ år"

        PhD ->
            "PhD"


type RetningInfo
    = RetningInfo { nivå : Utdanningsnivå, input : String }


type StartTidInfo
    = StartTidInfo { forrige : RetningInfo, startMonth : String, startYear : String }


type RedigererUtdanningFerdigInfo
    = RedigererUtdanningFerdigInfo { month : String, year : String }


type UtdanningSluttRedigering
    = IkkeSvart
    | RedigererUtdanningFerdig RedigererUtdanningFerdigInfo


type UtdanningSlutt
    = Pågår
    | Ferdig { month : String, year : String }


type SluttTidInfo
    = SluttTidInfo { forrige : StartTidInfo, slutt : UtdanningSluttRedigering }


type OppsummeringInfo
    = OppsummeringInfo { startTidInfo : StartTidInfo, slutt : UtdanningSlutt }


type Utfyllingssteg
    = Nivå
    | Retning RetningInfo
    | StartTid StartTidInfo
    | SluttTid SluttTidInfo
    | Oppsummering OppsummeringInfo


utdanningsnivåFraRetning : RetningInfo -> Utdanningsnivå
utdanningsnivåFraRetning (RetningInfo { nivå }) =
    nivå


inputFraRetning : RetningInfo -> String
inputFraRetning (RetningInfo { input }) =
    input


setUtfyllingssteg : Steg -> Utfyllingssteg -> Steg
setUtfyllingssteg steg usteg =
    Steg { steg = usteg }


oppdaterRetningInput : Steg -> String -> Steg
oppdaterRetningInput ((Steg { steg }) as hovedSteg) input =
    case steg of
        Retning (RetningInfo info) ->
            setUtfyllingssteg hovedSteg <| Retning (RetningInfo { info | input = input })

        _ ->
            hovedSteg


lagreUtdanningsretning : Steg -> Steg
lagreUtdanningsretning ((Steg { steg }) as hovedSteg) =
    case steg of
        Retning info ->
            setUtfyllingssteg hovedSteg <| StartTid (StartTidInfo { forrige = info, startMonth = "0", startYear = "1990" })

        _ ->
            hovedSteg


oppdaterStartMåned : Steg -> String -> Steg
oppdaterStartMåned ((Steg { steg }) as hovedSteg) s =
    case steg of
        StartTid (StartTidInfo info) ->
            setUtfyllingssteg hovedSteg <| StartTid (StartTidInfo { info | startMonth = s })

        _ ->
            hovedSteg


oppdaterStartÅr : Steg -> String -> Steg
oppdaterStartÅr ((Steg { steg }) as hovedSteg) s =
    case steg of
        StartTid (StartTidInfo info) ->
            setUtfyllingssteg hovedSteg <| StartTid (StartTidInfo { info | startYear = s })

        _ ->
            hovedSteg


oppdaterSluttMåned : Steg -> String -> Steg
oppdaterSluttMåned ((Steg { steg }) as hovedSteg) s =
    case steg of
        SluttTid (SluttTidInfo info) ->
            case info.slutt of
                RedigererUtdanningFerdig (RedigererUtdanningFerdigInfo sluttInfo) ->
                    setUtfyllingssteg hovedSteg <| SluttTid (SluttTidInfo { info | slutt = RedigererUtdanningFerdig (RedigererUtdanningFerdigInfo { sluttInfo | month = s }) })

                _ ->
                    hovedSteg

        _ ->
            hovedSteg


oppdaterSluttÅr : Steg -> String -> Steg
oppdaterSluttÅr ((Steg { steg }) as hovedSteg) s =
    case steg of
        SluttTid (SluttTidInfo info) ->
            case info.slutt of
                RedigererUtdanningFerdig (RedigererUtdanningFerdigInfo sluttInfo) ->
                    setUtfyllingssteg hovedSteg <| SluttTid (SluttTidInfo { info | slutt = RedigererUtdanningFerdig (RedigererUtdanningFerdigInfo { sluttInfo | year = s }) })

                _ ->
                    hovedSteg

        _ ->
            hovedSteg


velgUtdanningSlutt : Steg -> Steg
velgUtdanningSlutt ((Steg { steg }) as hovedSteg) =
    case steg of
        SluttTid (SluttTidInfo { forrige, slutt }) ->
            case slutt of
                IkkeSvart ->
                    hovedSteg

                RedigererUtdanningFerdig redigererUtdanningFerdigInfo ->
                    setUtfyllingssteg hovedSteg <| Oppsummering <| OppsummeringInfo { startTidInfo = forrige, slutt = toUtdanningSlutt redigererUtdanningFerdigInfo }

        _ ->
            hovedSteg


toUtdanningSlutt : RedigererUtdanningFerdigInfo -> UtdanningSlutt
toUtdanningSlutt (RedigererUtdanningFerdigInfo { month, year }) =
    Ferdig { month = month, year = year }


velgUtdanningStart : Steg -> Steg
velgUtdanningStart ((Steg { steg }) as hovedSteg) =
    case steg of
        StartTid info ->
            setUtfyllingssteg hovedSteg <|
                SluttTid
                    (SluttTidInfo
                        { forrige = info
                        , slutt = IkkeSvart
                        }
                    )

        _ ->
            hovedSteg


utdanningAvsluttet : Steg -> Steg
utdanningAvsluttet ((Steg { steg }) as hovedSteg) =
    case steg of
        SluttTid (SluttTidInfo info) ->
            setUtfyllingssteg hovedSteg <|
                SluttTid
                    (SluttTidInfo
                        { info
                            | slutt =
                                RedigererUtdanningFerdig <|
                                    RedigererUtdanningFerdigInfo
                                        { month = startMonthFraStartTidInfo info.forrige
                                        , year = startYearFraStartTidInfo info.forrige
                                        }
                        }
                    )

        _ ->
            hovedSteg


utdanningIkkeAvsluttet : Steg -> Steg
utdanningIkkeAvsluttet ((Steg { steg }) as hovedSteg) =
    case steg of
        SluttTid (SluttTidInfo { forrige }) ->
            setUtfyllingssteg hovedSteg <|
                Oppsummering
                    (OppsummeringInfo
                        { slutt = Pågår
                        , startTidInfo = forrige
                        }
                    )

        _ ->
            hovedSteg


utdanningSluttTid : SluttTidInfo -> UtdanningSluttRedigering
utdanningSluttTid (SluttTidInfo { slutt }) =
    slutt


startMonthFraStartTidInfo : StartTidInfo -> String
startMonthFraStartTidInfo (StartTidInfo { startMonth }) =
    startMonth


startYearFraStartTidInfo : StartTidInfo -> String
startYearFraStartTidInfo (StartTidInfo { startYear }) =
    startYear


monthFraRedigererUtdanningFerdig : RedigererUtdanningFerdigInfo -> String
monthFraRedigererUtdanningFerdig (RedigererUtdanningFerdigInfo { month }) =
    month


yearFraRedigererUtdanningFerdig : RedigererUtdanningFerdigInfo -> String
yearFraRedigererUtdanningFerdig (RedigererUtdanningFerdigInfo { year }) =
    year


utfyllingssteg : Steg -> Utfyllingssteg
utfyllingssteg (Steg { steg }) =
    steg


utdanningsnivåFraOppsummering : OppsummeringInfo -> Utdanningsnivå
utdanningsnivåFraOppsummering (OppsummeringInfo { startTidInfo }) =
    case startTidInfo of
        StartTidInfo { forrige } ->
            case forrige of
                RetningInfo { nivå } ->
                    nivå


utdanningsretningFraOppsummering : OppsummeringInfo -> String
utdanningsretningFraOppsummering (OppsummeringInfo { startTidInfo }) =
    case startTidInfo of
        StartTidInfo { forrige } ->
            case forrige of
                RetningInfo { input } ->
                    input


startMånedFraOppsummering : OppsummeringInfo -> String
startMånedFraOppsummering (OppsummeringInfo { startTidInfo }) =
    case startTidInfo of
        StartTidInfo { startMonth } ->
            startMonth


startÅrFraOppsummering : OppsummeringInfo -> String
startÅrFraOppsummering (OppsummeringInfo { startTidInfo }) =
    case startTidInfo of
        StartTidInfo { startYear } ->
            startYear


sluttFraOppsummering : OppsummeringInfo -> UtdanningSlutt
sluttFraOppsummering (OppsummeringInfo { slutt }) =
    slutt


init : Steg
init =
    Steg { steg = Nivå }


velgUtdanningsnivå : Steg -> Utdanningsnivå -> Steg
velgUtdanningsnivå steg utdanningsnivå =
    Steg { steg = Retning <| RetningInfo { nivå = utdanningsnivå, input = "" } }
