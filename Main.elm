module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Function exposing (compose)
import Elegant exposing (Style)
import Color


type alias Model =
    { monthlyRent : Int
    , collocs : Int
    , works : Int
    , rate : Float
    }


type Msg
    = UpdateCollocs String
    | UpdateMonthlyRent String
    | UpdateWorks String


defaultMonthlyRent : number
defaultMonthlyRent =
    300


defaultCollocs : number
defaultCollocs =
    3


init : ( Model, Cmd Msg )
init =
    ( { monthlyRent = defaultMonthlyRent
      , collocs = defaultCollocs
      , works = 0
      , rate = 0.0175
      }
    , Cmd.none
    )


rentaSimple : Float
rentaSimple =
    8.0


rentaColloc : Float
rentaColloc =
    10.0


renta : Int -> Float
renta collocs =
    if collocs > 1 then
        rentaColloc
    else
        rentaSimple


maxPrice : Model -> Float
maxPrice ({ collocs } as model) =
    (yearlyRent model |> toFloat) / ((renta collocs) / 100)


yearsOfDebt : number
yearsOfDebt =
    25


monthlyBankDebt : Model -> Int
monthlyBankDebt model =
    let
        k =
            model |> maxPrice

        t =
            model.rate

        n =
            yearsOfDebt * 12
    in
        (k * (t / 12)) / (1 - ((1 + t / 12) ^ (-n))) |> round


minSalary : Model -> Int
minSalary model =
    (model |> monthlyBankDebt) * 3


pad : Html.Attribute msg
pad =
    Html.Attributes.style [ ( "padding", "24px" ) ]


result : String -> a -> Html.Html msg
result label value =
    Html.div [ pad ]
        [ Html.text <| label
        , Html.br [] []
        , Html.text (value |> toString)
        ]


yearlyRent : Model -> Int
yearlyRent model =
    (totalMonthlyRent model) * 12


totalMonthlyRent : Model -> Int
totalMonthlyRent { collocs, monthlyRent } =
    monthlyRent * collocs


type alias Tree =
    { tag : String
    , content : List HtmlAttributes
    , style : List (Style -> Style)
    , hoverStyle : List (Style -> Style)
    }


type HtmlAttributes
    = HtmlAttributes Tree


base : HtmlAttributes
base =
    HtmlAttributes
        { tag = "div"
        , content = []
        , style = []
        , hoverStyle = []
        }


classes : a -> Html.Attribute msg
classes styles =
    Html.Attributes.class (Elegant.classes styles)


hoverClasses : a -> Html.Attribute msg
hoverClasses styles =
    Html.Attributes.class (Elegant.classesHover styles)


fold : (Tree -> b -> b) -> b -> HtmlAttributes -> b
fold fun answerSoFar (HtmlAttributes tree) =
    List.foldr
        (\e accu -> fold fun accu e)
        (fun tree answerSoFar)
        tree.content


getAllStyles : HtmlAttributes -> List ( List (Style -> Style), List (Style -> Style) )
getAllStyles tree =
    fold (\e acu -> ( e.style, e.hoverStyle ) :: acu) [] tree


htmlAttributesToCss : HtmlAttributes -> Html.Html msg
htmlAttributesToCss val =
    Elegant.stylesToCss (getAllStyles val)


htmlAttributesToHtml : HtmlAttributes -> Html.Html msg
htmlAttributesToHtml (HtmlAttributes val) =
    Html.node val.tag
        [ classes val.style
        , hoverClasses val.hoverStyle
        ]
        (val.content |> List.map htmlAttributesToHtml)


toHtml : HtmlAttributes -> Html.Html msg
toHtml val =
    Html.div []
        [ Html.node "style"
            []
            [ htmlAttributesToCss val ]
        , htmlAttributesToHtml val
        ]


tag : String -> HtmlAttributes -> HtmlAttributes
tag val (HtmlAttributes attrs) =
    HtmlAttributes { attrs | tag = val }


style : List (Elegant.Style -> Elegant.Style) -> HtmlAttributes -> HtmlAttributes
style val (HtmlAttributes attrs) =
    HtmlAttributes { attrs | style = val }


hoverStyle : List (Elegant.Style -> Elegant.Style) -> HtmlAttributes -> HtmlAttributes
hoverStyle val (HtmlAttributes attrs) =
    HtmlAttributes { attrs | hoverStyle = val }


content : List HtmlAttributes -> HtmlAttributes -> HtmlAttributes
content val (HtmlAttributes attrs) =
    HtmlAttributes { attrs | content = val }


div : HtmlAttributes -> HtmlAttributes
div =
    tag "div"


node : List (HtmlAttributes -> HtmlAttributes) -> HtmlAttributes
node htmlAttributesModifiers =
    base
        |> compose htmlAttributesModifiers


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ result "Renta standard en % : " (renta model.collocs)
        , toHtml
            (node
                [ div
                , content
                    [ node [ tag "div" ]
                    ]
                , style
                    [ Elegant.textColor Color.black ]
                , hoverStyle
                    [ Elegant.textColor Color.white ]
                ]
            )
        , Html.div [ pad ]
            [ Html.div []
                [ Html.text
                    ("Loyer mensuel "
                        ++ (if model.collocs > 1 then
                                " (par colloc)"
                            else
                                ""
                           )
                    )
                ]
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (model.monthlyRent |> toString)
                , Html.Events.onInput UpdateMonthlyRent
                ]
                []
            ]
        , Html.div [ pad ]
            [ Html.div [] [ Html.text "Nombre collocs (mettre à 1 si location simple)" ]
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (model.collocs |> toString)
                , Html.Events.onInput UpdateCollocs
                ]
                []
            ]
        , Html.div [ pad ]
            [ Html.div [] [ Html.text "Travaux" ]
            , Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (model.works |> toString)
                , Html.Events.onInput UpdateWorks
                ]
                []
            ]
        , result "Loyer mensuel total : " (totalMonthlyRent model)
        , result "Loyer annuel : " (yearlyRent model)
        , result "Prix d'acquisition global (travaux compris) max conseillé : " (maxPrice model)
        , result "Prix d'acquisition global (sans travaux) max conseillé : " (maxPrice model - (model.works |> toFloat))
        , result "Prix d'acquisition global (avant frais notaires) max conseillé : " ((maxPrice model - (model.works |> toFloat)) / 1.08)
        , result "Mensualités moyennes à payer à la banque (20 ans) : " (monthlyBankDebt model)
        , result "Revenus minimum pour endettement : " (minSalary model)
        ]


toPositiveInt : Int -> Int
toPositiveInt i =
    if i < 1 then
        1
    else
        i


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMonthlyRent monthlyRent ->
            ( { model | monthlyRent = (monthlyRent |> String.toInt |> Result.withDefault defaultMonthlyRent) }, Cmd.none )

        UpdateCollocs collocs ->
            ( { model | collocs = (collocs |> String.toInt |> Result.withDefault defaultCollocs |> toPositiveInt) }, Cmd.none )

        UpdateWorks works ->
            ( { model | works = (works |> String.toInt |> Result.withDefault 0) }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        ({ init = init
         , view = view
         , subscriptions = (\e -> Sub.none)
         , update = update
         }
        )



-- assurance : Generali
