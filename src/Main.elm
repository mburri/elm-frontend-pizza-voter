module Main exposing (..)

import Pizza

import Html.App as App
import Html exposing (Html, text, div, input, h1, button, ul, li)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)

import String

main = App.beginnerProgram {model = model, view = view, update = update}

-- Model

type alias Id = Int


type alias Model =
    { pizzas: List Pizza.Model
    , new: String
    , uid: Id
    }

model: Model
model =
    { pizzas = []
    , new = ""
    , uid = 0
    }

-- Update
type Msg
    = Add
    | UpdateField String
    | Like Int
    | Dislike Int

update: Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField str ->
            { model | new = str }
        Add ->
            addPizza model
        Like id ->
            { model | pizzas = List.map (Pizza.modifyPizza id 1) model.pizzas }
        Dislike id ->
            { model | pizzas = List.map (Pizza.modifyPizza id -1) model.pizzas }



addPizza: Model -> Model
addPizza model =
    { model
        | new = ""
        , uid = model.uid + 1
        , pizzas =
            if String.isEmpty model.new then
                model.pizzas
            else
                model.pizzas ++ [Pizza.newPizza model.new model.uid]
    }

-- View
view: Model -> Html Msg
view model =
    div []
        [ h1 [] [text "Pizza voter"]
        , input [placeholder "add your favorite pizza!", onInput UpdateField, value model.new] []
        , button [onClick Add] [text "add"]
        , viewAllPizzas model.pizzas
        ]

viewAllPizzas: List Pizza.Model -> Html Msg
viewAllPizzas pizzas =
    let
        sortByVotes a b =
            case compare a.votes b.votes of
              LT -> GT
              EQ -> EQ
              GT -> LT
    in
        ul []
           (List.map viewPizza (List.sortWith sortByVotes pizzas))

viewPizza: Pizza.Model -> Html Msg
viewPizza pizza =
    li []
       [ text (toString pizza.id)
       , text pizza.name
       , button [onClick (Dislike pizza.id)] [text "-"]
       , text (toString pizza.votes)
       , button [onClick (Like pizza.id)] [text "+"]
       ]
