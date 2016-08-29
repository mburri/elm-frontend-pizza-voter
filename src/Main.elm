import Html.App as App
import Html exposing (Html, text, div, input, h1, button, ul, li)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)

import String

main = App.beginnerProgram {model = model, view = view, update = update}

-- Model

type alias Pizza =
    { name: String
    , votes: Int
    }

type alias Model =
    { pizzas: List Pizza
    , new: String
    }

model: Model
model =
    { pizzas = []
    , new = ""
    }

newPizza: String -> Pizza
newPizza name =
    { name = name
    , votes = 0
    }

-- Update
type Msg
    = Add
    | UpdateField String
    | Like
    | Dislike

update: Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField str ->
            { model | new = str }
        Add ->
            { model
                | new = ""
                , pizzas =
                    if String.isEmpty model.new then
                        model.pizzas
                    else
                        model.pizzas ++ [newPizza model.new]
        }
        Like ->
            model
        Dislike ->
            model

-- View
view: Model -> Html Msg
view model =
    div []
        [ h1 [] [text "Pizza voter"]
        , input [placeholder "add your favorite pizza!", onInput UpdateField, value model.new] []
        , button [onClick Add] [text "add"]
        , viewAllPizzas model.pizzas
        ]

viewAllPizzas: List Pizza -> Html Msg
viewAllPizzas pizzas =
    ul []
       (List.map (\pizza -> li [] [text pizza.name]) pizzas)
