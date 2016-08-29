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
    , id: Int
    }

type alias Model =
    { pizzas: List Pizza
    , new: String
    , uid: Int
    }

model: Model
model =
    { pizzas = []
    , new = ""
    , uid = 0
    }

newPizza: String -> Int -> Pizza
newPizza name id =
    { name = name
    , votes = 0
    , id = id
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
                , uid = model.uid + 1
                , pizzas =
                    if String.isEmpty model.new then
                        model.pizzas
                    else
                        model.pizzas ++ [newPizza model.new model.uid]
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
       (List.map viewPizza pizzas)

viewPizza: Pizza -> Html Msg
viewPizza pizza =
    li []
       [ text (toString pizza.id)
       , text pizza.name
       , button [onClick Dislike] [text "-"]
       , text (toString pizza.votes)
       , button [onClick Like] [text "+"]
       ]
