module Pizza exposing (..)


-- Model
type alias Id = Int

type alias Model =
    { name: String
    , votes: Int
    , id: Id
    }

model: Model
model =
    { name = ""
    , votes = 0
    , id = 0
    }

-- Update

newPizza: String -> Id -> Model
newPizza name id =
    { name = name
    , votes = 0
    , id = id
    }

modifyPizza: Id -> Int -> Model -> Model
modifyPizza id change pizza =
    if pizza.id == id then { pizza | votes = pizza.votes + change } else pizza
