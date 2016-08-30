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
