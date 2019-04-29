module MaybeWork exposing (User, canBuyAlcohol)


type alias User =
    { name : String, age : Maybe Int }


canBuyAlcohol : User -> Bool
canBuyAlcohol user =
    case user.age of
        Nothing ->
            False

        Just age ->
            age >= 21


type alias Friend =
    { name : String
    , age : Maybe Int
    , height : Maybe Float
    , weight : Maybe Float
    }


type BetterFriend
    = Less String
    | More String Info


type alias Info =
    { age : Int
    , height : Float
    , weight : Float
    }
