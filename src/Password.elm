module Password exposing
    ( PasswordValidator
    , Validation
    , applyValidators
    , applyValidators2
    , basicPasswordValidators
    , shouldContain
    , shouldEqualTo
    , strLen
    , strMaxLen
    , strMinLen
    )


type alias Validation =
    ( Bool, String )


type alias ValidationList =
    List Validation


type alias PasswordValidator =
    String -> Validation


type alias ComparisonFunction =
    Int -> Int -> Bool


strLen : String -> ComparisonFunction -> Int -> String -> Validation
strLen name comparisonFunction expectedLen str =
    let
        errMessage =
            String.join " "
                [ "Should contain"
                , name
                , String.fromInt expectedLen
                , "characters."
                ]

        isValid =
            -- first (operand) second
            comparisonFunction (String.length str) expectedLen
    in
    ( isValid, errMessage )


strMaxLen =
    strLen "maximum" (<=)


strMinLen =
    strLen "minimum" (>=)


shouldContain : Int -> String -> String -> String -> Validation
shouldContain minCount charType charStr targetStr =
    let
        errMessage =
            String.join " "
                [ "Should contain at least "
                , String.fromInt minCount
                , " of these: '" ++ charStr ++ "'"
                , charType
                ]

        targetChars =
            String.toList targetStr

        hasSpecial c =
            if List.member c targetChars then
                1

            else
                0

        totalCount =
            List.sum <| List.map hasSpecial <| String.toList charStr
    in
    ( totalCount >= minCount, errMessage )


shouldEqualTo : String -> String -> Validation
shouldEqualTo first second =
    ( first == second, "Both inputs should be equal" )


basicPasswordValidators : List PasswordValidator
basicPasswordValidators =
    [ strMinLen 8
    , strMaxLen 150
    , shouldContain 1 "special characters" "!@#$%^&*()./|\\"
    , shouldContain 1 "numbers" "0123456789"
    , shouldContain 1 "uppercase letters" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    , shouldContain 1 "lowercase letters" "abcdefghijklmnopqrstuvwxyz"
    ]


applyValidators : List PasswordValidator -> String -> ValidationList
applyValidators validators str =
    let
        validationFailed : Validation -> Bool
        validationFailed v =
            Tuple.first v == False

        apply : PasswordValidator -> Validation
        apply v =
            v str
    in
    List.map apply validators |> List.filter validationFailed


applyValidators2 : List PasswordValidator -> String -> String -> ValidationList
applyValidators2 validators pw1 pw2 =
    applyValidators (shouldEqualTo pw1 :: validators) pw2
