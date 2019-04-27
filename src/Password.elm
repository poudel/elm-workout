module Password exposing
    ( PasswordValidator
    , Validation
    , applyValidators
    , basicPasswordValidators
    , shouldContain
    , shouldContainUpperCase
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


shouldContain : Int -> String -> String -> Validation
shouldContain number special str =
    let
        errMessage =
            "Should contain at least "
                ++ String.fromInt number
                ++ " of these: '"
                ++ special
                ++ "' characters."

        targetChars =
            String.toList str

        hasSpecial c =
            if List.member c targetChars then
                1

            else
                0

        foundCharacters =
            List.sum <| List.map hasSpecial <| String.toList special
    in
    ( foundCharacters >= number, errMessage )


shouldEqual : String -> String -> Validation
shouldEqual first second =
    ( first == second, "Both passwords should match." )


shouldContainUpperCase : Int -> String -> Validation
shouldContainUpperCase minCount str =
    let
        counter c =
            if Char.isUpper c then
                1

            else
                0

        count =
            String.toList str
                |> List.map counter
                |> List.sum
    in
    ( count >= minCount
    , "Should contain at least "
        ++ String.fromInt minCount
        ++ " uppercase characters"
    )


basicPasswordValidators : List PasswordValidator
basicPasswordValidators =
    [ strMinLen 8
    , strMaxLen 150
    , shouldContain 1 "!@#$%^&*()./|\\"
    , shouldContain 1 "0123456789"
    , shouldContainUpperCase 1
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
