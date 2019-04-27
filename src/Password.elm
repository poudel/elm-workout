module Password exposing
    ( PasswordValidator
    , Validation
    , applyValidators
    , basicPasswordValidators
    , shouldContain
    , shouldContainUpperCase
    , strMaxLen
    , strMinLen
    )


type alias Validation =
    ( Bool, String )


type alias ValidationList =
    List Validation


type alias PasswordValidator =
    String -> Validation


strMinLen : Int -> String -> Validation
strMinLen minLen str =
    let
        errMessage =
            "Should contain at least "
                ++ String.fromInt minLen
                ++ " characters."

        isValid =
            String.length str >= minLen
    in
    ( isValid, errMessage )


strMaxLen : Int -> String -> Validation
strMaxLen maxLen str =
    let
        errMessage =
            "Should not contain more than "
                ++ String.fromInt maxLen
                ++ " characters."

        isValid =
            String.length str <= maxLen
    in
    ( isValid, errMessage )


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
