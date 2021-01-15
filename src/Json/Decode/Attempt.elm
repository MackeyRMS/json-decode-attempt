module Json.Decode.Attempt exposing
    ( Attempt, Dangerously, Safely
    , try, risk, applySafely, applyDangerously
    , fromDecoder, fromValue, fromDecoderWithFallback, fallbackTo, field, map
    , decodeStringDangerously, decodeStringSafely, decodeValueDangerously, decodeValueSafely
    )

{-| When decoding Json, there is usually a hard trade off

1.  Risk failing to decode (bad) in order to get error messages (good)
2.  Have fallbacks (good) but suppress error messages (bad)

This attempt decoding technique is a way to get the good of both.
The result of decoding an `Attempt` gives you the value you sought
and a list of errors it accumulated along the way.


# Attempt

@docs Attempt, Dangerously, Safely


# Pipeline

@docs try, risk, applySafely, applyDangerously


# Basics

@docs fromDecoder, fromValue, fromDecoderWithFallback, fallbackTo, field, map


# Decode

@docs decodeStringDangerously, decodeStringSafely, decodeValueDangerously, decodeValueSafely

-}

import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Test exposing (Test)



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


{-| An attempt at decoding a particular `value`. The `safety` parameter describes
if this kind of attempt will be able to recover or if it will fail hard.
-}
type Attempt value safety
    = Attempt (Decoder (Model value))


type alias Model value =
    { value : value
    , errors : List Decode.Error
    }


{-| A marker that this `Attempt value Safely` can be safely decoded straight to
a `value`, without an intermediate `Result Decode.Error value`. In this example
below, `attemptInt` will always give you an `OperatingSystem`, and it may or
may not give you an error too.

    attemptOperatingSystem : Attempt OperatingSystem Safely
    attemptOperatingSystem =
        Attempt.fromDecoderWithDefault
            (Decode.field "operating_system" OperatingSystem.decoder)
            OperatingSystem.NotDetected

    ( value, errors ) =
        Attempt.decodeValueSafely attemptOperatingSystem

-}
type Safely
    = Safely__Unit


{-| A marker that this `Attempt value Dangerously` cannot be safely decoded straight to
a `value`, and it may fail in a big way that doesnt deliver a `value` at all. In addition,
if it does succeed decoding a `value`, it still preserves the `Decode.Error` is found
along the way.

    type alias User =
        { id : Int
        , name : Maybe String
        }

    decodeUser : Decode.Value -> Result Decode.Error ( User, List Decode.Error )
    decodeUser json =
        let
            attemptUser : Attempt User Dangerously
            attemptUser =
                Attempt.succeed User
                    |> Attempt.risk (Decode.field "id" Decode.int)
                    |> Attempt.try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
        in
        Attempt.decodeValueDangerously attemptUser json

-}
type Dangerously
    = Dangerously__Unit



---------------------------------------------------------------
-- INTERNAL HELPERS --
---------------------------------------------------------------


mapModel : (a -> b) -> Model a -> Model b
mapModel f model =
    { value = f model.value
    , errors = model.errors
    }


decodeSafely :
    (Decoder (Model value) -> json -> Result Decode.Error (Model value))
    -> Attempt value Safely
    -> json
    -> ( value, List Decode.Error )
decodeSafely decode (Attempt decoder) json =
    case decode decoder json of
        Ok model ->
            ( model.value, model.errors )

        Err _ ->
            -- !!!!!! THIS SHOULD BE IMPOSSIBLE !!!!!!
            -- The only way we can get a `Safely` Attempt is
            -- if a fallback is provided for every value
            -- SO THIS SHOULD NOT BE ABLE TO FAIL!!!
            decodeSafely decode (Attempt decoder) json


decodeDangerously :
    (Decoder (Model value) -> json -> Result Decode.Error (Model value))
    -> Attempt value Dangerously
    -> json
    -> Result Decode.Error ( value, List Decode.Error )
decodeDangerously decode (Attempt decoder) json =
    decode decoder json
        |> Result.map (\model -> ( model.value, model.errors ))



---------------------------------------------------------------
-- API --
---------------------------------------------------------------


{-| The final step in an attempt to decode json. This function takes a safe attempt, and
therefore can never fail. It will pass along both the value you intended to decode, and
a list of whatever errors it encountered along the way.
-}
decodeValueSafely : Attempt value Safely -> Decode.Value -> ( value, List Decode.Error )
decodeValueSafely =
    decodeSafely Decode.decodeValue


{-| Just like `decodeValueSafely`, except it takes a `String` instead of a `Decode.Value`. Just like
the difference between `Decode.decodeValue` and `Decode.decodeString`.
-}
decodeStringSafely : Attempt value Safely -> String -> ( value, List Decode.Error )
decodeStringSafely =
    decodeSafely Decode.decodeString


{-| The final step in an attempt to decode json. This function takes a dangerous attempt,
and therefore it can possibly fail. If it does succeed, the `Ok` case is `(value, List Decode.Error)`,
containing both the desired `value` and the `List Decode.Error` of non-critical errors it encountered
along the way.
-}
decodeValueDangerously : Attempt value Dangerously -> Decode.Value -> Result Decode.Error ( value, List Decode.Error )
decodeValueDangerously =
    decodeDangerously Decode.decodeValue


{-| Just like `decodeValueDangerously`, except it takes a `String` instead of a `Decode.Value`. Just like
the difference between `Decode.decodeValue` and `Decode.decodeString`.
-}
decodeStringDangerously : Attempt value Dangerously -> String -> Result Decode.Error ( value, List Decode.Error )
decodeStringDangerously =
    decodeDangerously Decode.decodeString


{-| Convert a value into an attempt, presumably to use it in a pipeline. Since the
value is provided outright the Attempt is safe
-}
fromValue : value -> Attempt value Safely
fromValue value =
    { value = value
    , errors = []
    }
        |> Decode.succeed
        |> Attempt


{-| Attempt to decode json that is under a certain field, much like `Decode.field`. By checking
under a field, you risk the field not being present in the first place, so using `field` is always `Dangerous`

    type alias User =
        { id : Int
        , name : Maybe String
        , car : Car
        }

    type alias Car =
        { plateNumber : Maybe String
        , miles : Maybe Int
        }

    attemptCar : Attempt Car Safely
    attemptCar =
        Attempt.fromValue Car
            |> Attempt.try (Decode.map Just <| Decode.field "plate_number" Decode.string) Nothing
            |> Attempt.try (Decode.map Just <| Decode.field "miles" Decode.int) Nothing

    attemptUser : Attempt User Dangerously
    attemptUser =
        Attempt.fromValue User
            |> Attempt.risk (Decode.field "id" Decode.int)
            |> Attempt.try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
            |> Attempt.applyDangerously (Attempt.field "car" attemptCar)

-}
field : String -> Attempt value safety -> Attempt value Dangerously
field fieldName (Attempt decoder) =
    let
        fromJson : Decode.Value -> Decoder (Model value)
        fromJson json =
            case Decode.decodeValue (Decode.field fieldName Decode.value) json of
                Ok _ ->
                    Decode.field fieldName decoder

                Err error ->
                    Decode.fail <| Decode.errorToString error
    in
    Decode.value
        |> Decode.andThen fromJson
        |> Attempt


{-| Take a dangerous attempt, and specify how it should fall back, thereby making it a safe attempt
-}
fallbackTo : value -> Attempt value Dangerously -> Attempt value Safely
fallbackTo fallbackValue attempt =
    let
        fromJson : Decode.Value -> Model value
        fromJson json =
            case decodeValueDangerously attempt json of
                Ok ( value, errors ) ->
                    { value = value
                    , errors = errors
                    }

                Err error ->
                    { value = fallbackValue
                    , errors = [ error ]
                    }
    in
    Decode.value
        |> Decode.map fromJson
        |> Attempt


{-| Convert the value of an `Attempt` from one type to another
-}
map : (a -> b) -> Attempt a safety -> Attempt b safety
map f (Attempt decoder) =
    Attempt <| Decode.map (mapModel f) decoder


{-| Apply a dangerous attempt to a pipeline, turning the rest of the pipeline into a dangerous attempt.
Kind of like a more general form of `risk`.

    risk : Decoder a -> Attempt (a -> b) safety -> Attempt b Dangerously
    risk decoder =
        applyDangerously (fromDecoder decoder)

    type Id
        = Id Int

    attemptId : Attempt Id Dangerous
    attemptId =
        Attempt.succeed Id
            |> Attempt.risk Decode.int

-}
applyDangerously : Attempt a Dangerously -> Attempt (a -> b) safety -> Attempt b Dangerously
applyDangerously (Attempt aDecoder) (Attempt fDecoder) =
    let
        fromJson : Model (a -> b) -> Model a -> Model b
        fromJson fModel aModel =
            { value = fModel.value aModel.value
            , errors = fModel.errors ++ aModel.errors
            }
    in
    Decode.map2 fromJson fDecoder aDecoder
        |> Attempt


{-| Apply a safe attempt to a pipeline. Kind of like a more general form of `try`

    try : Decoder a -> a -> Attempt (a -> b) safety -> Attempt b safety
    try decoder fallback =
        applySafely (fromDecoderWithFallback decoder fallback)

    type alias Flags =
        { landingPageUrl : String }

    attemptFlags : Attempt Flags Safely
    attemptFlags =
        Attempt.succeed Flags
            |> Attempt.applySafely (fromDecoderWithFallback Decode.string (Route.toString Route.home))

-}
applySafely : Attempt a Safely -> Attempt (a -> b) safety -> Attempt b safety
applySafely aAttempt (Attempt fDecoder) =
    let
        fromJson : Model (a -> b) -> Decode.Value -> Model b
        fromJson fModel json =
            let
                ( a, errors ) =
                    decodeValueSafely aAttempt json
            in
            { value = fModel.value a
            , errors = errors ++ fModel.errors
            }
    in
    Decode.map2 fromJson fDecoder Decode.value
        |> Attempt


{-| Use a decoder in an attempt, with a fallback. Since a fallback is provided,
the attempt is safe
-}
fromDecoderWithFallback : Decoder a -> a -> Attempt a Safely
fromDecoderWithFallback decoder fallback =
    fromDecoder decoder
        |> fallbackTo fallback


{-| Use a decoder in an attempt. Since the decoder might fail, the resulting
attempt is a `Dangerous` one.
-}
fromDecoder : Decoder a -> Attempt a Dangerously
fromDecoder decoder =
    decoder
        |> Decode.map
            (\value ->
                { value = value
                , errors = []
                }
            )
        |> Attempt


oneOfSafely : Attempt a Safely -> List (Attempt a Dangerously) -> Attempt a Safely
oneOfSafely safeAttempt_ dangerousAttempts_ =
    let
        oneOfSafelyHelp : List Decode.Error -> Attempt a Safely -> List (Attempt a Dangerously) -> Decoder (Model a)
        oneOfSafelyHelp errors (Attempt safeAttempt) dangerousAttempts =
            case dangerousAttempts of
                [] ->
                    Decode.map
                        (\model -> { model | errors = errors ++ model.errors })
                        safeAttempt

                (Attempt first) :: rest ->
                    let
                        fromJson : Decode.Value -> Decoder (Model a)
                        fromJson json =
                            case Decode.decodeValue first json of
                                Ok dangerousModel ->
                                    { value = dangerousModel.value
                                    , errors = dangerousModel.errors ++ errors
                                    }
                                        |> Decode.succeed

                                Err error ->
                                    oneOfSafelyHelp
                                        (error :: errors)
                                        (Attempt safeAttempt)
                                        rest
                    in
                    Decode.andThen fromJson Decode.value
    in
    oneOfSafelyHelp [] safeAttempt_ dangerousAttempts_
        |> Attempt


{-| Try a decoder, and risk it failing, thereby transforming your attempt into a `Dangerous` one

    type Id
        = Id Int

    attemptId : Attempt Id Dangerous
    attemptId =
        Attempt.succeed Id
            |> Attempt.risk Decode.int

-}
risk : Decoder a -> Attempt (a -> b) safety -> Attempt b Dangerously
risk decoder =
    applyDangerously (fromDecoder decoder)


{-| Try a decoder, and if it fails, fallback `Safely` to a fallback value

    type alias Flags =
        { landingPageUrl : String }

    attemptFlags : Attempt Flags Safely
    attemptFlags =
        Attempt.succeed Flags
            |> Attempt.try Decode.string (Route.toString Route.home)

-}
try : Decoder a -> a -> Attempt (a -> b) safety -> Attempt b safety
try decoder fallback =
    applySafely (fromDecoderWithFallback decoder fallback)



---------------------------------------------------------------
-- TESTS --
---------------------------------------------------------------


type alias TestUser =
    { id : Int
    , name : Maybe String
    }


type alias TestUserWithFriend =
    { id : Int
    , name : Maybe String
    , friend : Maybe TestUser
    }


type TestHasFriend
    = TestHasFriend { friend : Maybe TestHasFriend }


tests : Test
tests =
    Test.describe "JSon Decode Attempt Tests"
        [ jsonTests
        , stringTests
        ]


stringTests : Test
stringTests =
    let
        simpleArithmeticDecoder : Attempt Int Safely
        simpleArithmeticDecoder =
            fromValue ((+) 1)
                |> try (Decode.field "number" Decode.int) 3
    in
    Test.describe "Attempt Decoder String Decodes"
        [ Test.test "Attempt Decoder decodes correctly when the value is there" <|
            \_ ->
                decodeStringSafely
                    simpleArithmeticDecoder
                    (Encode.encode 2 <| Encode.object [ Tuple.pair "number" <| Encode.int 1 ])
                    |> Expect.equal ( 2, [] )
        , Test.test "Attempt Decoder decodes falls back gracefully when the error is not there" <|
            \_ ->
                decodeStringSafely
                    simpleArithmeticDecoder
                    (Encode.encode 2 <| Encode.object [ Tuple.pair "unicorns" <| Encode.string "They are real" ])
                    |> (\( value, errors ) ->
                            { value = value
                            , errors = List.map Decode.errorToString errors
                            }
                       )
                    |> Expect.equal
                        { value = 4
                        , errors = [ "Problem with the given value:\n\n{\n        \"unicorns\": \"They are real\"\n    }\n\nExpecting an OBJECT with a field named `number`" ]
                        }
        , Test.test "Attempt Decoder decodes and fallsback" <|
            \_ ->
                decodeStringSafely
                    (fromValue (\a str b -> String.join " " [ "There are", String.fromInt (a + b), str ])
                        |> try (Decode.field "number" Decode.int) 3
                        |> try (Decode.field "animal" Decode.string) "Horses"
                        |> try (Decode.field "number_2" Decode.int) 5
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "number" <| Encode.int 1
                            , Tuple.pair "animal" <| Encode.string "Dogs"
                            ]
                    )
                    |> (\( value, errors ) ->
                            { value = value
                            , errors = List.map Decode.errorToString errors
                            }
                       )
                    |> Expect.equal
                        { value = "There are 6 Dogs"
                        , errors = [ "Problem with the given value:\n\n{\n        \"number\": 1,\n        \"animal\": \"Dogs\"\n    }\n\nExpecting an OBJECT with a field named `number_2`" ]
                        }
        , Test.test "Attempt decode risks decoding" <|
            \_ ->
                decodeStringDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "id" <| Encode.int 2
                            , Tuple.pair "name" <| Encode.string "Hank"
                            ]
                    )
                    |> Expect.equal (Ok ( TestUser 2 (Just "Hank"), [] ))
        , Test.test "Attempt decode risks and fails decoding" <|
            \_ ->
                decodeStringDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "id" <| Encode.int 2
                            ]
                    )
                    |> Result.map
                        (\( user, errors ) ->
                            ( user, List.map Decode.errorToString errors )
                        )
                    |> Expect.equal
                        (Ok
                            ( TestUser 2 Nothing
                            , [ "Problem with the given value:\n\n{\n        \"id\": 2\n    }\n\nExpecting an OBJECT with a field named `name`" ]
                            )
                        )
        , Test.test "Attempt decode risks and fails decoding hard" <|
            \_ ->
                decodeStringDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "id" <| Encode.string "400"
                            , Tuple.pair "name" <| Encode.string "Hank"
                            ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal
                        (Err "Problem with the value at json.id:\n\n    \"400\"\n\nExpecting an INT")
        , Test.test "Nested Pipelines" <|
            \_ ->
                decodeStringDangerously
                    (fromValue TestUserWithFriend
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                        |> applySafely
                            (fromValue TestUser
                                |> risk (Decode.field "id" Decode.int)
                                |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                                |> map Just
                                |> field "friend"
                                |> fallbackTo Nothing
                            )
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "id" <| Encode.int 400
                            , Tuple.pair "name" <| Encode.string "Hank"
                            , Tuple.pair "friend" <|
                                Encode.object
                                    [ Tuple.pair "id" <| Encode.int 50
                                    , Tuple.pair "name" <| Encode.string "John"
                                    ]
                            ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal
                        (Ok
                            ( { id = 400
                              , name = Just "Hank"
                              , friend =
                                    Just
                                        { id = 50
                                        , name = Just "John"
                                        }
                              }
                            , []
                            )
                        )
        , Test.test "Nested Pipelines that fail" <|
            \_ ->
                decodeStringDangerously
                    (fromValue TestUserWithFriend
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                        |> applySafely
                            (fromValue TestUser
                                |> risk (Decode.field "id" Decode.int)
                                |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                                |> map Just
                                |> field "friend"
                                |> fallbackTo Nothing
                            )
                    )
                    (Encode.encode 2 <|
                        Encode.object
                            [ Tuple.pair "id" <| Encode.int 400
                            , Tuple.pair "name" <| Encode.string "Hank"
                            ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Result.map (Tuple.mapSecond (List.map Decode.errorToString))
                    |> Expect.equal
                        (Ok
                            ( { id = 400
                              , name = Just "Hank"
                              , friend = Nothing
                              }
                            , [ "Problem with the given value:\n\n{\n        \"id\": 400,\n        \"name\": \"Hank\"\n    }\n\nProblem with the given value:\n\n{\n        \"id\": 400,\n        \"name\": \"Hank\"\n    }\n\nExpecting an OBJECT with a field named `friend`"
                              ]
                            )
                        )
        , Test.describe "oneOf"
            [ Test.test "oneOfSafely with no decoders" <|
                \_ ->
                    decodeStringSafely
                        (oneOfSafely
                            (fromValue 0)
                            []
                        )
                        (Encode.encode 2 Encode.null)
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 0, [] )
            , Test.test "oneOfSafely with one successful decoder" <|
                \_ ->
                    decodeStringSafely
                        (oneOfSafely
                            (fromValue 0)
                            [ fromDecoder Decode.int ]
                        )
                        (Encode.encode 2 <| Encode.int 4)
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 4, [] )
            , Test.test "oneOfSafely with one unsuccessful decoder, and one successful" <|
                \_ ->
                    decodeStringSafely
                        (oneOfSafely
                            (fromValue 0)
                            [ fromDecoder Decode.int
                            , Decode.string
                                |> Decode.andThen
                                    (\str ->
                                        case String.toInt str of
                                            Just i ->
                                                Decode.succeed i

                                            Nothing ->
                                                Decode.fail "Could not decode string to int"
                                    )
                                |> fromDecoder
                            , fromDecoder Decode.int
                            ]
                        )
                        (Encode.encode 2 <| Encode.string "4")
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 4, [ "Problem with the given value:\n\n\"4\"\n\nExpecting an INT" ] )
            ]
        ]


jsonTests : Test
jsonTests =
    let
        simpleArithmeticDecoder : Attempt Int Safely
        simpleArithmeticDecoder =
            fromValue ((+) 1)
                |> try (Decode.field "number" Decode.int) 3
    in
    Test.describe "Attempt Decoder Json Decodes"
        [ Test.test "Attempt Decoder decodes correctly when the value is there" <|
            \_ ->
                decodeValueSafely
                    simpleArithmeticDecoder
                    (Encode.object [ Tuple.pair "number" <| Encode.int 1 ])
                    |> Expect.equal ( 2, [] )
        , Test.test "Attempt Decoder decodes falls back gracefully when the error is not there" <|
            \_ ->
                decodeValueSafely
                    simpleArithmeticDecoder
                    (Encode.object [ Tuple.pair "unicorns" <| Encode.string "They are real" ])
                    |> (\( value, errors ) ->
                            { value = value
                            , errors = List.map Decode.errorToString errors
                            }
                       )
                    |> Expect.equal
                        { value = 4
                        , errors = [ "Problem with the given value:\n\n{\n        \"unicorns\": \"They are real\"\n    }\n\nExpecting an OBJECT with a field named `number`" ]
                        }
        , Test.test "Attempt Decoder decodes and fallsback" <|
            \_ ->
                decodeValueSafely
                    (fromValue (\a str b -> String.join " " [ "There are", String.fromInt (a + b), str ])
                        |> try (Decode.field "number" Decode.int) 3
                        |> try (Decode.field "animal" Decode.string) "Horses"
                        |> try (Decode.field "number_2" Decode.int) 5
                    )
                    (Encode.object
                        [ Tuple.pair "number" <| Encode.int 1
                        , Tuple.pair "animal" <| Encode.string "Dogs"
                        ]
                    )
                    |> (\( value, errors ) ->
                            { value = value
                            , errors = List.map Decode.errorToString errors
                            }
                       )
                    |> Expect.equal
                        { value = "There are 6 Dogs"
                        , errors = [ "Problem with the given value:\n\n{\n        \"number\": 1,\n        \"animal\": \"Dogs\"\n    }\n\nExpecting an OBJECT with a field named `number_2`" ]
                        }
        , Test.test "Attempt decode risks decoding" <|
            \_ ->
                decodeValueDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.object
                        [ Tuple.pair "id" <| Encode.int 2
                        , Tuple.pair "name" <| Encode.string "Hank"
                        ]
                    )
                    |> Expect.equal (Ok ( TestUser 2 (Just "Hank"), [] ))
        , Test.test "Attempt decode risks and fails decoding" <|
            \_ ->
                decodeValueDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.object
                        [ Tuple.pair "id" <| Encode.int 2
                        ]
                    )
                    |> Result.map
                        (\( user, errors ) ->
                            ( user, List.map Decode.errorToString errors )
                        )
                    |> Expect.equal
                        (Ok
                            ( TestUser 2 Nothing
                            , [ "Problem with the given value:\n\n{\n        \"id\": 2\n    }\n\nExpecting an OBJECT with a field named `name`" ]
                            )
                        )
        , Test.test "Attempt decode risks and fails decoding hard" <|
            \_ ->
                decodeValueDangerously
                    (fromValue TestUser
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                    )
                    (Encode.object
                        [ Tuple.pair "id" <| Encode.string "400"
                        , Tuple.pair "name" <| Encode.string "Hank"
                        ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal
                        (Err "Problem with the value at json.id:\n\n    \"400\"\n\nExpecting an INT")
        , Test.test "Nested Pipelines" <|
            \_ ->
                decodeValueDangerously
                    (fromValue TestUserWithFriend
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                        |> applySafely
                            (fromValue TestUser
                                |> risk (Decode.field "id" Decode.int)
                                |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                                |> map Just
                                |> field "friend"
                                |> fallbackTo Nothing
                            )
                    )
                    (Encode.object
                        [ Tuple.pair "id" <| Encode.int 400
                        , Tuple.pair "name" <| Encode.string "Hank"
                        , Tuple.pair "friend" <|
                            Encode.object
                                [ Tuple.pair "id" <| Encode.int 50
                                , Tuple.pair "name" <| Encode.string "John"
                                ]
                        ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Expect.equal
                        (Ok
                            ( { id = 400
                              , name = Just "Hank"
                              , friend =
                                    Just
                                        { id = 50
                                        , name = Just "John"
                                        }
                              }
                            , []
                            )
                        )
        , Test.test "Nested Pipelines that fail" <|
            \_ ->
                decodeValueDangerously
                    (fromValue TestUserWithFriend
                        |> risk (Decode.field "id" Decode.int)
                        |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                        |> applySafely
                            (fromValue TestUser
                                |> risk (Decode.field "id" Decode.int)
                                |> try (Decode.map Just <| Decode.field "name" Decode.string) Nothing
                                |> map Just
                                |> field "friend"
                                |> fallbackTo Nothing
                            )
                    )
                    (Encode.object
                        [ Tuple.pair "id" <| Encode.int 400
                        , Tuple.pair "name" <| Encode.string "Hank"
                        ]
                    )
                    |> Result.mapError Decode.errorToString
                    |> Result.map (Tuple.mapSecond (List.map Decode.errorToString))
                    |> Expect.equal
                        (Ok
                            ( { id = 400
                              , name = Just "Hank"
                              , friend = Nothing
                              }
                            , [ "Problem with the given value:\n\n{\n        \"id\": 400,\n        \"name\": \"Hank\"\n    }\n\nProblem with the given value:\n\n{\n        \"id\": 400,\n        \"name\": \"Hank\"\n    }\n\nExpecting an OBJECT with a field named `friend`"
                              ]
                            )
                        )
        , Test.describe "oneOf"
            [ Test.test "oneOfSafely with no decoders" <|
                \_ ->
                    decodeValueSafely
                        (oneOfSafely
                            (fromValue 0)
                            []
                        )
                        Encode.null
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 0, [] )
            , Test.test "oneOfSafely with one successful decoder" <|
                \_ ->
                    decodeValueSafely
                        (oneOfSafely
                            (fromValue 0)
                            [ fromDecoder Decode.int ]
                        )
                        (Encode.int 4)
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 4, [] )
            , Test.test "oneOfSafely with one unsuccessful decoder, and one successful" <|
                \_ ->
                    decodeValueSafely
                        (oneOfSafely
                            (fromValue 0)
                            [ fromDecoder Decode.int
                            , Decode.string
                                |> Decode.andThen
                                    (\str ->
                                        case String.toInt str of
                                            Just i ->
                                                Decode.succeed i

                                            Nothing ->
                                                Decode.fail "Could not decode string to int"
                                    )
                                |> fromDecoder
                            , fromDecoder Decode.int
                            ]
                        )
                        (Encode.string "4")
                        |> Tuple.mapSecond (List.map Decode.errorToString)
                        |> Expect.equal ( 4, [ "Problem with the given value:\n\n\"4\"\n\nExpecting an INT" ] )
            ]
        ]
