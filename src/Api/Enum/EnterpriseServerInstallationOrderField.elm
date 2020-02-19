-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.EnterpriseServerInstallationOrderField exposing (EnterpriseServerInstallationOrderField(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which Enterprise Server installation connections can be ordered.

  - HostName - Order Enterprise Server installations by host name
  - CustomerName - Order Enterprise Server installations by customer name
  - CreatedAt - Order Enterprise Server installations by creation time

-}
type EnterpriseServerInstallationOrderField
    = HostName
    | CustomerName
    | CreatedAt


list : List EnterpriseServerInstallationOrderField
list =
    [ HostName, CustomerName, CreatedAt ]


decoder : Decoder EnterpriseServerInstallationOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "HOST_NAME" ->
                        Decode.succeed HostName

                    "CUSTOMER_NAME" ->
                        Decode.succeed CustomerName

                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    _ ->
                        Decode.fail ("Invalid EnterpriseServerInstallationOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : EnterpriseServerInstallationOrderField -> String
toString enum =
    case enum of
        HostName ->
            "HOST_NAME"

        CustomerName ->
            "CUSTOMER_NAME"

        CreatedAt ->
            "CREATED_AT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe EnterpriseServerInstallationOrderField
fromString enumString =
    case enumString of
        "HOST_NAME" ->
            Just HostName

        "CUSTOMER_NAME" ->
            Just CustomerName

        "CREATED_AT" ->
            Just CreatedAt

        _ ->
            Nothing
