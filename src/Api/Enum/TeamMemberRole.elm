-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.TeamMemberRole exposing (TeamMemberRole(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| The possible team member roles; either 'maintainer' or 'member'.

  - Maintainer - A team maintainer has permission to add and remove team members.
  - Member - A team member has no administrative permissions on the team.

-}
type TeamMemberRole
    = Maintainer
    | Member


list : List TeamMemberRole
list =
    [ Maintainer, Member ]


decoder : Decoder TeamMemberRole
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "MAINTAINER" ->
                        Decode.succeed Maintainer

                    "MEMBER" ->
                        Decode.succeed Member

                    _ ->
                        Decode.fail ("Invalid TeamMemberRole type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : TeamMemberRole -> String
toString enum =
    case enum of
        Maintainer ->
            "MAINTAINER"

        Member ->
            "MEMBER"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe TeamMemberRole
fromString enumString =
    case enumString of
        "MAINTAINER" ->
            Just Maintainer

        "MEMBER" ->
            Just Member

        _ ->
            Nothing
