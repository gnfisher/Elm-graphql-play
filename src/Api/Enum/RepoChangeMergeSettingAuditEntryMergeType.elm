-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.RepoChangeMergeSettingAuditEntryMergeType exposing (RepoChangeMergeSettingAuditEntryMergeType(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| The merge options available for pull requests to this repository.

  - Merge - The pull request is added to the base branch in a merge commit.
  - Rebase - Commits from the pull request are added onto the base branch individually without a merge commit.
  - Squash - The pull request's commits are squashed into a single commit before they are merged to the base branch.

-}
type RepoChangeMergeSettingAuditEntryMergeType
    = Merge
    | Rebase
    | Squash


list : List RepoChangeMergeSettingAuditEntryMergeType
list =
    [ Merge, Rebase, Squash ]


decoder : Decoder RepoChangeMergeSettingAuditEntryMergeType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "MERGE" ->
                        Decode.succeed Merge

                    "REBASE" ->
                        Decode.succeed Rebase

                    "SQUASH" ->
                        Decode.succeed Squash

                    _ ->
                        Decode.fail ("Invalid RepoChangeMergeSettingAuditEntryMergeType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : RepoChangeMergeSettingAuditEntryMergeType -> String
toString enum =
    case enum of
        Merge ->
            "MERGE"

        Rebase ->
            "REBASE"

        Squash ->
            "SQUASH"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe RepoChangeMergeSettingAuditEntryMergeType
fromString enumString =
    case enumString of
        "MERGE" ->
            Just Merge

        "REBASE" ->
            Just Rebase

        "SQUASH" ->
            Just Squash

        _ ->
            Nothing
