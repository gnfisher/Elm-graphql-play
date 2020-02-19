-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.ContributionCalendarDay exposing (color, contributionCount, date, weekday)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| The hex color code that represents how many contributions were made on this day compared to others in the calendar.
-}
color : SelectionSet String Api.Object.ContributionCalendarDay
color =
    Object.selectionForField "String" "color" [] Decode.string


{-| How many contributions were made by the user on this day.
-}
contributionCount : SelectionSet Int Api.Object.ContributionCalendarDay
contributionCount =
    Object.selectionForField "Int" "contributionCount" [] Decode.int


{-| The day this square represents.
-}
date : SelectionSet Api.ScalarCodecs.Date Api.Object.ContributionCalendarDay
date =
    Object.selectionForField "ScalarCodecs.Date" "date" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDate |> .decoder)


{-| A number representing which day of the week this square represents, e.g., 1 is Monday.
-}
weekday : SelectionSet Int Api.Object.ContributionCalendarDay
weekday =
    Object.selectionForField "Int" "weekday" [] Decode.int