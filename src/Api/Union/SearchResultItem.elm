-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Union.SearchResultItem exposing (Fragments, fragments, maybeFragments)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onApp : SelectionSet decodesTo Api.Object.App
    , onIssue : SelectionSet decodesTo Api.Object.Issue
    , onMarketplaceListing : SelectionSet decodesTo Api.Object.MarketplaceListing
    , onOrganization : SelectionSet decodesTo Api.Object.Organization
    , onPullRequest : SelectionSet decodesTo Api.Object.PullRequest
    , onRepository : SelectionSet decodesTo Api.Object.Repository
    , onUser : SelectionSet decodesTo Api.Object.User
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Api.Union.SearchResultItem
fragments selections =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "App" selections.onApp
        , Object.buildFragment "Issue" selections.onIssue
        , Object.buildFragment "MarketplaceListing" selections.onMarketplaceListing
        , Object.buildFragment "Organization" selections.onOrganization
        , Object.buildFragment "PullRequest" selections.onPullRequest
        , Object.buildFragment "Repository" selections.onRepository
        , Object.buildFragment "User" selections.onUser
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onApp = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onIssue = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onMarketplaceListing = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onOrganization = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPullRequest = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepository = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onUser = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }