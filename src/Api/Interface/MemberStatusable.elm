-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Interface.MemberStatusable exposing (Fragments, MemberStatusesOptionalArguments, fragments, maybeFragments, memberStatuses)

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
    { onOrganization : SelectionSet decodesTo Api.Object.Organization
    , onTeam : SelectionSet decodesTo Api.Object.Team
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Api.Interface.MemberStatusable
fragments selections =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "Organization" selections.onOrganization
        , Object.buildFragment "Team" selections.onTeam
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onOrganization = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onTeam = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


type alias MemberStatusesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , orderBy : OptionalArgument Api.InputObject.UserStatusOrder
    }


{-| Get the status messages members of this entity have set that are either public or visible only to the organization.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - orderBy - Ordering options for user statuses returned from the connection.

-}
memberStatuses : (MemberStatusesOptionalArguments -> MemberStatusesOptionalArguments) -> SelectionSet decodesTo Api.Object.UserStatusConnection -> SelectionSet decodesTo Api.Interface.MemberStatusable
memberStatuses fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeUserStatusOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "memberStatuses" optionalArgs object_ identity
