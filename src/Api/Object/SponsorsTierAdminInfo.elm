-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.SponsorsTierAdminInfo exposing (SponsorshipsOptionalArguments, sponsorships)

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


type alias SponsorshipsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , includePrivate : OptionalArgument Bool
    , orderBy : OptionalArgument Api.InputObject.SponsorshipOrder
    }


{-| The sponsorships associated with this tier.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - includePrivate - Whether or not to include private sponsorships in the result set
  - orderBy - Ordering options for sponsorships returned from this connection. If left blank, the sponsorships will be ordered based on relevancy to the viewer.

-}
sponsorships : (SponsorshipsOptionalArguments -> SponsorshipsOptionalArguments) -> SelectionSet decodesTo Api.Object.SponsorshipConnection -> SelectionSet decodesTo Api.Object.SponsorsTierAdminInfo
sponsorships fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, includePrivate = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "includePrivate" filledInOptionals.includePrivate Encode.bool, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeSponsorshipOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "sponsorships" optionalArgs object_ identity
