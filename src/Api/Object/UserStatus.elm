-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UserStatus exposing (createdAt, emoji, emojiHTML, expiresAt, id, indicatesLimitedAvailability, message, organization, updatedAt, user)

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


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserStatus
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| An emoji summarizing the user's status.
-}
emoji : SelectionSet (Maybe String) Api.Object.UserStatus
emoji =
    Object.selectionForField "(Maybe String)" "emoji" [] (Decode.string |> Decode.nullable)


{-| The status emoji as HTML.
-}
emojiHTML : SelectionSet (Maybe Api.ScalarCodecs.Html) Api.Object.UserStatus
emojiHTML =
    Object.selectionForField "(Maybe ScalarCodecs.Html)" "emojiHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder |> Decode.nullable)


{-| If set, the status will not be shown after this date.
-}
expiresAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.UserStatus
expiresAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "expiresAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| ID of the object.
-}
id : SelectionSet Api.ScalarCodecs.Id Api.Object.UserStatus
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether this status indicates the user is not fully available on GitHub.
-}
indicatesLimitedAvailability : SelectionSet Bool Api.Object.UserStatus
indicatesLimitedAvailability =
    Object.selectionForField "Bool" "indicatesLimitedAvailability" [] Decode.bool


{-| A brief message describing what the user is doing.
-}
message : SelectionSet (Maybe String) Api.Object.UserStatus
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)


{-| The organization whose members can see this status. If null, this status is publicly visible.
-}
organization : SelectionSet decodesTo Api.Object.Organization -> SelectionSet (Maybe decodesTo) Api.Object.UserStatus
organization object_ =
    Object.selectionForCompositeField "organization" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserStatus
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The user who has this status.
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet decodesTo Api.Object.UserStatus
user object_ =
    Object.selectionForCompositeField "user" [] object_ identity
