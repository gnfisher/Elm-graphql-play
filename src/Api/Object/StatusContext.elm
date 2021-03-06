-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.StatusContext exposing (AvatarUrlOptionalArguments, avatarUrl, commit, context, createdAt, creator, description, id, state, targetUrl)

import Api.Enum.StatusState
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


type alias AvatarUrlOptionalArguments =
    { size : OptionalArgument Int }


{-| The avatar of the OAuth application or the user that created the status

  - size - The size of the resulting square image.

-}
avatarUrl : (AvatarUrlOptionalArguments -> AvatarUrlOptionalArguments) -> SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.StatusContext
avatarUrl fillInOptionals =
    let
        filledInOptionals =
            fillInOptionals { size = Absent }

        optionalArgs =
            [ Argument.optional "size" filledInOptionals.size Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "avatarUrl" optionalArgs (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| This commit this status context is attached to.
-}
commit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (Maybe decodesTo) Api.Object.StatusContext
commit object_ =
    Object.selectionForCompositeField "commit" [] object_ (identity >> Decode.nullable)


{-| The name of this status context.
-}
context : SelectionSet String Api.Object.StatusContext
context =
    Object.selectionForField "String" "context" [] Decode.string


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.StatusContext
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The actor who created this status context.
-}
creator : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.StatusContext
creator object_ =
    Object.selectionForCompositeField "creator" [] object_ (identity >> Decode.nullable)


{-| The description for this status context.
-}
description : SelectionSet (Maybe String) Api.Object.StatusContext
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.StatusContext
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The state of this status context.
-}
state : SelectionSet Api.Enum.StatusState.StatusState Api.Object.StatusContext
state =
    Object.selectionForField "Enum.StatusState.StatusState" "state" [] Api.Enum.StatusState.decoder


{-| The URL for this status context.
-}
targetUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.StatusContext
targetUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "targetUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
