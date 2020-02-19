-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.AddPullRequestReviewCommentPayload exposing (clientMutationId, comment, commentEdge)

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


{-| A unique identifier for the client performing the mutation.
-}
clientMutationId : SelectionSet (Maybe String) Api.Object.AddPullRequestReviewCommentPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The newly created comment.
-}
comment : SelectionSet decodesTo Api.Object.PullRequestReviewComment -> SelectionSet (Maybe decodesTo) Api.Object.AddPullRequestReviewCommentPayload
comment object_ =
    Object.selectionForCompositeField "comment" [] object_ (identity >> Decode.nullable)


{-| The edge from the review's comment connection.
-}
commentEdge : SelectionSet decodesTo Api.Object.PullRequestReviewCommentEdge -> SelectionSet (Maybe decodesTo) Api.Object.AddPullRequestReviewCommentPayload
commentEdge object_ =
    Object.selectionForCompositeField "commentEdge" [] object_ (identity >> Decode.nullable)