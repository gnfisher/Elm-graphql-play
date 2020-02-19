-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.IssueTimelineItemsConnection exposing (edges, filteredCount, nodes, pageCount, pageInfo, totalCount, updatedAt)

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


{-| A list of edges.
-}
edges : SelectionSet decodesTo Api.Object.IssueTimelineItemsEdge -> SelectionSet (Maybe (List (Maybe decodesTo))) Api.Object.IssueTimelineItemsConnection
edges object_ =
    Object.selectionForCompositeField "edges" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| Identifies the count of items after applying `before` and `after` filters.
-}
filteredCount : SelectionSet Int Api.Object.IssueTimelineItemsConnection
filteredCount =
    Object.selectionForField "Int" "filteredCount" [] Decode.int


{-| A list of nodes.
-}
nodes : SelectionSet decodesTo Api.Union.IssueTimelineItems -> SelectionSet (Maybe (List (Maybe decodesTo))) Api.Object.IssueTimelineItemsConnection
nodes object_ =
    Object.selectionForCompositeField "nodes" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| Identifies the count of items after applying `before`/`after` filters and `first`/`last`/`skip` slicing.
-}
pageCount : SelectionSet Int Api.Object.IssueTimelineItemsConnection
pageCount =
    Object.selectionForField "Int" "pageCount" [] Decode.int


{-| Information to aid in pagination.
-}
pageInfo : SelectionSet decodesTo Api.Object.PageInfo -> SelectionSet decodesTo Api.Object.IssueTimelineItemsConnection
pageInfo object_ =
    Object.selectionForCompositeField "pageInfo" [] object_ identity


{-| Identifies the total count of items in the connection.
-}
totalCount : SelectionSet Int Api.Object.IssueTimelineItemsConnection
totalCount =
    Object.selectionForField "Int" "totalCount" [] Decode.int


{-| Identifies the date and time when the timeline was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.IssueTimelineItemsConnection
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
