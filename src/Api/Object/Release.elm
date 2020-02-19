-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Release exposing (ReleaseAssetsOptionalArguments, ShortDescriptionHTMLOptionalArguments, author, createdAt, description, descriptionHTML, id, isDraft, isPrerelease, name, publishedAt, releaseAssets, resourcePath, shortDescriptionHTML, tag, tagName, updatedAt, url)

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


{-| The author of the release
-}
author : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.Release
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Release
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The description of the release.
-}
description : SelectionSet (Maybe String) Api.Object.Release
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| The description of this release rendered to HTML.
-}
descriptionHTML : SelectionSet (Maybe Api.ScalarCodecs.Html) Api.Object.Release
descriptionHTML =
    Object.selectionForField "(Maybe ScalarCodecs.Html)" "descriptionHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder |> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Release
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether or not the release is a draft
-}
isDraft : SelectionSet Bool Api.Object.Release
isDraft =
    Object.selectionForField "Bool" "isDraft" [] Decode.bool


{-| Whether or not the release is a prerelease
-}
isPrerelease : SelectionSet Bool Api.Object.Release
isPrerelease =
    Object.selectionForField "Bool" "isPrerelease" [] Decode.bool


{-| The title of the release.
-}
name : SelectionSet (Maybe String) Api.Object.Release
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


{-| Identifies the date and time when the release was created.
-}
publishedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Release
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


type alias ReleaseAssetsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , name : OptionalArgument String
    }


{-| List of releases assets which are dependent on this release.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - name - A list of names to filter the assets by.

-}
releaseAssets : (ReleaseAssetsOptionalArguments -> ReleaseAssetsOptionalArguments) -> SelectionSet decodesTo Api.Object.ReleaseAssetConnection -> SelectionSet decodesTo Api.Object.Release
releaseAssets fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, name = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "name" filledInOptionals.name Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "releaseAssets" optionalArgs object_ identity


{-| The HTTP path for this issue
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Release
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


type alias ShortDescriptionHTMLOptionalArguments =
    { limit : OptionalArgument Int }


{-| A description of the release, rendered to HTML without any links in it.

  - limit - How many characters to return.

-}
shortDescriptionHTML : (ShortDescriptionHTMLOptionalArguments -> ShortDescriptionHTMLOptionalArguments) -> SelectionSet (Maybe Api.ScalarCodecs.Html) Api.Object.Release
shortDescriptionHTML fillInOptionals =
    let
        filledInOptionals =
            fillInOptionals { limit = Absent }

        optionalArgs =
            [ Argument.optional "limit" filledInOptionals.limit Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForField "(Maybe ScalarCodecs.Html)" "shortDescriptionHTML" optionalArgs (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder |> Decode.nullable)


{-| The Git tag the release points to
-}
tag : SelectionSet decodesTo Api.Object.Ref -> SelectionSet (Maybe decodesTo) Api.Object.Release
tag object_ =
    Object.selectionForCompositeField "tag" [] object_ (identity >> Decode.nullable)


{-| The name of the release's Git tag
-}
tagName : SelectionSet String Api.Object.Release
tagName =
    Object.selectionForField "String" "tagName" [] Decode.string


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Release
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this issue
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.Release
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)
