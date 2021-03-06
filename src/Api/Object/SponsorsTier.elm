-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.SponsorsTier exposing (adminInfo, createdAt, description, descriptionHTML, id, monthlyPriceInCents, monthlyPriceInDollars, name, sponsorsListing, updatedAt)

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


{-| SponsorsTier information only visible to users that can administer the associated Sponsors listing.
-}
adminInfo : SelectionSet decodesTo Api.Object.SponsorsTierAdminInfo -> SelectionSet (Maybe decodesTo) Api.Object.SponsorsTier
adminInfo object_ =
    Object.selectionForCompositeField "adminInfo" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.SponsorsTier
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The description of the tier.
-}
description : SelectionSet String Api.Object.SponsorsTier
description =
    Object.selectionForField "String" "description" [] Decode.string


{-| The tier description rendered to HTML
-}
descriptionHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.SponsorsTier
descriptionHTML =
    Object.selectionForField "ScalarCodecs.Html" "descriptionHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.SponsorsTier
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| How much this tier costs per month in cents.
-}
monthlyPriceInCents : SelectionSet Int Api.Object.SponsorsTier
monthlyPriceInCents =
    Object.selectionForField "Int" "monthlyPriceInCents" [] Decode.int


{-| How much this tier costs per month in dollars.
-}
monthlyPriceInDollars : SelectionSet Int Api.Object.SponsorsTier
monthlyPriceInDollars =
    Object.selectionForField "Int" "monthlyPriceInDollars" [] Decode.int


{-| The name of the tier.
-}
name : SelectionSet String Api.Object.SponsorsTier
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The sponsors listing that this tier belongs to.
-}
sponsorsListing : SelectionSet decodesTo Api.Object.SponsorsListing -> SelectionSet decodesTo Api.Object.SponsorsTier
sponsorsListing object_ =
    Object.selectionForCompositeField "sponsorsListing" [] object_ identity


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.SponsorsTier
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
