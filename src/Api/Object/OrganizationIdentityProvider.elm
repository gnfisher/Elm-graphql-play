-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.OrganizationIdentityProvider exposing (ExternalIdentitiesOptionalArguments, digestMethod, externalIdentities, id, idpCertificate, issuer, organization, signatureMethod, ssoUrl)

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


{-| The digest algorithm used to sign SAML requests for the Identity Provider.
-}
digestMethod : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrganizationIdentityProvider
digestMethod =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "digestMethod" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


type alias ExternalIdentitiesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| External Identities provisioned by this Identity Provider

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
externalIdentities : (ExternalIdentitiesOptionalArguments -> ExternalIdentitiesOptionalArguments) -> SelectionSet decodesTo Api.Object.ExternalIdentityConnection -> SelectionSet decodesTo Api.Object.OrganizationIdentityProvider
externalIdentities fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "externalIdentities" optionalArgs object_ identity


id : SelectionSet Api.ScalarCodecs.Id Api.Object.OrganizationIdentityProvider
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The x509 certificate used by the Identity Provder to sign assertions and responses.
-}
idpCertificate : SelectionSet (Maybe Api.ScalarCodecs.X509Certificate) Api.Object.OrganizationIdentityProvider
idpCertificate =
    Object.selectionForField "(Maybe ScalarCodecs.X509Certificate)" "idpCertificate" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecX509Certificate |> .decoder |> Decode.nullable)


{-| The Issuer Entity ID for the SAML Identity Provider
-}
issuer : SelectionSet (Maybe String) Api.Object.OrganizationIdentityProvider
issuer =
    Object.selectionForField "(Maybe String)" "issuer" [] (Decode.string |> Decode.nullable)


{-| Organization this Identity Provider belongs to
-}
organization : SelectionSet decodesTo Api.Object.Organization -> SelectionSet (Maybe decodesTo) Api.Object.OrganizationIdentityProvider
organization object_ =
    Object.selectionForCompositeField "organization" [] object_ (identity >> Decode.nullable)


{-| The signature algorithm used to sign SAML requests for the Identity Provider.
-}
signatureMethod : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrganizationIdentityProvider
signatureMethod =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "signatureMethod" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The URL endpoint for the Identity Provider's SAML SSO.
-}
ssoUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrganizationIdentityProvider
ssoUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "ssoUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
