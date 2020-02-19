-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.SecurityAdvisory exposing (VulnerabilitiesOptionalArguments, databaseId, description, ghsaId, id, identifiers, origin, publishedAt, references, severity, summary, updatedAt, vulnerabilities, withdrawnAt)

import Api.Enum.SecurityAdvisoryEcosystem
import Api.Enum.SecurityAdvisorySeverity
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


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Api.Object.SecurityAdvisory
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| This is a long plaintext description of the advisory
-}
description : SelectionSet String Api.Object.SecurityAdvisory
description =
    Object.selectionForField "String" "description" [] Decode.string


{-| The GitHub Security Advisory ID
-}
ghsaId : SelectionSet String Api.Object.SecurityAdvisory
ghsaId =
    Object.selectionForField "String" "ghsaId" [] Decode.string


id : SelectionSet Api.ScalarCodecs.Id Api.Object.SecurityAdvisory
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| A list of identifiers for this advisory
-}
identifiers : SelectionSet decodesTo Api.Object.SecurityAdvisoryIdentifier -> SelectionSet (List decodesTo) Api.Object.SecurityAdvisory
identifiers object_ =
    Object.selectionForCompositeField "identifiers" [] object_ (identity >> Decode.list)


{-| The organization that originated the advisory
-}
origin : SelectionSet String Api.Object.SecurityAdvisory
origin =
    Object.selectionForField "String" "origin" [] Decode.string


{-| When the advisory was published
-}
publishedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.SecurityAdvisory
publishedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "publishedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| A list of references for this advisory
-}
references : SelectionSet decodesTo Api.Object.SecurityAdvisoryReference -> SelectionSet (List decodesTo) Api.Object.SecurityAdvisory
references object_ =
    Object.selectionForCompositeField "references" [] object_ (identity >> Decode.list)


{-| The severity of the advisory
-}
severity : SelectionSet Api.Enum.SecurityAdvisorySeverity.SecurityAdvisorySeverity Api.Object.SecurityAdvisory
severity =
    Object.selectionForField "Enum.SecurityAdvisorySeverity.SecurityAdvisorySeverity" "severity" [] Api.Enum.SecurityAdvisorySeverity.decoder


{-| A short plaintext summary of the advisory
-}
summary : SelectionSet String Api.Object.SecurityAdvisory
summary =
    Object.selectionForField "String" "summary" [] Decode.string


{-| When the advisory was last updated
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.SecurityAdvisory
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


type alias VulnerabilitiesOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.SecurityVulnerabilityOrder
    , ecosystem : OptionalArgument Api.Enum.SecurityAdvisoryEcosystem.SecurityAdvisoryEcosystem
    , package : OptionalArgument String
    , severities : OptionalArgument (List Api.Enum.SecurityAdvisorySeverity.SecurityAdvisorySeverity)
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| Vulnerabilities associated with this Advisory

  - orderBy - Ordering options for the returned topics.
  - ecosystem - An ecosystem to filter vulnerabilities by.
  - package - A package name to filter vulnerabilities by.
  - severities - A list of severities to filter vulnerabilities by.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
vulnerabilities : (VulnerabilitiesOptionalArguments -> VulnerabilitiesOptionalArguments) -> SelectionSet decodesTo Api.Object.SecurityVulnerabilityConnection -> SelectionSet decodesTo Api.Object.SecurityAdvisory
vulnerabilities fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, ecosystem = Absent, package = Absent, severities = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeSecurityVulnerabilityOrder, Argument.optional "ecosystem" filledInOptionals.ecosystem (Encode.enum Api.Enum.SecurityAdvisoryEcosystem.toString), Argument.optional "package" filledInOptionals.package Encode.string, Argument.optional "severities" filledInOptionals.severities (Encode.enum Api.Enum.SecurityAdvisorySeverity.toString |> Encode.list), Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "vulnerabilities" optionalArgs object_ identity


{-| When the advisory was withdrawn, if it has been withdrawn
-}
withdrawnAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.SecurityAdvisory
withdrawnAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "withdrawnAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)