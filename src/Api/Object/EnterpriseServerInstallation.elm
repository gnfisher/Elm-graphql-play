-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.EnterpriseServerInstallation exposing (UserAccountsOptionalArguments, UserAccountsUploadsOptionalArguments, createdAt, customerName, hostName, id, isConnected, updatedAt, userAccounts, userAccountsUploads)

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
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.EnterpriseServerInstallation
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The customer name to which the Enterprise Server installation belongs.
-}
customerName : SelectionSet String Api.Object.EnterpriseServerInstallation
customerName =
    Object.selectionForField "String" "customerName" [] Decode.string


{-| The host name of the Enterprise Server installation.
-}
hostName : SelectionSet String Api.Object.EnterpriseServerInstallation
hostName =
    Object.selectionForField "String" "hostName" [] Decode.string


id : SelectionSet Api.ScalarCodecs.Id Api.Object.EnterpriseServerInstallation
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Whether or not the installation is connected to an Enterprise Server installation via GitHub Connect.
-}
isConnected : SelectionSet Bool Api.Object.EnterpriseServerInstallation
isConnected =
    Object.selectionForField "Bool" "isConnected" [] Decode.bool


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.EnterpriseServerInstallation
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


type alias UserAccountsOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.EnterpriseServerUserAccountOrder
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| User accounts on this Enterprise Server installation.

  - orderBy - Ordering options for Enterprise Server user accounts returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
userAccounts : (UserAccountsOptionalArguments -> UserAccountsOptionalArguments) -> SelectionSet decodesTo Api.Object.EnterpriseServerUserAccountConnection -> SelectionSet decodesTo Api.Object.EnterpriseServerInstallation
userAccounts fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeEnterpriseServerUserAccountOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "userAccounts" optionalArgs object_ identity


type alias UserAccountsUploadsOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.EnterpriseServerUserAccountsUploadOrder
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| User accounts uploads for the Enterprise Server installation.

  - orderBy - Ordering options for Enterprise Server user accounts uploads returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
userAccountsUploads : (UserAccountsUploadsOptionalArguments -> UserAccountsUploadsOptionalArguments) -> SelectionSet decodesTo Api.Object.EnterpriseServerUserAccountsUploadConnection -> SelectionSet decodesTo Api.Object.EnterpriseServerInstallation
userAccountsUploads fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeEnterpriseServerUserAccountsUploadOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "userAccountsUploads" optionalArgs object_ identity
