-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.BranchProtectionRuleConflict exposing (branchProtectionRule, conflictingBranchProtectionRule, ref)

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


{-| Identifies the branch protection rule.
-}
branchProtectionRule : SelectionSet decodesTo Api.Object.BranchProtectionRule -> SelectionSet (Maybe decodesTo) Api.Object.BranchProtectionRuleConflict
branchProtectionRule object_ =
    Object.selectionForCompositeField "branchProtectionRule" [] object_ (identity >> Decode.nullable)


{-| Identifies the conflicting branch protection rule.
-}
conflictingBranchProtectionRule : SelectionSet decodesTo Api.Object.BranchProtectionRule -> SelectionSet (Maybe decodesTo) Api.Object.BranchProtectionRuleConflict
conflictingBranchProtectionRule object_ =
    Object.selectionForCompositeField "conflictingBranchProtectionRule" [] object_ (identity >> Decode.nullable)


{-| Identifies the branch ref that has conflicting rules
-}
ref : SelectionSet decodesTo Api.Object.Ref -> SelectionSet (Maybe decodesTo) Api.Object.BranchProtectionRuleConflict
ref object_ =
    Object.selectionForCompositeField "ref" [] object_ (identity >> Decode.nullable)