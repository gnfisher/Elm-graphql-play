-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.BranchProtectionRule exposing (BranchProtectionRuleConflictsOptionalArguments, MatchingRefsOptionalArguments, PushAllowancesOptionalArguments, ReviewDismissalAllowancesOptionalArguments, branchProtectionRuleConflicts, creator, databaseId, dismissesStaleReviews, id, isAdminEnforced, matchingRefs, pattern, pushAllowances, repository, requiredApprovingReviewCount, requiredStatusCheckContexts, requiresApprovingReviews, requiresCodeOwnerReviews, requiresCommitSignatures, requiresStatusChecks, requiresStrictStatusChecks, restrictsPushes, restrictsReviewDismissals, reviewDismissalAllowances)

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


type alias BranchProtectionRuleConflictsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of conflicts matching branches protection rule and other branch protection rules

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
branchProtectionRuleConflicts : (BranchProtectionRuleConflictsOptionalArguments -> BranchProtectionRuleConflictsOptionalArguments) -> SelectionSet decodesTo Api.Object.BranchProtectionRuleConflictConnection -> SelectionSet decodesTo Api.Object.BranchProtectionRule
branchProtectionRuleConflicts fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "branchProtectionRuleConflicts" optionalArgs object_ identity


{-| The actor who created this branch protection rule.
-}
creator : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.BranchProtectionRule
creator object_ =
    Object.selectionForCompositeField "creator" [] object_ (identity >> Decode.nullable)


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Api.Object.BranchProtectionRule
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| Will new commits pushed to matching branches dismiss pull request review approvals.
-}
dismissesStaleReviews : SelectionSet Bool Api.Object.BranchProtectionRule
dismissesStaleReviews =
    Object.selectionForField "Bool" "dismissesStaleReviews" [] Decode.bool


id : SelectionSet Api.ScalarCodecs.Id Api.Object.BranchProtectionRule
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Can admins overwrite branch protection.
-}
isAdminEnforced : SelectionSet Bool Api.Object.BranchProtectionRule
isAdminEnforced =
    Object.selectionForField "Bool" "isAdminEnforced" [] Decode.bool


type alias MatchingRefsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| Repository refs that are protected by this rule

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
matchingRefs : (MatchingRefsOptionalArguments -> MatchingRefsOptionalArguments) -> SelectionSet decodesTo Api.Object.RefConnection -> SelectionSet decodesTo Api.Object.BranchProtectionRule
matchingRefs fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "matchingRefs" optionalArgs object_ identity


{-| Identifies the protection rule pattern.
-}
pattern : SelectionSet String Api.Object.BranchProtectionRule
pattern =
    Object.selectionForField "String" "pattern" [] Decode.string


type alias PushAllowancesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list push allowances for this branch protection rule.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
pushAllowances : (PushAllowancesOptionalArguments -> PushAllowancesOptionalArguments) -> SelectionSet decodesTo Api.Object.PushAllowanceConnection -> SelectionSet decodesTo Api.Object.BranchProtectionRule
pushAllowances fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "pushAllowances" optionalArgs object_ identity


{-| The repository associated with this branch protection rule.
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet (Maybe decodesTo) Api.Object.BranchProtectionRule
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ (identity >> Decode.nullable)


{-| Number of approving reviews required to update matching branches.
-}
requiredApprovingReviewCount : SelectionSet (Maybe Int) Api.Object.BranchProtectionRule
requiredApprovingReviewCount =
    Object.selectionForField "(Maybe Int)" "requiredApprovingReviewCount" [] (Decode.int |> Decode.nullable)


{-| List of required status check contexts that must pass for commits to be accepted to matching branches.
-}
requiredStatusCheckContexts : SelectionSet (Maybe (List (Maybe String))) Api.Object.BranchProtectionRule
requiredStatusCheckContexts =
    Object.selectionForField "(Maybe (List (Maybe String)))" "requiredStatusCheckContexts" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| Are approving reviews required to update matching branches.
-}
requiresApprovingReviews : SelectionSet Bool Api.Object.BranchProtectionRule
requiresApprovingReviews =
    Object.selectionForField "Bool" "requiresApprovingReviews" [] Decode.bool


{-| Are reviews from code owners required to update matching branches.
-}
requiresCodeOwnerReviews : SelectionSet Bool Api.Object.BranchProtectionRule
requiresCodeOwnerReviews =
    Object.selectionForField "Bool" "requiresCodeOwnerReviews" [] Decode.bool


{-| Are commits required to be signed.
-}
requiresCommitSignatures : SelectionSet Bool Api.Object.BranchProtectionRule
requiresCommitSignatures =
    Object.selectionForField "Bool" "requiresCommitSignatures" [] Decode.bool


{-| Are status checks required to update matching branches.
-}
requiresStatusChecks : SelectionSet Bool Api.Object.BranchProtectionRule
requiresStatusChecks =
    Object.selectionForField "Bool" "requiresStatusChecks" [] Decode.bool


{-| Are branches required to be up to date before merging.
-}
requiresStrictStatusChecks : SelectionSet Bool Api.Object.BranchProtectionRule
requiresStrictStatusChecks =
    Object.selectionForField "Bool" "requiresStrictStatusChecks" [] Decode.bool


{-| Is pushing to matching branches restricted.
-}
restrictsPushes : SelectionSet Bool Api.Object.BranchProtectionRule
restrictsPushes =
    Object.selectionForField "Bool" "restrictsPushes" [] Decode.bool


{-| Is dismissal of pull request reviews restricted.
-}
restrictsReviewDismissals : SelectionSet Bool Api.Object.BranchProtectionRule
restrictsReviewDismissals =
    Object.selectionForField "Bool" "restrictsReviewDismissals" [] Decode.bool


type alias ReviewDismissalAllowancesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list review dismissal allowances for this branch protection rule.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
reviewDismissalAllowances : (ReviewDismissalAllowancesOptionalArguments -> ReviewDismissalAllowancesOptionalArguments) -> SelectionSet decodesTo Api.Object.ReviewDismissalAllowanceConnection -> SelectionSet decodesTo Api.Object.BranchProtectionRule
reviewDismissalAllowances fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "reviewDismissalAllowances" optionalArgs object_ identity
