module Main exposing (main)

import Api.Object
import Api.Object.Repository
import Api.Object.RepositoryConnection
import Api.Object.User
import Api.Query
import Browser
import Graphql.Http
import Graphql.Operation
import Graphql.OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Http
import Process
import Task


type Model
    = Loading
    | Loaded (List String)
    | Failure


type alias Flags =
    ()


type alias GraphqlResult a =
    Result (Graphql.Http.Error a) a


type Msg
    = ReceiveRepos (GraphqlResult (List String))


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRepos )


getRepos : Cmd Msg
getRepos =
    Graphql.Http.queryRequest "https://api.github.com/graphql" bioSelection
        |> Graphql.Http.withHeader "Authorization" "Bearer <GITHUB_KEY>"
        |> Graphql.Http.send ReceiveRepos


bioSelection : SelectionSet (List String) Graphql.Operation.RootQuery
bioSelection =
    SelectionSet.map (Maybe.withDefault []) namesOnUser


namesOnUser : SelectionSet (Maybe (List String)) Graphql.Operation.RootQuery
namesOnUser =
    Api.Query.user { login = "<GITHIB_USERNAME" } namesOnRepo


namesOnRepo : SelectionSet (List String) Api.Object.User
namesOnRepo =
    Api.Object.User.repositories
        (\args -> { args | first = Graphql.OptionalArgument.Present 25 })
        names


names : SelectionSet (List String) Api.Object.RepositoryConnection
names =
    SelectionSet.map cleanList rawNames


cleanList : Maybe (List (Maybe a)) -> List a
cleanList dirty =
    dirty
        |> Maybe.map (List.filterMap identity)
        |> Maybe.withDefault []


rawNames : SelectionSet (Maybe (List (Maybe String))) Api.Object.RepositoryConnection
rawNames =
    Api.Object.RepositoryConnection.nodes repoName


repoName : SelectionSet String Api.Object.Repository
repoName =
    Api.Object.Repository.name


view : Model -> Html msg
view model =
    case model of
        Loading ->
            Html.text "Loading . . ."

        Loaded repos ->
            Html.ul [] <|
                List.map (\el -> Html.li [] [ Html.text el ]) repos

        Failure ->
            Html.text "YOU LOSE"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveRepos (Ok repos) ->
            ( Loaded repos, Cmd.none )

        ReceiveRepos (Err _) ->
            ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
