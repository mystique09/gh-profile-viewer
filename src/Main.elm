module Main exposing (main)
import Json.Decode exposing (Error(..))
import Http
import Html exposing (Html, div, p, img, h1, text, input)
import Html.Events exposing (onInput)
import Browser
import Json.Decode exposing (Decoder, map6, field, int, string, maybe)
import Html.Attributes exposing (src, height, width, alt, style, placeholder)
import Http exposing (Error(..))
import Html exposing (Attribute)


-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
        
type Model =
    Failure (Maybe String)
    | Loading
    | Success User
    
type alias User =
    {
        id : Int
        , node_id : String
        , login : String
        , followers : Int
        , following : Int
        , avatar_url : String
    }

init : () -> (Model, Cmd Msg)
init _ = (Loading, getUser "mystique09")


-- UPDATE
type Msg =
    GetAnotherUserBasedOnString String
    | GotUser (Result Http.Error User)
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
    case msg of
        GetAnotherUserBasedOnString user ->
            (Loading, getUser user)
            
        GotUser res ->
            case res of
                Ok user ->
                    (Success user, Cmd.none)
                    
                Err err ->
                    case err of
                        BadStatus status ->
                            if status == 403 then
                                (Failure (Just "Too many request! You are ratelimmited."), Cmd.none)
                            else
                                (Failure Nothing, Cmd.none)
                                
                        BadBody bodyErr ->
                            (Failure (Just bodyErr), Cmd.none)
                        
                        BadUrl badUrlERr ->
                            (Failure (Just badUrlERr), Cmd.none)
                        
                        Timeout ->
                            (Failure (Just "Timedout!"), Cmd.none)
                            
                        NetworkError ->
                            (Failure (Just "Bad internet!"), Cmd.none)
                    
                    
-- SUBSCRIPTION
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
    
-- STYLES
containerStyle : List (Attribute Msg)
containerStyle =
    [
        style "padding" "2rem"
        , style "display" "grid"
        , style "place-items" "center"
    ]
    
loadingStyle : List (Attribute Msg)
loadingStyle =
    [
        style "display" "flex"
    ]
    
successStyle : List (Attribute Msg)
successStyle =
    [
        style "padding" "1rem"
        , style "border" "solid 1px"
        , style "min-width" "15rem"
        , style "max-width" "20rem"
    ]
    
-- VIEW
view : Model -> Html Msg
view model =
    div containerStyle 
        [
            h1 [style "text-align" "center"] [text "Github user fetcher"]
            , input [
                placeholder "Username"
                , onInput GetAnotherUserBasedOnString
                , style "margin-bottom" "1rem"
                ] [text "GetUser"]
            , viewUser model
        ]
    
-- User component
viewUser : Model -> Html Msg
viewUser model =
    case model of
        Loading ->
            div loadingStyle
                [
                    p [] [text "Fething user..."]
                ]
                
        Success  user ->
            div successStyle
                [ 
                    div [style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "1rem"
                        ] [
                        img [src user.avatar_url, height 50, width 50, alt user.login] []
                        , h1 [] [text user.login]
                    ]
                    , p [] [text ("NodeId: " ++ user.node_id)]
                    , p [] [text ("Id: " ++ String.fromInt user.id)]
                    , p [] [text ("Followers: " ++ String.fromInt user.followers)]
                    , p [] [text ("Following: " ++ String.fromInt user.following)]
                ]
                
        Failure err ->
            case err of
                Just message ->
                    div []
                    [
                        p [] [text message]
                    ]
                
                Nothing ->                    
                     div []
                    [
                        p [] [text "User not found!"]
                    ]
                
-- FUNCTIONS, Http Request
getUser : String -> Cmd Msg
getUser name =
    Http.get
        {
            url = "https://api.github.com/users/" ++ name
            , expect = Http.expectJson GotUser userDecoder
        }
        
        
userDecoder : Decoder User
userDecoder =
    map6 User
        (field "id" int)
        (field "node_id" string)        
        (field "login" string)
        (field "followers" int)
        (field "following" int)
        (field "avatar_url" string)