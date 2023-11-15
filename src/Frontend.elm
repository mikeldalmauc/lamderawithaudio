module Frontend exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Lamdera
import Types exposing (..)
import Url exposing (Url)
import Lamdera
import Audio exposing (Audio, AudioCmd, AudioData)
import Effect.Lamdera
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Browser.Navigation
import Effect.Time as Time
import Effect.Task
import Effect.Time
import PingData
import Sound exposing (Sound)
import Duration exposing (Duration)
import Ports
import Effect.Browser.Dom exposing (Element)
import Element as El exposing (layout)
import Element.Input as EI
import Element.Events as EV
import AssocList
import Sound exposing (Sound(..))
import Random
import LoadingPage
import Effect.WebGL.Texture exposing (Texture)

app :
    { init : Url -> Lamdera.Key -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , view : Audio.Model FrontendMsg_ FrontendModel_ -> Browser.Document (Audio.Msg FrontendMsg_)
    , update :
        Audio.Msg FrontendMsg_
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , updateFromBackend :
        ToFrontend
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , subscriptions : Audio.Model FrontendMsg_ FrontendModel_ -> Sub (Audio.Msg FrontendMsg_)
    , onUrlRequest : Browser.UrlRequest -> Audio.Msg FrontendMsg_
    , onUrlChange : Url -> Audio.Msg FrontendMsg_
    }
app =
    Effect.Lamdera.frontend Lamdera.sendToBackend app_


app_ :
    { init :
        Url
        -> Effect.Browser.Navigation.Key
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , view : Audio.Model FrontendMsg_ FrontendModel_ -> Browser.Document (Audio.Msg FrontendMsg_)
    , update :
        Audio.Msg FrontendMsg_
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , updateFromBackend :
        ToFrontend
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , subscriptions :
        Audio.Model FrontendMsg_ FrontendModel_
        -> Subscription FrontendOnly (Audio.Msg FrontendMsg_)
    , onUrlRequest : Browser.UrlRequest -> Audio.Msg FrontendMsg_
    , onUrlChange : Url -> Audio.Msg FrontendMsg_
    }
app_ =
    Audio.lamderaFrontendWithAudio
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = \audioData msg model -> update audioData msg model
        , updateFromBackend = \_ msg model -> updateFromBackend msg model |> (\( a, b ) -> ( a, b, Audio.cmdNone ))
        , subscriptions = subscriptions
        , view = view
        , audio = audio
        , audioPort =
            { toJS = Command.sendToJs "audioPortToJS" Ports.audioPortToJS
            , fromJS = Subscription.fromJs "audioPortFromJS" Ports.audioPortFromJS
            }
        }


audio : AudioData -> FrontendModel_ -> Audio
audio audioData model =
    case model of
        Loaded loaded ->
            audioLoaded audioData loaded

        Loading _ ->
            Audio.silence


audioLoaded : AudioData -> FrontendLoaded -> Audio
audioLoaded audioData model =
    let

        timeOffset =
            PingData.pingOffset model

        playSound sound time =
            Sound.play model sound (Duration.subtractFrom time timeOffset)

        playWithConfig config sound time =
            Sound.playWithConfig audioData model config sound (Duration.subtractFrom time timeOffset)


    in
        playSound model.music.sound model.music.startTime |> Audio.scaleVolume 0.5



maxVolumeDistance : number
maxVolumeDistance =
    10


type alias Model =
    FrontendModel

init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    ( Loading
        { key = key
        , message = ""
        , sounds = AssocList.empty
        , musicVolume = 0
        , soundEffectVolume = 0
        , time = Nothing
        , texture = Nothing
        }
    , Command.batch []
    , Audio.cmdNone
    )


update : AudioData -> FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update audioData msg model =
    case model of
        Loading loadingModel ->
            LoadingPage.update msg loadingModel

        Loaded frontendLoaded ->
            updateLoaded audioData msg frontendLoaded
            |>  (\( newModel, cmd ) ->
                (Loaded newModel, cmd, Audio.cmdNone)
            )


updateLoaded : AudioData -> FrontendMsg_ -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoaded audioData msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    ,  Command.batch [Effect.Browser.Navigation.pushUrl model.key (Url.toString url)]
                    )

                Browser.External url ->
                    ( model
                    , Command.batch [Effect.Browser.Navigation.load url]
                    )

        UrlChanged url ->
            ( model, Command.batch [])

        NoOpFrontendMsg ->
            ( model, Command.batch [])

        ShortIntervalElapsed time ->
            ( model, Command.batch [])

        SoundLoaded sound result ->
            ( { model | sounds = AssocList.insert sound result model.sounds }, Command.none )

        TextureLoaded _ ->
            ( model, Command.none )




updateFromBackend : ToFrontend -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_ )
updateFromBackend msg model =
    case ( model, msg ) of
        ( Loading loading , NoOpToFrontend) ->
            (Loading loading, Command.none )

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded
                |> Tuple.mapFirst Loaded

        _ ->
            (model, Command.none )

updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoadedFromBackend msg model =
    case msg of
        PingResponse serverTime ->
            (model, Command.none )
        _ -> 
            (model, Command.none )


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view audioData model =
    { title =
        case model of
            Loading _ ->
                "Gazta "

            Loaded loaded ->
                "Gazta"
    , body =
        [ case model of
            Loading loadingModel ->
                loadingView

            Loaded loadedModel ->
                loadedView audioData loadedModel
        , Html.node "style" [] [ Html.text "body { overflow: hidden; margin: 0; }" ]
        ]
    }

loadingView : Html FrontendMsg_
loadingView = 
    El.layout []
        <| El.row []
            [El.el [El.centerX, El.centerY] <| El.text "loading"
            , EI.button [ EV.onClick  NoOpFrontendMsg]  
                    { onPress = Just NoOpFrontendMsg
                    , label = El.text "Run" 
                    }
            ]


loadedView : AudioData -> FrontendLoaded ->  Html FrontendMsg_
loadedView audioData model = 
    El.layout []
        <| El.el [El.centerX, El.centerY] <| El.text "loaded"


subscriptions : AudioData -> FrontendModel_ -> Subscription FrontendOnly FrontendMsg_
subscriptions _ model =
    Subscription.batch
        [ 
            case model of
                Loading _ ->
                    Subscription.batch
                        [ ]

                Loaded loaded ->
                    Subscription.batch
                        [ Effect.Time.every
                            (Duration.milliseconds 100)
                            (\time -> Duration.addTo time (PingData.pingOffset loaded) |> ShortIntervalElapsed)
                        ]
        ]
