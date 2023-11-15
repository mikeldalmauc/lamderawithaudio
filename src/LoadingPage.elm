module LoadingPage exposing (..)


import Array
import AssocList
import Audio exposing (AudioCmd)
import Codec

import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.File.Download
import Effect.File.Select
import Effect.Lamdera
import Effect.Task
import Effect.Time as Time
import Effect.WebGL
import Effect.WebGL.Texture exposing (Texture)
import Html exposing (Html)
import Html.Attributes

import List.Extra as List
import List.Nonempty exposing (Nonempty(..))

import Pixels exposing (Pixels)
import Ports
import Quantity exposing (Quantity)
import Random
import Set exposing (Set)

import Sound

import Types exposing (FrontendLoaded, FrontendLoading, FrontendModel_(..), FrontendMsg_(..), ToBackend)

import WebGL.Texture


update :
    FrontendMsg_
    -> FrontendLoading
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update msg loadingModel =
    case msg of
        SoundLoaded sound result ->
            ( Loading { loadingModel | sounds = AssocList.insert sound result loadingModel.sounds }
            , Command.none
            , Audio.cmdNone
            )

        TextureLoaded result ->
            case result of
                Ok texture ->
                    ( Loading { loadingModel | texture = Just texture }, Command.none, Sound.load SoundLoaded )

                Err _ ->
                    ( Loading loadingModel, Command.none, Audio.cmdNone )

        _ ->
            ( Loading loadingModel, Command.none, Audio.cmdNone )

loadedInit : 
    Time.Posix -> 
    Texture ->
    FrontendLoading -> 
    ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit time texture loading  =
    let
      model =  
        { key = loading.key
        , message = loading.message
        , sounds = loading.sounds
        , musicVolume = loading.musicVolume
        , soundEffectVolume = loading.soundEffectVolume
        , pingData = Nothing
        , music =
                { startTime = Duration.addTo time (Duration.seconds 0)
                , sound = Sound.Music1
                }
        , time = loading.time
        , texture = texture
        }
  
    in
       (model , Command.batch [Effect.Lamdera.sendToBackend Types.PingRequest])
       |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))