module Types exposing (..)

import Browser exposing (UrlRequest)
import Url exposing (Url)
import Audio
import AssocList
import Sound exposing (Sound)
import PingData exposing (PingData)
import Effect.Time
import Effect.Browser.Navigation
import Effect.WebGL.Texture

type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded
    

type alias FrontendLoading = 
    { key : Effect.Browser.Navigation.Key
    , message : String
    , sounds : AssocList.Dict Sound (Result Audio.LoadError Audio.Source)
    , musicVolume : Int
    , soundEffectVolume : Int
    , time : Maybe Effect.Time.Posix
    , texture : Maybe Effect.WebGL.Texture.Texture
    }

type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , message : String
    , sounds : AssocList.Dict Sound (Result Audio.LoadError Audio.Source)
    , musicVolume : Int
    , soundEffectVolume : Int
    , pingData : Maybe PingData
    , music : { startTime : Effect.Time.Posix, sound : Sound }
    , time : Maybe Effect.Time.Posix
    , texture : Effect.WebGL.Texture.Texture
    }


type alias BackendModel =
    { message : String
    }

type alias FrontendMsg =
    Audio.Msg FrontendMsg_

type FrontendMsg_
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SoundLoaded Sound (Result Audio.LoadError Audio.Source)
    | TextureLoaded (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | ShortIntervalElapsed Effect.Time.Posix

type ToBackend
    = NoOpToBackend
    | PingRequest


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | PingResponse Effect.Time.Posix
