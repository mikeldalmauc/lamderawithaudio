module Sound exposing (Model, Sound(..), length, load, maxVolume, nextSong, play, playWithConfig)

import AssocList as Dict exposing (Dict)
import Audio exposing (Audio, AudioCmd, AudioData, PlayAudioConfig)
import Duration exposing (Duration)
import Effect.Time
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Quantity
import Random


type alias Model a =
    { a
        | sounds : Dict Sound (Result Audio.LoadError Audio.Source)
        , musicVolume : Int
        , soundEffectVolume : Int
    }


type Sound
    = Music0
    | Music1
    | Music2
    


allSounds : List Sound
allSounds =
    [ 
      Music0
    , Music1
    , Music2
    ]


songs : Nonempty Sound
songs =
    Nonempty Music0 [ Music1, Music2 ]


isMusic : Sound -> Bool
isMusic sound =
    List.Nonempty.any ((==) sound) songs


nextSong : Maybe Sound -> Random.Generator Sound
nextSong maybePreviousSong =
    case maybePreviousSong of
        Just previousSong ->
            case List.Nonempty.toList songs |> List.remove previousSong |> List.Nonempty.fromList of
                Just nonempty ->
                    List.Nonempty.sample nonempty

                Nothing ->
                    List.Nonempty.sample songs

        Nothing ->
            List.Nonempty.sample songs


maxVolume : number
maxVolume =
    10


play : Model a -> Sound -> Effect.Time.Posix -> Audio
play model sound startTime =
    case Dict.get sound model.sounds of
        Just (Ok audio) ->
            Audio.audio audio startTime
                |> (if isMusic sound then
                        Audio.scaleVolume (toFloat model.musicVolume / maxVolume)

                    else
                        Audio.scaleVolume (toFloat model.soundEffectVolume / maxVolume)
                   )

        _ ->
            Audio.silence


playWithConfig : AudioData -> Model a -> (Duration -> PlayAudioConfig) -> Sound -> Effect.Time.Posix -> Audio
playWithConfig audioData model config sound startTime =
    case Dict.get sound model.sounds of
        Just (Ok audio) ->
            Audio.audioWithConfig (config (Audio.length audioData audio)) audio startTime
                |> (if isMusic sound then
                        Audio.scaleVolume (toFloat model.musicVolume / maxVolume)

                    else
                        Audio.scaleVolume (toFloat model.soundEffectVolume / maxVolume)
                   )

        _ ->
            Audio.silence


load : (Sound -> Result Audio.LoadError Audio.Source -> msg) -> AudioCmd msg
load onLoad =
    List.map
        (\sound ->
            ("/"
                ++ (case sound of

                        Music0 ->
                            "grasslands.mp3"

                        Music1 ->
                            "dawn.mp3"

                        Music2 ->
                            "now-arriving-at.mp3"
                   )
            )
                |> Audio.loadAudio (onLoad sound)
        )
        allSounds
        |> Audio.cmdBatch


length : AudioData -> Dict Sound (Result Audio.LoadError Audio.Source) -> Sound -> Duration
length audioData dict sound =
    case Dict.get sound dict of
        Just (Ok audio) ->
            Audio.length audioData audio

        _ ->
            Quantity.zero
