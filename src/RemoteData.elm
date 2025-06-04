module RemoteData exposing (RemoteData(..), map)


type RemoteData err data
    = NotLoaded
    | Loading
    | LoadSuccessful data
    | LoadFailed err


map : (a -> b) -> RemoteData err a -> RemoteData err b
map fn remoteData =
    case remoteData of
        NotLoaded ->
            NotLoaded

        Loading ->
            Loading

        LoadSuccessful data ->
            LoadSuccessful (fn data)

        LoadFailed err ->
            LoadFailed err
