module MacOS.FileSystem exposing (FileSystem, Volume, name, new, volume, volumes)


type FileSystem
    = FileSystem Internals


type alias Internals =
    { volumes : List Volume
    }


type Volume
    = Volume String (List File)


name : Volume -> String
name (Volume n _) =
    n


type File
    = Folder String (List File)
    | Data String
    | Executable String


volume : String -> List File -> Volume
volume n children =
    Volume n children


volumes : FileSystem -> List Volume
volumes (FileSystem internals) =
    internals.volumes


new : List Volume -> FileSystem
new vs =
    FileSystem
        { volumes = vs
        }
