module Vent.VentScript exposing (Error(..), compile)

import Parser.Advanced as Parser
import Result exposing (Result(..))
import Vent.ObjectStore exposing (ObjectStore)
import Vent.VentScript.Canonicalize as Canonicalize
import Vent.VentScript.Parse as Parse
import Vent.VentScript.Script as Script exposing (Script)


compile : String -> ObjectStore -> String -> Result Error Script
compile localObject objectStore input =
    input
        |> parse
        |> Result.andThen (canonicalize localObject objectStore)


type Error
    = ParseError Parse.Error
    | CanonicalizeError Canonicalize.Error


parse : String -> Result Error Parse.Script
parse input =
    case Parse.execute input of
        Ok script ->
            Ok script

        Err error ->
            Err (ParseError error)


canonicalize : String -> ObjectStore -> Parse.Script -> Result Error Script
canonicalize localObject objectStore input =
    case Canonicalize.execute localObject objectStore input of
        Ok script ->
            Ok script

        Err error ->
            Err (CanonicalizeError error)
