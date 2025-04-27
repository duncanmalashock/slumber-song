module Vent exposing (Error(..), compile)

import ObjectStore exposing (ObjectStore)
import Parser.Advanced as Parser
import Result exposing (Result(..))
import Script exposing (Script)
import Vent.Canonicalize
import Vent.Parse



-- VENT: the Vintage Exploratory-Narrative Toolkit


compile : String -> ObjectStore -> String -> Result Error Script
compile localObject objectStore input =
    input
        |> parse
        |> Result.andThen (canonicalize localObject objectStore)


type Error
    = ParseError Vent.Parse.Error
    | CanonicalizeError Vent.Canonicalize.Error


parse : String -> Result Error Vent.Parse.Script
parse input =
    case Vent.Parse.execute input of
        Ok script ->
            Ok script

        Err error ->
            Err (ParseError error)


canonicalize : String -> ObjectStore -> Vent.Parse.Script -> Result Error Script
canonicalize localObject objectStore input =
    case Vent.Canonicalize.execute localObject objectStore input of
        Ok script ->
            Ok script

        Err error ->
            Err (CanonicalizeError error)
