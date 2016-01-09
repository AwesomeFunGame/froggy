module Froggy.Util where

import Maybe exposing (..)

remove : List a -> a -> List a
remove xs x = xs |> List.filter (\element -> element /= x)

getOrElse : a -> Maybe a -> a
getOrElse defaultValue maybeValue = maybeValue |> withDefault defaultValue
--getOrElse defaultValue maybeValue = maybeValue |> maybe defaultValue identity

distance : Int -> Int -> Int
distance a b = abs(a - b)