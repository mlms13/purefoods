module App.Routes where

data Route = Home | NotFound String

match :: String -> Route
match url = Home
