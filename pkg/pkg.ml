#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "sturgeon" @@ fun c ->
  Ok [ Pkg.mllib "src/sturgeon.mllib"
     ; Pkg.mllib "src/sturgeon_recipes_command.mllib"
     ; Pkg.mllib "src/sturgeon_recipes_server.mllib"
     ]
