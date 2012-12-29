#!/bin/bash


ocamlbuild -classic-display util.byte
ocamlbuild -classic-display util.native
ocamlbuild -classic-display proc.byte
ocamlbuild -classic-display proc.native
