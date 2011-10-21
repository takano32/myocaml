#!/usr/bin/env ocaml

let make_stream filename =
        let ich = open_in filename in
        Stream.from (fun i -> try Some (input_line ich) with End_of_file ->
                close_in ich; None);;

let filename = "catn.ml" in
        Stream.iter
                (let lineno = ref 0 in
                        fun l -> incr lineno; Printf.printf "%d: %s\n" !lineno l)
                (make_stream filename)


