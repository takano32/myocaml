#!/usr/bin/env ocaml
(*
CGI look-up environment variable's value
http://www.cs.uvm.edu/~dvanhorn/ocaml/getenv.html
Cf.
http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-19.html#node_sec_17.2
*)

let getenv env_var =
        try Sys.getenv env_var
        with Not_found -> "";;

let split c s =
        let rec loop s =
                if s = "" then []
                else try
                        let i = String.index s c in
                        (String.sub s 0 i)::loop (String.sub s (i+1) ((String.length s)-(i+1)))
        with Not_found ->
                s::[]
                        in
                        loop s;;

let args =
        List.map
        (function par_arg -> split '=' par_arg)
        (split '&' (getenv "QUERY_STRING"));;

let print_vars () =
        List.iter
        (function "envvar"::x::[] ->
                print_string x;
                print_string " = ";
                print_string (getenv x);
                print_newline()
                | _ -> ())
        args;;

let main () =
        print_string "content-type: text/plain";
        print_newline();
        print_newline();
        print_newline();
        print_vars();
        exit 0;;

main();;
