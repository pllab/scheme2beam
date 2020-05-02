

let load_scm_file filepath = 
  let file = open_in filepath in
  let lexbuf = Lexing.from_channel file in
  (* let rec loop () = *)
  let cexpr = (Parser.parse Lexer.lex) lexbuf in 
  match cexpr with
  | None -> print_string "empty"
  | Some s -> begin
      match s with
      | Int i -> print_string (string_of_int i)
      | Atom a -> print_string a
    end
       (* let expr = Env.value_of_sexpr s in *) (* todo *)
       (* ignore (Eval.eval expr env macros); *) (* todo *)
  (*      loop () in
   * (\* Change the current working directory.
   *  * Important for (load ...) when a relative path is given. *\)
   * let original_cwd = Sys.getcwd () in
   * Sys.chdir (Filename.dirname filepath);
   * (\* Evaluate file. *\)
   * loop ();
   * close_in file;
   * (\* Restore the current working directory. *\)
   * Sys.chdir original_cwd
   * (\* WARN: catch errors and close file properly. *\) *)


let () =
  match Sys.argv with
  | [|_; "-h"|] | [|_; "--help"|] ->
     let usage = "usage: ./scheme [filename] [-h | --help]" in
     print_endline usage
  | [|_; path|] ->
  (* let res = load_scm_file path in print_string (int_of_string res) *)
     load_scm_file path
  | _ ->
     prerr_endline "Invalid arguments provided.";
     exit 1
