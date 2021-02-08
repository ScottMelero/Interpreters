(* $Id: interp.ml,v 1.17 2020-10-29 10:22:11-07 - - $ *)

open Absyn
open Tables

let want_dump = ref false
let source_filename = ref ""

(* eval_expr *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> 
        Hashtbl.find Tables.unary_fn_table oper 
        (eval_expr expr)
    | Binary (oper, expr1, expr2) -> 
        Hashtbl.find Tables.binary_fn_table oper
        (eval_expr expr1) (eval_expr expr2)

(*evaluate boolean expressions*)
and eval_rel_expr (expr : Absyn.relexpr) : bool = match expr with
    | Relexpr (oper, expr1, expr2) -> 
        Hashtbl.find Tables.bool_fn_table oper
        (eval_expr expr1) (eval_expr expr2)

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> Array.get 
                (Hashtbl.find Tables.array_table ident)
                (Etc.int_of_round_float (eval_expr expr));
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0;

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
      | _, _, None -> interpret continue
      | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continue
    | Let (memref, expr) -> interp_let memref expr continue
    | Goto label -> interp_goto label continue
    | If (expr, label) -> interp_if expr label continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

(*interp let*)
and interp_let (memref : Absyn.memref) 
               (expr : Absyn.expr)
               (continue : Absyn.program) = 
                match memref with
    | Variable ident ->
        Hashtbl.replace Tables.variable_table ident (eval_expr expr);
    interpret continue
    | Arrayref (ident, exprr) -> 
        Array.set (Hashtbl.find Tables.array_table ident) 
        (Etc.int_of_round_float (eval_expr exprr)) 
        (eval_expr expr);
    interpret continue

(* let interp_goto *)
and interp_goto (label : Absyn.label) (continue : Absyn.program) = 
    interpret (Hashtbl.find Tables.label_table label)

(* let interp_dim *)
and interp_dim (ident : Absyn.ident)
               (expr : Absyn.expr)
               (continue : Absyn.program) = 
               Hashtbl.replace Tables.array_table ident 
               (* Hashtbl.add Tables.array_table ident *)
               (Array.make (Etc.int_of_round_float 
               (eval_expr expr)) 0.0);
    interpret continue

(* let interp_if *)
and interp_if (expr : Absyn.relexpr)
              (label : Absyn.label)
              (continue : Absyn.program) =
            if (eval_rel_expr expr) then interp_goto label continue
            else interpret continue


and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item =
        match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue

(*let Interp_input*)
and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        try let number = Etc.read_number ()
            in  match memref with
            | Variable ident ->   
                (Hashtbl.replace Tables.variable_table ident number)
            | Arrayref (ident, expr1) -> 
                Array.set (Hashtbl.find Tables.array_table ident) 
                (Etc.int_of_round_float (eval_expr expr1)) 
                number;
         with End_of_file -> 
             Etc.usage_exit ();
    in List.iter input_number memref_list; 
    interpret continue

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)