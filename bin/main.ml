open Sexplib
open PPrint

(*----------------------------------------------------------------------------*)
let op_prec = function
  | "and" -> 100
  | "or" -> 110
  | "not" -> 60
  | "=>" -> 120
  | _ -> 0

let rec op_prec_sexpr = function
  | Sexp.List args -> op_prec_sexpr (List.hd args)
  | Sexp.Atom a -> op_prec a

let unpackAtom = function Sexp.Atom a -> a | _ -> "n/a"

let rec expandTerm op exprs =
  let opd = space ^^ PPrint.string op ^^ space in
  let prec = op_prec op in
  PPrint.flow opd
    (List.map
       (fun x ->
         if prec >= op_prec_sexpr x then term2KeY x
         else PPrint.parens (term2KeY x))
       exprs)

and term2KeY = function
  | Sexp.Atom a -> PPrint.string a
  | Sexp.List args -> (
      let hd = unpackAtom (List.hd args) in
      let tl = List.tl args in
      match hd with
      | "and" -> expandTerm "&" tl
      | "or" -> expandTerm "|" tl
      | "=>" -> expandTerm "->" tl
      | "+" -> expandTerm "+" tl
      | "-" -> expandTerm "-" tl
      | "*" -> expandTerm "*" tl
      | "/" -> expandTerm "/" tl
      | "mod" -> expandTerm "%" tl
      | "<" -> expandTerm "%" tl
      | "<=" -> expandTerm "<=" tl
      | ">" -> expandTerm ">" tl
      | ">=" -> expandTerm ">=" tl
      | "=" -> expandTerm "=" tl
      | _ ->
        PPrint.precede (PPrint.string hd)
          (PPrint.parens
             (PPrint.nest 4 (PPrint.separate_map
                               (PPrint.precede PPrint.space PPrint.comma)
                               term2KeY tl))))

(*----------------------------------------------------------------------------*)

let pHeader filename =
  (* Gives the Header of the KeY file *)
  string "// KeY File generated of" ^^ space ^^ string filename ^^ hardline

let pSection name fn sexprs =
  let x = List.filter_map fn sexprs in
  let doc = PPrint.separate PPrint.hardline x in
  hardline ^^  string "\\" ^^ !^name ^^ braces (nest 4 (hardline ^^ doc)^^hardline)

let pSort = function
  | Sexp.List [ Sexp.Atom "define-sort"; Sexp.Atom name ] ->
      Some
        (PPrint.concat [ PPrint.utf8string name; PPrint.semi; PPrint.hardline ])
  | _ -> None

let rec pFunction = function
  | Sexp.List
      [ Sexp.Atom "declare-fun"; Sexp.Atom name; Sexp.List args; Sexp.Atom ret ]
    ->
      buildFunction name args ret
  | Sexp.List
      [
        Sexp.Atom "define-fun"; Sexp.Atom name; Sexp.List args; Sexp.Atom ret; _;
      ] ->
      buildFunction name args ret
      (* (List.map (fun x -> match x with
           | Sexp.List n::_ -> n
           | _ -> Sexp.Atom "") args)*)
  | _ -> None

and buildFunction name args ret =
  if ret = "bool" then None
  else if List.length args = 0 then
    Some
      (PPrint.concat
         [
           PPrint.string ret; PPrint.space; PPrint.utf8string name; PPrint.semi;
         ])
  else
    Some
      (PPrint.concat
         [
           PPrint.string ret;
           PPrint.space;
           PPrint.utf8string name;
           PPrint.parens
             (PPrint.separate_map
                (PPrint.precede PPrint.comma PPrint.space)
                (fun x ->
                  match x with
                  | Sexp.Atom y -> PPrint.string y
                  | Sexp.List [ Sexp.Atom n ] -> PPrint.string n
                  | _ -> PPrint.empty)
                args);
           PPrint.semi;
         ])

let pPredicate = function
  | Sexp.List [ Sexp.Atom "define-fun"; Sexp.Atom name; _; Sexp.Atom "Bool" ] ->
      Some
        (PPrint.concat [ PPrint.utf8string name; PPrint.semi; PPrint.hardline ])
  | _ -> None


let break0 =  break 0
let break1 =  break 1

let buildRuleForFunction name args ret def =
  string ("def" ^ name) ^^ space
  ^^ braces
    (nest 4
       (break0 ^^
     (PPrint.separate_map break0
        (fun x ->
           match x with
           | Sexp.List [ Sexp.Atom name; Sexp.Atom sort ] ->
             !^"\\schemaVar" ^^ space ^^ !^"\\term " ^^ !^sort ^^ space
             ^^ !^name ^^semi
           | _ -> empty)
        args
      ^^ break0
      ^^ string "\\find"
      ^^ parens(!^name ^^ parens
                  (nest 4
                     (PPrint.separate_map (comma ^^ space)
                        (fun x ->
                           match x with
                           | Sexp.List [ Sexp.Atom name; _ ] -> !^name
                           | _ -> empty)
                        args)))
      ^^ break0 ^^ !^"\\replacewith" ^^ PPrint.parens (term2KeY def)))^^break0)


let pRule = function
  (* Tries to construct proof rules for declare functions *)
  | Sexp.List
      [
        Sexp.Atom "define-fun";
        Sexp.Atom name;
        Sexp.List args;
        Sexp.Atom ret;
        def
      ] ->
      Some (buildRuleForFunction name args ret def)
  | _ -> None

let pprint doc = ToChannel.pretty 0.9 80 stdout doc

let pProblem sexprs =
  let assertions =
    List.filter_map
      (function
        | Sexp.List [ Sexp.Atom "assert"; term ] -> Some term | _ -> None)
      sexprs
  in
  let a = Sexp.Atom "and" :: assertions in
  let sexpr = Sexp.List a in
  let term = term2KeY sexpr in
  hardline ^^ !^"\\problem" ^^ braces (term ^^ space ^^ !^ "==>")

let pFooter = hardline

let build_goals sexprs =
  let rec loop sexprs goals stack =
    match sexprs with
    | Sexp.List (Sexp.Atom "check-sat" :: _) :: rest ->
        loop rest (List.rev (List.flatten stack) :: goals) stack
    | Sexp.List (Sexp.Atom "pop" :: _) :: rest ->
        loop rest goals (List.tl stack)
    | Sexp.List (Sexp.Atom "push" :: _) :: rest -> loop rest goals ([] :: stack)
    | (_ as term) :: rest ->
        let firstStack = if 0 = List.length stack then [] else List.hd stack in
        let restStack = if 0 = List.length stack then [] else List.tl stack in
        loop rest goals ((term :: firstStack) :: restStack)
    | [] -> goals
  in
  loop sexprs [] []

let smt2key filename =
  let sexprs = Sexp.load_sexps filename in
  let goals = build_goals sexprs in

  let docs =
    List.map
      (fun dt ->
        pHeader filename ^^ pSection "sorts" pSort dt
        ^^ pSection "functions" pFunction dt
        ^^ pSection "predicates" pSort dt
        ^^ pSection "rules" pRule dt
        ^^ pProblem dt ^^ pFooter)
      goals
  in
  List.iter (fun doc -> ToChannel.pretty 0.9 80 stdout doc) docs

open Sys

let () =
  if Array.length Sys.argv > 1 then
    let arg = Array.get Sys.argv 1 in
    smt2key arg
  else print_endline "Insufficient Arguments"
