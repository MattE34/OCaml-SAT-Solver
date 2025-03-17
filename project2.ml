(* Matt Eng *)

(* Testing *)
(*
   #use "project2.ml";;
   #mod_use "project2.ml";;
   #load "str.cma";;
   #use "project2_driver.ml";;
*)

(*
   generateNextAssignments [("a", true);("b", true)];;
   buildCNF ["("; "a"; "OR"; "NOT"; "b"; "OR"; "c"; ")"; "AND"; "("; "b"; ")"];;
   buildCNF ["("; "NOT"; "a"; "OR"; "b"; ")"; "AND"; "("; "NOT"; "b"; ")"];;
*)

(*
   evaluateCNF [[("a", ""); ("b", "NOT")]; [("b", "")]] [("a", true); ("b", false)];;
   evaluateCNF [[("a", ""); ("b", "NOT")]; [("b", "")]] [("a", true); ("b", true)];;
   evaluateCNF [[("a", ""); ("b", "NOT")]; [("b", "")]] [("a", false); ("b", true)];;
*)

(*
   satisfy ["("; "a"; "OR"; "NOT"; "b"; ")"; "AND"; "("; "b"; ")"];;
*)


let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input: string list) (bound : string) : string list list =
  let rec aux acc sublist = function
      | [] -> List.rev (List.rev sublist :: acc)
      | hd :: tl when hd = bound -> aux (List.rev sublist :: acc) [] tl
      | hd :: tl -> aux acc (hd :: sublist) tl
    in
    aux [] [] input
;;

(* Helper Method *)
let process_clause (sublist : string list) : (string * string) list =
  let rec process_literal acc = function
    | [] -> acc
    | "NOT" :: var :: tl -> process_literal ((var, "NOT") :: acc) tl
    | var :: tl -> process_literal ((var, "") :: acc) tl
  in
  List.rev (process_literal [] sublist)
;;

let buildCNF (input : string list) : (string * string) list list =
  let rec process_input = function
    | [] -> []
    | "AND" :: tl -> process_input tl
    | "(" :: tl ->
        let rec process_clause acc = function
          | ")" :: rest -> List.rev acc, rest
          | hd :: tl when hd = "OR" -> process_clause acc tl
          | "NOT" :: var :: tl -> process_clause ((var, "NOT") :: acc) tl
          | var :: tl -> process_clause ((var, "") :: acc) tl
          | [] -> acc, []
        in
        let clause, rest = process_clause [] tl in
        clause :: process_input rest
    | _ :: tl -> process_input tl
  in
  process_input input
;;

let getVariables (input : string list) : string list = 
  let is_valid_var_name str =
    let str_len = String.length str in
    str_len > 0 && str_len <= 1 && List.mem str validVarNames
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl ->
        if is_valid_var_name hd && not (List.mem hd acc) then
          aux (hd :: acc) tl
        else
          aux acc tl
  in
  aux [] input
;;

let rec generateDefaultAssignments (varList : string list) : (string * bool) list = 
  match varList with
  | [] -> []
  | hd :: tl -> (hd, false) :: generateDefaultAssignments tl
;;

let rec generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool = 
  let rec increment_variable = function
    | [] -> []
    | (v, b) :: tl ->
      if b then
        (v, false) :: increment_variable tl
      else
        (v, true) :: tl
  in
  let rec carry_over = function
    | [] -> (assignList, true)
    | (v, b) :: tl ->
      if b then
        let rest, c = carry_over tl in
        ((v, false) :: rest, c)
      else
        ((v, true) :: tl, false)
  in
  let incremented = increment_variable (List.rev assignList) in
  if List.exists (fun (_, b) -> b) incremented then
    (List.rev incremented, false)
  else
    carry_over (List.rev incremented)

  (*let rec carry_over = function
    | [] -> [], true
    | (var, true) :: tl -> let rest, carry = carry_over tl in (var, false) :: rest, carry
    | (var, false) :: tl -> (var, true) :: tl, false
  in
  match assignList with
  | [] -> [], false
  | hd :: tl -> 
    let rest, carry = carry_over (List.rev tl) in
    (fst hd, not (snd hd)) :: (List.rev rest), carry*)
;;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool = 
  match assignList with
  | (var, value) :: tl ->
      if var = str then value
      else lookupVar tl str
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool = 
  let eval_clause clause =
    List.exists (fun (var, negation) ->
      if negation = "" then
        lookupVar assignList var
      else
        not (lookupVar assignList var)
    ) clause
  in
  List.for_all eval_clause t
;;

let satisfy (input : string list) : (string * bool) list =
  let rec try_assignments assignList =
    if evaluateCNF (buildCNF input) assignList then
      assignList
    else
      let next_assignment, carry = generateNextAssignments assignList in
      if carry then
        [("error", true)]
      else
        try_assignments next_assignment
  in
  try_assignments (generateDefaultAssignments (getVariables input))
;;