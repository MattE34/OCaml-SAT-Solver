
let validVarNames = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];;

let partition (input: string list) (bound : string) : string list list =
  raise (Invalid_argument "Not implemented")
;;

let buildCNF (input : string list) : (string * string) list list = 
  raise (Invalid_argument "Not implemented")
;;

let getVariables (input : string list) : string list = 
  raise (Invalid_argument "Not implemented")
;;

let rec generateDefaultAssignments (varList : string list) : (string * bool) list = 
  raise (Invalid_argument "Not implemented")
;;

let rec generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool = 
  raise (Invalid_argument "Not implemented")
;;

let rec lookupVar (assignList : (string * bool) list) (str : string) : bool = 
  raise (Invalid_argument "Not implemented")
;;

let evaluateCNF (t : (string * string) list list) (assignList : (string * bool) list) : bool = 
  raise (Invalid_argument "Not implemented")
;;

let satisfy (input : string list) : (string * bool) list =
  raise (Invalid_argument "Not implemented")
;;