open Ast
(*let func_name_table = Hashtbl.create 0;;*)
let var_name_table: (string list, string list) Hashtbl.t = Hashtbl.create 0;;
let func_name_table: (string list, (string list)*(arg_decl list)) Hashtbl.t = Hashtbl.create 0;;
(*let class_name_table = Hashtbl.create 0;;*)

(* This recursive function searches in the variable symbol table whether a variable has been declared in the current or in the ancestral scopes. *)
let rec find var_name var_line_no var_level1 func_num1 class_num1 = 
    if !var_level1 = 0 then (Printf.printf "Variable '%s' used at line %s has not been declared.\n" var_name var_line_no; exit 0;);
    try 
        Hashtbl.find var_name_table [var_name; string_of_int !var_level1; string_of_int !func_num1; string_of_int !class_num1]
    with Not_found -> begin     
        decr var_level1; 
        find var_name var_line_no var_level1 func_num1 class_num1;
    end
;;

(* For a given function/class, var_name_table stores the variables corresponding to that specific body. We search for the variables from the table via this function. *)
let search_vtable var_name var_line_no var_level func_num class_num = 
        (* 
           O(deepest scope) search to find the closest binding. Might be able to optimize it to 
           O(log (deepest scope)) using a divide and conquer approach but it might use about 4 times the memory the current table is using. 
        *)
    (*Printf.printf "Is it var? %s\n" var_name;*)
    let scope1 = ref (int_of_string var_level) in 
    let func_num1 = ref (int_of_string func_num) in
    let class_num1 = ref (int_of_string class_num) in   
    find var_name var_line_no scope1 func_num1 class_num1
;;

let insert_vtable var_name var_type func_num class_num var_tag var_level var_line_no = (* For a given function/class, var_name_table stores the variables corresponding to that specific body. We insert the variables 
                                                                    into that table via this function. *)
    try
        let result = Hashtbl.find var_name_table [var_name; var_level; func_num; class_num] in
        Printf.printf "Variable '%s' at line %s was already declared at %s.\n" var_name var_line_no (List.nth result 2);
        exit 0
    with Not_found-> begin 
        Hashtbl.add var_name_table [var_name; var_level; func_num; class_num] [var_type; var_tag; var_line_no];
		(*Hashtbl.iter (fun x y -> Printf.printf "%s %s %s %s -> %s %s %s\n" (List.nth x 0) (List.nth x 1) (List.nth x 2) (List.nth x 3) (List.nth y 0) (List.nth y 1) (List.nth y 2)) var_name_table;*)
    end
;;


let check_fun exp1 = (* Returns the type of the expression. *)
    match exp1 with
        Integer(x,_,_) 		-> x
        |Double(x,_,_) 		-> x
        |String(x,_,_) 		-> x
        |Bool(x,_,_) 		-> x
        |Binop(x,_,_,_) 	-> x
        |Boolean(x,_,_,_) 	-> x
        |Brela(x,_,_,_)	    -> x
        |Bracket(x,_) 	    -> x
        |ID(x,_)            -> x
        |Call(x,_,_)        -> x
        |Call_meth(x, _, _, _) -> x
        |Arr_ID(x, _, _)    -> x
        |Call_atr(x,_,_)    -> x
        |_ 					-> "in";
;;

let var_type_check e1 e2 l_no = (* Type checking for data types that support MDAS operations.*)
    let i1 = check_fun e1 in
    let i2 = check_fun e2 in
    (*Printf.printf "%s %s\n" i1 i2; *)
    if i1= "string" || i1 = "bool" then (Printf.printf "Operation at line %s is not compatible for type %s.\n" l_no i1; exit 0);
    if i2= "string" || i2 = "bool" then (Printf.printf "Operation at line %s is not compatible for type %s.\n" l_no i2; exit 0);
    if i1 = "float" || i2 = "float" then "float" else "int";
;;

let var_type_check2 e1 e2 l_no = (* For relational operators; doesn't allow string comparison though (we don't see the need for this in our language at the moment).*)
    let i1 = check_fun e1 in
    let i2 = check_fun e2 in
    (* Printf.printf "%s %s\n" i1 i2; *)
    if i1 = "string" then (Printf.printf "Operation at line %s is not compatible for type %s.\n" l_no i1; exit 0);
    if i2 = "string" then (Printf.printf "Operation at line %s is not compatible for type %s.\n" l_no i2; exit 0);
    if i1 = i2 then  "bool" else (Printf.printf "Type %s, %s can't be compared at %s.\n" i1 i2 l_no; exit 0);
;;

let var_type_check3 e1 e2 l_no = (* Handles AND, OR operations on Boolean datatypes.*)
    let i1 = check_fun e1 in
    let i2 = check_fun e2 in
    (* Printf.printf "%s %s\n" i1 i2; *)
    if i1 = "bool" && i2 = "bool" then "bool" else (Printf.printf "Boolean expression expected at line number %s.\n" l_no; exit 0);
;;

let var_type_check4 id e scope func_num class_num =  (* Handles assignment statements.*)
    let y = search_vtable (snd id) (fst id) scope func_num class_num in
    let i1 = (List.nth y 0) in
    let i2 = check_fun e in
    if i1 <> i2 then (Printf.printf "Line: %s -> Types of %s(%s) and expression(%s) not matching.\n" (fst id) (snd id) i1 i2; exit 0);
;;

let check_middle_exp expression l_no = 
    let i = check_fun expression in
    if i <> "bool" then (Printf.printf "Line: %s -> Boolean expression expected.\n" l_no; exit 0);
;; 

let insert_ftable f_name f_line_no f_param_list f_return_type class_num = 
    try
        let result = Hashtbl.find func_name_table [f_name; class_num] in
        Printf.printf "Error at line %s: Function '%s' was already declared at %s.\n" f_line_no f_name (List.nth (fst result) 1);
        exit 0
    with _ -> begin 
        Hashtbl.add func_name_table [f_name; class_num] ([f_return_type; f_line_no], f_param_list);
    end
;;

let rec loop i arg_expr_list par_list l_no l =  
    let t1 = check_fun (List.nth arg_expr_list i) in
    let t2 = (List.nth par_list i).argtype in
    if  t1 <> t2  then (
        Printf.printf "Error at line %s: argument-%d was of type %s but was expected to be type %s\n" l_no i t1 t2; 
        exit 0 
    )
    else (if i < l-1 then (loop (i+1) arg_expr_list par_list l_no l))
;;


let search_ftable f_name class_num arg_expr_list l_no = 
    try
        let result = Hashtbl.find func_name_table [f_name; class_num] in
        let par_list = snd result in

        let l1 = List.length arg_expr_list in
        let l2 = List.length par_list in
        if l1 < l2 then (Printf.printf "Error at line %s: too few arguments for the function %s.\n" l_no f_name; exit 0);
        if l1 > l2 then (Printf.printf "Error at line %s: too many arguments for the function %s.\n" l_no f_name; exit 0);
        
        loop 0 arg_expr_list par_list l_no l1;
        List.nth (fst result) 0;

    with Not_found -> begin
        Printf.printf "Error at line %s: Function '%s' was not defined.\n" l_no f_name ;
        exit 0
    end
;;

let insert_arr_vtable var_name var_type func_num class_num var_tag var_level var_line_no var_size = (* For a given function/class, var_name_table stores the variables corresponding to that specific body. We insert the variables 
                                                                    into that table via this function. *)
    try
        let result = Hashtbl.find var_name_table [var_name; var_level; func_num; class_num] in
        Printf.printf "Variable '%s' at line %s was already declared at %s.\n" var_name var_line_no (List.nth result 2);
        exit 0
    with Not_found-> begin 
        Hashtbl.add var_name_table [var_name; var_level; func_num; class_num] [var_type; var_tag; var_line_no; var_size];
		(*Hashtbl.iter (fun x y -> Printf.printf "%s %s %s %s -> %s %s %s\n" (List.nth x 0) (List.nth x 1) (List.nth x 2) (List.nth x 3) (List.nth y 0) (List.nth y 1) (List.nth y 2)) var_name_table;*)
    end
;;

let search_arr arr_name arr_line_no arr_level func_num class_num arr_pos = 
    if (int_of_string arr_pos) < 0 then (Printf.printf "Error at line %s: Array index can't be negative\n" arr_line_no; exit 0;);
    let scope1 = ref (int_of_string arr_level) in 
    let func_num1 = ref (int_of_string func_num) in
    let class_num1 = ref (int_of_string class_num) in   
    let out_val = find arr_name arr_line_no scope1 func_num1 class_num1 in
    if int_of_string (List.nth out_val 3) < int_of_string arr_pos then( Printf.printf "Error at line %s: Array index out of size\n" arr_line_no; exit 0;)
    else ((List.nth out_val 0);)
;;

let dummy t = 
    match t with 
        BowlType    -> "Bowl"
        |PanType    -> "Pan"
        |MicrowaveType -> "Microwave"
        |ContainerType -> "Container"
        |StoveType  -> "Stove"
        |VesselType -> "Vessel"
        |CaldronType-> "Caldron"
        |NPanType   -> "NPan"
        |GlassType  -> "Glass"
        |SaucepanType -> "Saucepan"
        |CookerType -> "Cooker"
        |SkilletType-> "Skillet"
        |WokType    -> "Wok"
        |PeelerType -> "Peeler"
        |IngType    -> "Ing"
        |ToolsType  -> "Tools"
        |LidType    -> "Lid"
        |PlateType  -> "Plate"
        |KnifeType  -> "Knife"
        |SpoonType  -> "Spoon"
        |FoodProcessorType -> "FP"
;;

(*helper*)
let give_cust_datatype x = 
    match x with
        Inb(t) -> dummy t;
        |_      -> "Custom";
;;

(*checks types of cust_datatypes*)
let check_obj_types t1 t2 l_no var_name = 
    let type1 = give_cust_datatype t1 in
    let type2 = give_cust_datatype t2 in
    (*
        errors:
            Plate x = new Tools()

    *)
    if type1 <> "Vessel" && (type1 <> "Custom" && type2 <> "Custom") && type1 <> type2
    then (Printf.printf "Error at %s: Types not matching for %s\n" l_no var_name; exit 0)
;;

(* insert a object in symbol table *)
let insert_obj obj_name obj_type func_num class_num obj_tag obj_level obj_line_no = 
    try
        let result = Hashtbl.find var_name_table [obj_name; obj_level; func_num; class_num] in
        Printf.printf "Variable '%s' at line %s was already declared at %s.\n" obj_name obj_line_no (List.nth result 2);
        exit 0
    with Not_found-> begin 
        Hashtbl.add var_name_table [obj_name; obj_level; func_num; class_num] [give_cust_datatype obj_type; obj_tag; obj_line_no];
    end
;;

let give_dt y = 
    match y with
							IntType		-> 	"int"
							|BoolType	-> 	"bool"
							|DoubleType	->  "float"
							|StringType	-> 	"string"
;;
