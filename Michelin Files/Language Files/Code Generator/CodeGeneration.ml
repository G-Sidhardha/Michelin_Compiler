module lm = Llvm
module ast = Ast


(* Infer types from surrounding context. *)
  let int64_t     = lm.i32_type    context (*Mapping ints; in our langugae 'ints' are actually long long ints.*)
  let int8_t      = lm.i8_type     context (*Mapping chars.*)
  and int1_t      = lm.i1_type     context (*Mapping booleans.*)
  and double_t    = lm.double_type context (*Mapping doubles.*)

(* Return the LLVM type for a datatype (as defined in Ast.ml) from our own language.*)
let llvm_type = function
| ast.StringType -> lm.pointer_type (lm.vector_type int8_t 512)
| ast.IntType  -> int64_t
| ast.BoolType -> int1_t
| ast.DoubleType -> double_t


let rec expr builder e = match e with
  Binop (e1, op, e2) ->
    let e1' = expr builder e1 and
    e2' = expr builder e2 in
    let type_of_e1 = lm.classify_type (lm.type_of e1') and
    type_of_e2 = lm.classify_type (lm.type_of e2') in
    if type_of_e1 = lm.TypeKind.Double && type_of_e2 = lm.TypeKind.Double then
    (match op with
      ast.Add     -> lm.build_fadd e1' e2' "tmp" builder
    | ast.Sub     -> lm.build_fsub e1' e2' "tmp" builder
    | ast.Mul    -> lm.build_fmul e1' e2' "tmp" builder
    | ast.Div     -> lm.build_fdiv e1' e2' "tmp" builder
    | ast.Eq   -> lm.build_fcmp lm.Fcmp.Oeq e1' e2' "tmp" builder
    | ast.Neq     -> lm.build_fcmp lm.Fcmp.One e1' e2' "tmp" builder
    | ast.Lt    -> lm.build_fcmp lm.Fcmp.Olt e1' e2' "tmp" builder
    | ast.Ltq     -> lm.build_fcmp lm.Fcmp.Ole e1' e2' "tmp" builder
    | ast.Gt -> lm.build_fcmp lm.Fcmp.Ogt e1' e2' "tmp" builder
    | ast.Gtq     -> lm.build_fcmp lm.Fcmp.Oge e1' e2' "tmp" builder
    | _ -> raise (Failure "internal error: semant should have rejected on doubles")
    )  else if type_of_e1 = lm.TypeKind.Double && type_of_e2 = lm.TypeKind.Integer then
    (match op with
      ast.Add     -> lm.build_fadd e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Sub     -> lm.build_fsub e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Mul    -> lm.build_fmul e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Div     -> lm.build_fdiv e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Eq   -> lm.build_fcmp lm.Fcmp.Oeq e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Neq     -> lm.build_fcmp lm.Fcmp.One e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Lt    -> lm.build_fcmp lm.Fcmp.Olt e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Ltq     -> lm.build_fcmp lm.Fcmp.Ole e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Gt -> lm.build_fcmp lm.Fcmp.Ogt e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | ast.Gtq     -> lm.build_fcmp lm.Fcmp.Oge e1' (lm.build_uitofp e2' double_t "tmp" builder) "tmp" builder
    | _ -> raise (Failure "internal error: semant should have rejected")
    ) else if type_of_e1 = lm.TypeKind.Integer && type_of_e2 = lm.TypeKind.Double then
    (match op with
      ast.Add     -> lm.build_fadd (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Sub     -> lm.build_fsub (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Mul    -> lm.build_fmul (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Div     -> lm.build_fdiv (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Equal   -> lm.build_fcmp lm.Fcmp.Oeq (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Neq     -> lm.build_fcmp lm.Fcmp.One (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Lt    -> lm.build_fcmp lm.Fcmp.Olt (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Ltq     -> lm.build_fcmp lm.Fcmp.Ole (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Gt -> lm.build_fcmp lm.Fcmp.Ogt (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | ast.Gtq     -> lm.build_fcmp lm.Fcmp.Oge (lm.build_uitofp e1' double_t "tmp" builder) e2' "tmp" builder
    | _ -> raise (Failure "internal error: semant should have rejected")
    ) else if type_of_e1 = lm.TypeKind.Integer && type_of_e2 = lm.TypeKind.Integer then
    (match op with
      ast.Add     -> lm.build_add e1' e2' "tmp" builder
    | ast.Sub     -> lm.build_sub e1' e2' "tmp" builder
    | ast.Mul    -> lm.build_mul e1' e2' "tmp" builder
    | ast.Div     -> lm.build_sdiv e1' e2' "tmp" builder
    | ast.Eq   -> lm.build_icmp lm.Icmp.Eq e1' e2' "tmp" builder
    | ast.Neq     -> lm.build_icmp lm.Icmp.Ne e1' e2' "tmp" builder
    | ast.Lt    -> lm.build_icmp lm.Icmp.Slt e1' e2' "tmp" builder
    | ast.Ltq     -> lm.build_icmp lm.Icmp.Sle e1' e2' "tmp" builder
    | ast.Gt -> lm.build_icmp lm.Icmp.Sgt e1' e2' "tmp" builder
    | ast.Gtq     -> lm.build_icmp lm.Icmp.Sge e1' e2' "tmp" builder
    | ast.And     -> lm.build_and e1' e2' "tmp" builder
    | ast.Or      -> lm.build_or e1' e2' "tmp" builder
    | ast.Mod     -> lm.build_srem e1' e2' "tmp" builder
    | _ -> raise (Failure "internal error: semant should have rejected")
    )

    let t3 decl = 
      match decl with
      |Attr_decl(p) -> builder p.vdasn
      |_ -> Printf.printf "Other";
  
    let t2 st = 
      match st with
      |Expr(x) -> builder x
      |Locals(y) -> t3 y
      |_ -> Printf.printf "Other";

  let rec output_list ast =
    List.iter output_entry ast

  and output_entry ast = 
    match ast with
    |Func(x)-> t1 x.body
    |_ -> Printf.printf "Other";
    
  and t1 st_list = 
    List.iter t2 st_list 
  in
