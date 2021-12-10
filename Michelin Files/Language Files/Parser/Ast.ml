type operator = Add | Sub | Mul | Div | Mod
	type rop = Eq | Neq | Lt | Ltq | Gt | Gtq 
	type re = And | Or
	type datatypes = IntType | BoolType | StringType | DoubleType
	type identifier = string*string

	type inb_datatypes =
			BowlType
			|PanType
			|MicrowaveType
			|ContainerType
			|StoveType
			|VesselType
			|CaldronType
			|NPanType
			|GlassType
			|PeelerType
			|SpoonType
			|SaucepanType
			|CookerType
			|SkilletType
			|WokType
			|IngType
			|ToolsType
			|LidType
			|KnifeType
			|PlateType
			|FoodProcessorType

	type cust_datatypes = UserDT of identifier | Inb of inb_datatypes
	type lang_datatypes = D of datatypes | Ib of inb_datatypes

	type expr =
		Binop of string * expr * operator * expr
		| Brela of string*expr * re * expr
		| Integer of string*string*string
		| String of string*string*string
		| Bool of string*string*string
		| Boolean of string*expr * rop * expr
		| Double of string*string*string
		| ID of string*identifier
		| Arr_ID of string * identifier * string
		| Call of string*identifier * expr list
		| Call_meth of string*identifier * identifier * expr list
		| Call_atr of string*identifier * identifier
		| Bracket of string* expr
		| Null 
		| Noexpr
	
    type variable_decl = {
  		vname : identifier;
  		vtype : datatypes;
	}
	
	type variable_def = {
  		vdname : identifier;
  		vdtype : datatypes;
			vdasn  : expr;
	}
	
	type object_def = {
		tname_l : cust_datatypes;
		oname : identifier;
		tname_r : cust_datatypes;
		par_list : expr list;
	}

	type array_def = {
		arr_name : identifier;
		arr_type : datatypes;
		arr_size : string;
	}

	type init_decl = {
		init_name : identifier;
		init_position : expr;
		init_dtype : cust_datatypes;
		init_arg_list : expr list;
	}

	type obj_array = {
		arr_type : cust_datatypes;
		arr_name : identifier;
		arr_size : string;
		obj_init_list : init_decl list;
	}

	type variable = 
		Attr_decl of variable_decl
		|Attr_def of variable_def
		|Obj_def of object_def
		|Arr_def of array_def
		|Obj_arr_def of obj_array

	type statement = 
		Locals of variable
		|Expr of expr
		|Asn of identifier * expr
		|Arr_Asn of identifier * string * expr
		|Block of statement list
		|Return of expr
		|If of expr * statement list* statement list 
		|For of identifier * expr * expr * identifier * expr * statement list
		|While of expr * statement list
		|Serve of identifier
		|Ready
	
	type arg_decl = {
  		argname : identifier;
  		argtype : string;
	}
	
  type func = {
		ftype : datatypes;
		fname : identifier;
		arguments : arg_decl list;
		body : statement list;
	}

	(* Defines structure of a class (consists of a sequence of either variable declarations or
	function (i.e. method) declarations.*)
	type class_unit =
		Attr of variable 
		|Func of func

	(* A series of class declarations.*)
	type class_decl = class_unit list

	(* Stores the information related to classes that don't inherit from another class.*)
	type class_noI_info = {
		cname : identifier;
		cbody : class_decl;
	}
	
	(* Stores the information related to classes that inherit from another class.*)
	type class_I_info = {
		cname : identifier;
		cbody : class_decl;
		access : string*string;
		iname : identifier;
	}
	

	type class_info =
			Inherits of class_I_info
			|NoInherits of class_noI_info
	
	type entry =
			Classes of class_info
			|Func of func

	type program = entry list

