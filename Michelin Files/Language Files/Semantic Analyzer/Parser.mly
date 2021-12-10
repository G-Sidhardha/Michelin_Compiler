%{  
	open Ast
	open Semantic
	let parse_error err = (* Called by parser on error. *)
	print_endline err;
	flush stdout
	let scope = ref 1;;	
	let class_dum = ref 0;;
	let class_num = ref 1;;
	let func_num = ref 1;;
%}

%token <string> COLON LPARAN RPARAN LBRAC RBRAC LCURLY RCURLY COMMA PERIOD SEMICOLON
%token <string> PLUS PLUSASSIGN MINUS MINUSASSIGN MULT MULTASSIGN DIV DIVASSIGN MOD MODASSIGN POWER POWERASSIGN 
%token <string> ASSIGN EQUAL NEQUAL LT LTE GT GTE DQOUTES DEF
%token <string> IF ELSE FOR WHILE SWITCH CASE DEFAULT NEW RETURN READY SERVE
%token <string> MICROWAVE BAKE CONTAINER STOVE VESSEL CAULDRON BOWL NONSTICKPAN PAN GLASS SAUCEPAN PRESSURECOOKER SKILLET WOK INGREDIENT TOOLS LID PLATE FOODPROCESSOR
%token <string> STIRRER SPOON KNIFE PEELER TABLESPOON TEASPOON GRATER WHISK
%token <string> INT DOUBLE STRING BOOL CLASS INHERITS 
%token <string> ACCESS NEWLINE PRIVATE PUBLIC PROTECTED RECIPE
%token <string> AND OR NOT TRUE FALSE
%token <string> INT BOOLEAN STRING DOUBLE UNIT MASS VOLUME 
%token <string*string> BOOL_LIT
%token <string*string> STR_LIT
%token <string*string> IDENTIFIER
%token <string*string> INT_CONST
%token <string*string> DEC_CONST
%token EOF

// Operator precedence stated in reverse order. 
%left COMMA
%nonassoc IF
%nonassoc ELSE
%right ASSIGN PLUSASSIGN MINUSASSIGN MULTASSIGN DIVASSIGN MODASSIGN POWERASSIGN
%left OR
%left AND
%left EQUAL NEQUAL
%left LT LTE GT GTE
%left PLUS MINUS
%left MULT DIV MOD
%right NEW
%left ACCESS
%nonassoc LBRAC RBRAC
%nonassoc LPARAN RPARAN

%start entry_list
%type <Ast.program>  entry_list

// Grammar rules. 

%%

entry_list: 
        	entry entry_list      {$1::$2}
			|EOF					{[]}
        	;
  
entry:	m_class PERIOD	{ incr class_num;(*pr ($1 : class_info);*) Classes($1)} 
		|func_decl		{incr func_num; Func($1)}
		;

m_class:
		CLASS IDENTIFIER LCURLY body_list RCURLY			
		{NoInherits({
			cname = $2;
			cbody = $4;
		})}
		
		|CLASS IDENTIFIER INHERITS access_type IDENTIFIER LCURLY body_list RCURLY		
		{Inherits({
			cname = $2;
			cbody = $7;
			access = $4;
			iname = $5;
		})}
		;

access_type: PUBLIC		{$1, "public"}
	     |PRIVATE	   	{$1, "private"}
	     |PROTECTED    	{$1, "protected"}
	     ;
		
body_list:
	body 				{ [$1] }
	|body body_list		{$1::$2}
	;

body:
	attr_decl PERIOD    {Attr($1)}
	|func_decl		    {Func($1)}
	;

cust_datatypes: IDENTIFIER	    {UserDT($1)}
				|inb_datatypes	{Inb($1)}

lang_datatypes: datatypes			{D($1)}
				|inb_datatypes		{Ib($1)}

attr_decl:  
		datatypes IDENTIFIER ASSIGN expr	
		{	
			let x = match $1 with
				IntType		-> 	"int"
				|BoolType	-> 	"bool"
				|DoubleType	->  "float"
				|StringType	-> 	"string"
			in
			Semantic.insert_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "var" (string_of_int (!scope)) (fst $2);
			if x <> (check_fun $4) then (Printf.printf "Type of variable %s at line %s does not match type of expression\n" (snd $2) (fst $2); exit 0);
			Attr_def({vdname = $2;	vdtype = $1; vdasn = $4;})
		}
		|datatypes IDENTIFIER			
		{
			let x = match $1 with
				IntType		-> 	"int"
				|BoolType	-> 	"bool"
				|DoubleType	->  "float"
				|StringType	-> 	"string"
			in
			Semantic.insert_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "var" (string_of_int (!scope)) (fst $2);
			Attr_decl({vname = $2;vtype = $1;})}
		
		|cust_datatypes IDENTIFIER ASSIGN NEW cust_datatypes LPARAN expr_list RPARAN
		{
			check_obj_types $1 $5 (fst $2) (snd $2);
			insert_obj (snd $2) $1 (string_of_int !func_num) (string_of_int !class_dum) "object" (string_of_int (!scope)) (fst $2);
			Obj_def({
				tname_l = $1;
				oname = $2;
				tname_r = $5;
				par_list = $7;
			})
		}
		|datatypes IDENTIFIER LBRAC INT_CONST RBRAC 
		{
			let x = match $1 with
				IntType		-> 	"int"
				|BoolType	-> 	"bool"
				|DoubleType	->  "float"
				|StringType	-> 	"string"
			in
			insert_arr_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "arr" (string_of_int (!scope)) (fst $2) (snd $4);
			Arr_def({
				arr_name = $2;
				arr_type = $1;
				arr_size = (snd $4);
			})
		}
		/*
			vessel x[5] <
				x[1] = new bowl().
				x[2] = new glass().
			>

			
		*/
		| cust_datatypes IDENTIFIER LBRAC INT_CONST RBRAC LT init_list GT 
		{
			let x = give_cust_datatype $1 in
			insert_arr_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "arr" (string_of_int (!scope)) (fst $2) (snd $4);
			Obj_arr_def({
				arr_type = $1;
				arr_name = $2;
				arr_size = (snd $4);
				obj_init_list = $7;
			})
		}
		;

init_list :    		 {[]}
	|init init_list  {$1::$2}

init : IDENTIFIER LBRAC expr RBRAC ASSIGN NEW cust_datatypes LPARAN expr_list RPARAN PERIOD 
	{ 
		{
			init_name = $1;
			init_position = $3;
			init_dtype = $7;
			init_arg_list = $9;
		}
	}

func_decl:
		datatypes IDENTIFIER LPARAN arg_list RPARAN LCURLY statement_list RCURLY	
			{	
				let x = match $1 with
				IntType		-> 	"int"
				|BoolType	-> 	"bool"
				|DoubleType	->  "float"
				|StringType	-> 	"string"
				in
				
				(*Semantic.insert_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "function" (string_of_int !scope) (fst $2);*)
				Semantic.insert_ftable (snd $2) (fst $2) $4 x (string_of_int !class_dum);
				(*Printf.printf "Start of Function %s\nParsing the Function\n" (snd $2);*)
				
				{
					ftype = $1;
					fname = $2;
					arguments = $4;
					body = $7;
				}
			}
		;

statement:
	attr_decl PERIOD	   		        {Locals($1)}
	| 	expr PERIOD						{Expr($1)}	
	|   IDENTIFIER ASSIGN expr PERIOD	{ 
											var_type_check4 $1 $3 (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum);
											Asn($1,$3)
										}	
	|   IDENTIFIER LBRAC INT_CONST RBRAC ASSIGN expr PERIOD	{ 
											var_type_check4 $1 $6 (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum);
											Semantic.search_arr (snd $1) (fst $1) (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum) (snd $3);
											Arr_Asn($1, snd $3, $6)
										}										
	|   RETURN expr PERIOD  			{Return($2)}
	|	LCURLY statement_list RCURLY 	{Block($2)}
	|	If_Statement 					{$1}	
	|	FOR LPARAN IDENTIFIER ASSIGN expr SEMICOLON expr SEMICOLON IDENTIFIER ASSIGN expr RPARAN LCURLY statement_list RCURLY       {check_middle_exp $7 $1; For($3,$5,$7,$9,$11,$14)}
	|WHILE LPARAN expr RPARAN LCURLY statement_list RCURLY {check_middle_exp $3 $1; While($3, $6)}
	|   READY 	PERIOD								{Ready}
	| 	SERVE IDENTIFIER PERIOD						{Serve($2)}
	;

If_Statement: IF LPARAN expr RPARAN LCURLY statement_list RCURLY										{check_middle_exp $3 $1; If($3,$6,[])}	
			  |IF LPARAN expr RPARAN LCURLY statement_list RCURLY ELSE LCURLY statement_list RCURLY 	{check_middle_exp $3 $1; If($3,$6,$10)}	

statement_list: 					{[]}
		|statement statement_list	{ $1::$2 }
		;

argument:
		lang_datatypes IDENTIFIER
		{
			
			let x = match $1 with
				|D(y)  -> give_dt y
				|Ib(y) -> dummy y
			in
			Semantic.insert_vtable (snd $2) x (string_of_int !func_num) (string_of_int !class_dum) "var"  (string_of_int (!scope + 1)) (fst $2);
			
			{
				argname = $2;
				argtype = x;
			}
		}
		;

arg_list:					{ [] }
		| argument          {[$1]}
		| argument COMMA arg_list	{ $1::$3 }
		;

datatypes:  INT		{IntType}
	    |BOOL		{BoolType}
	    |STRING		{StringType}
	    |DOUBLE		{DoubleType}
	    ;


inb_datatypes:	BOWL 	   {BowlType}
				|PAN	   {PanType}
				|MICROWAVE {MicrowaveType}
				|CONTAINER {ContainerType}
				|PEELER    {PeelerType}
				|STOVE	   {StoveType}
				|VESSEL	   {VesselType}
				|CAULDRON  {CaldronType}
				|NONSTICKPAN 	{NPanType}
				|GLASS       	{GlassType}
				|SAUCEPAN 	 	{SaucepanType}
				|PRESSURECOOKER {CookerType}
				|KNIFE          {KnifeType}
				|TABLESPOON     {SpoonType}
				|SKILLET 		{SkilletType}
				|WOK 			{WokType}
				|INGREDIENT 	{IngType}
				|TOOLS 			{ToolsType}
				|LID 			{LidType}
				|PLATE          {PlateType}
				|FOODPROCESSOR  {FoodProcessorType}
				;

// expr_list_helper: 		   {[]}
// 		| COMMA expr_list  {$2}
// 		;

// expr_list:					    { [] }
// 		| expr expr_list_helper	{ $1::$2 }
//		;

expr_list:                      {[]}
	|expr 						{$1::[]}
	|expr COMMA expr_list		{$1::$3} 

expr : INT_CONST		{Integer("int",fst $1, snd $1)}
	| DEC_CONST			{Double("float", fst $1, snd $1)}
	| BOOL_LIT          {Bool("bool",fst $1, snd $1)}
    | STR_LIT           {String("string",fst $1, snd $1)} 
	| IDENTIFIER 		{ 
							(*Printf.printf "%s %s %s %s %s\n" (snd $1) (fst $1) (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum);*)
							let y = Semantic.search_vtable (snd $1) (fst $1) (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum) in
							ID((List.nth y 0), $1);
						}
	| IDENTIFIER LBRAC INT_CONST RBRAC {
									let y = Semantic.search_arr (snd $1) (fst $1) (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum) (snd $3) in
									Arr_ID(y, $1, snd $3);
								  }
	| IDENTIFIER LPARAN expr_list RPARAN  {let t = search_ftable (snd $1) (string_of_int 0) $3 (fst $1) in Call(t,$1,$3);}
	| IDENTIFIER ACCESS IDENTIFIER		  {
												let t = Semantic.search_vtable (snd $3) (fst $3) (string_of_int !scope) (string_of_int !func_num) (string_of_int !class_dum) in
												Call_atr((List.nth t 0),$1,$3)
										  }
	| IDENTIFIER ACCESS IDENTIFIER LPARAN expr_list RPARAN {let t = search_ftable (snd $3) (string_of_int 0) $5 (fst $3) in Call_meth(t, $1, $3, $5)}
	|expr PLUS expr    	{ let type_var = var_type_check $1 $3 $2 in	Binop( type_var, $1, Add, $3); }   
	|expr MINUS expr    { let type_var = var_type_check $1 $3 $2 in Binop( type_var, $1, Sub, $3); } 
	|expr MULT expr    	{ let type_var = var_type_check $1 $3 $2 in Binop( type_var, $1, Mul, $3); } 
	|expr DIV expr    	{ let type_var = var_type_check $1 $3 $2 in	Binop( type_var, $1, Div, $3); }   	
	|expr MOD expr    	{ let type_var = var_type_check $1 $3 $2 in	Binop( type_var, $1, Mod, $3); }   	
	|expr EQUAL expr    { let type_var = var_type_check2 $1 $3 $2 in Boolean( type_var, $1, Eq, $3); }   
	|expr NEQUAL expr   { let type_var = var_type_check2 $1 $3 $2 in Boolean( type_var, $1, Neq, $3); }   
	|expr LT expr    	{ let type_var = var_type_check2 $1 $3 $2 in Boolean(type_var, $1, Lt, $3);} 
	|expr GT expr    	{ let type_var = var_type_check2 $1 $3 $2 in Boolean(type_var, $1, Gt, $3);} 
	|expr LTE expr    	{ let type_var = var_type_check2 $1 $3 $2 in Boolean(type_var, $1, Ltq, $3);} 
	|expr GTE expr    	{ let type_var = var_type_check2 $1 $3 $2 in Boolean(type_var, $1, Gtq, $3);} 
	|expr AND expr   	{ let type_var = var_type_check3 $1 $3 $2 in Brela(type_var, $1, And, $3);} 
	|expr OR expr    	{ let type_var = var_type_check3 $1 $3 $2 in Brela(type_var, $1, Or, $3);} 
	|LPARAN expr RPARAN {Bracket(check_fun $2, $2)}
	;

%%

let read_tokens token_filename = 
	let fin = open_in token_filename in
	let tokens_queue = Queue.create() in
	let get_line() = String.trim (input_line fin) in
	(try while true do
		let line_number = get_line () in
		let token_type = get_line () in
		let token = match token_type with	 	
	 		"PERIOD"  -> PERIOD(line_number)
        	| "COLON"   -> COLON(line_number)
        	| "DQOUTES" -> DQOUTES(line_number)
        	| "LPARAN"  -> LPARAN(line_number)
        	| "RPARAN"  -> RPARAN(line_number)
        	| "LCURLY"  -> LCURLY(line_number)
        	| "RCURLY"  -> RCURLY(line_number)
        	| "LBRAC"   -> LBRAC(line_number)
        	| "RBRAC"   -> RBRAC(line_number)
        	| "COMMA"   -> COMMA(line_number)
			| "SEMICOLON" -> SEMICOLON(line_number)
        	| "PLUS"    -> PLUS(line_number)
        	| "PLUSASSIGN"  -> PLUSASSIGN(line_number)
        	| "MINUS"       -> MINUS(line_number)
        	| "MINUSASSIGN" -> MINUSASSIGN(line_number)
        	| "MULT"        -> MULT(line_number)
        	| "MULTASSIGN"  -> MULTASSIGN(line_number)
        	| "DIV" 	 -> DIV(line_number)
       	    | "DIVASSIGN" 	 -> DIVASSIGN(line_number)
        	| "MOD" 	 -> MOD(line_number)
        	| "MODASSIGN"   -> MODASSIGN(line_number)
        	| "POWER" 	 -> POWER(line_number)
        	| "POWERASSIGN" -> POWERASSIGN(line_number)
        	| "ACCESS" 	 -> ACCESS(line_number)
        	| "ASSIGN" 	 -> ASSIGN(line_number)
        	| "EQUAL"	 -> EQUAL(line_number)
        	| "NEQUAL" 	 -> NEQUAL(line_number)
        	| "LT" 	 -> LT(line_number)
        	| "LTE" -> LTE(line_number)
        	| "GT" -> GT(line_number)
        	| "GTE" -> GTE(line_number)
        	| "AND" -> AND(line_number)
        	| "OR" -> OR(line_number)
        	| "NOT" -> NOT(line_number)
        	| "IF" -> IF(line_number)
        	| "ELSE" -> ELSE(line_number)
        	| "WHILE" -> WHILE(line_number)
        	| "FOR" -> FOR(line_number)
        	| "SWITCH" -> SWITCH(line_number)
        	| "CASE" -> CASE(line_number)
        	| "DEFAULT" -> DEFAULT(line_number)
        	| "INT" -> INT(line_number)
        	| "DOUBLE" -> DOUBLE(line_number)
        	| "STRING" -> STRING(line_number)
        	| "BOOL" -> BOOL(line_number)
        	| "PLATE" -> PLATE(line_number)
        	| "MICROWAVE" -> MICROWAVE(line_number)
        	| "BAKE" -> BAKE(line_number)
        	| "CONTAINER" -> CONTAINER(line_number)
        	| "STOVE" -> STOVE(line_number)
        	| "VESSEL" -> VESSEL(line_number)
        	| "CAULDRON" -> CAULDRON(line_number)
        	| "BOWL" -> BOWL(line_number)
        	| "NONSTICKPAN" -> NONSTICKPAN(line_number)
        	| "PAN" -> PAN(line_number)
        	| "GLASS" -> GLASS(line_number)
        	| "SAUCEPAN" -> SAUCEPAN(line_number)
        	| "PRESSURECOOKER" -> PRESSURECOOKER(line_number)
        	| "SKILLET" -> SKILLET(line_number)
        	| "WOK" -> WOK(line_number)
        	| "INGREDIENT" -> INGREDIENT(line_number)
        	| "TOOLS" -> TOOLS(line_number)
        	| "LID" -> LID(line_number)
        	| "STIRRER" -> STIRRER(line_number)
        	| "SPOON" -> SPOON(line_number)
        	| "KNIFE" -> KNIFE(line_number)
        	| "PEELER" -> PEELER(line_number)
        	| "TABLESPOON" -> TABLESPOON(line_number)
        	| "TEASPOON" -> TEASPOON(line_number)
        	| "GRATER" -> GRATER(line_number)
        	| "WHISK" -> WHISK(line_number)
        	| "FOODPROCESSOR" -> FOODPROCESSOR(line_number)
        	| "CLASS" -> CLASS(line_number) 
			| "INHERITS" -> INHERITS(line_number)
        	| "PUBLIC" -> PUBLIC(line_number)
        	| "PRIVATE" -> PRIVATE(line_number)
        	| "PROTECTED" -> PROTECTED(line_number)
        	| "UNIT" -> UNIT(line_number)
        	| "MASS" -> MASS(line_number)
        	| "VOLUME" -> VOLUME(line_number)
        	| "NEW" -> NEW(line_number)
        	| "READY" -> READY(line_number)
        	| "RETURN" -> RETURN(line_number)
        	| "SERVE" -> SERVE(line_number)
        	| "RECIPE" -> RECIPE(line_number)
        	| "BOOL_LIT" -> BOOL_LIT(line_number, get_line())
        	| "INT_CONST" -> INT_CONST(line_number, get_line())
        	| "DEC_CONST" -> DEC_CONST(line_number, get_line())
        	| "STR_LIT" -> STR_LIT(line_number, get_line())
        	| "IDENTIFIER" -> IDENTIFIER(line_number, get_line())
			| "EOF"  -> EOF
		| _ -> begin
			Printf.printf "syntax error %s\n" token_type;
			exit 1
		end in
		Queue.add (line_number,token) tokens_queue
		done with _ -> ());
		close_in fin;
		tokens_queue

let main () = begin
	
	let token_filename = "L2_output.txt" in
	let tokens_queue = read_tokens token_filename in
	let lexbuf = Lexing.from_string "" in 
	
	let last_line = ref "1" in
	
	let lexer_token lb =
		if Queue.is_empty tokens_queue then
			EOF
		else begin
			let line_number,next_token = Queue.take tokens_queue in
			last_line := line_number ;
			
			match next_token with
			  LCURLY(a) 			->    	incr scope; next_token;
			| RCURLY(a) 			-> 		decr scope; next_token;
			| _				        -> 		next_token;
		end
	in
	Parsing.set_trace true;  begin
	let ast =
	  try 
	    entry_list lexer_token lexbuf
	  with _-> begin
	    Printf.printf "Parsing error at line %s.\n" !last_line;
	    exit 0
	  end
	in
	
	let rec output_list ast =
		List.iter output_entry ast

	and output_entry ast = 
		match ast with
		|Classes(name)	-> output_class name
		|_		-> Printf.printf "End of Function with no errors!\n";
	
	and output_class ast = 
		match ast with
		|Inherits(c) -> 
			Printf.printf "End of class which inherited class %s with no errors!\n" (snd (c.iname)) ;
		|_ -> Printf.printf "End of Class with no inheritance with no errors!\n";
	in
	
	
	output_list ast;
	(*CodeGen.output_list ast;*)
	end
	(*Printf.printf "Funcs : %d\n" (List.length ast);*)
	
end;;

main();;
