open Printf

let _ = 
  try 
    let filename = Sys.argv.(1)  in
    let in_channel = open_in filename in
    let lexbuf = Lexing.from_channel in_channel in
      while true do
        let line, result = Lexer.token lexbuf in
        match result with 
          |PERIOD       -> printf "The token . is found at line %d \n"  line;
          |COLON        -> printf "The token : is found at line %d \n"  line;
          |DQOUTES      -> printf "The token \" is found at line %d \n"  line;
          |LPARAN       -> printf "The token ( is found at line %d \n"  line;
          |RPARAN       -> printf "The token ) is found at line %d \n"  line;
          |LCURLY       -> printf "The token { is found at line %d \n"  line;
          |RCURLY       -> printf "The token } is found at line %d \n"  line;
          |LBRAC        -> printf "The token [ is found at line %d \n"  line;
          |RBRAC        -> printf "The token ] is found at line %d \n"  line;
          |COMMA        -> printf "The token , is found at line %d \n"  line;
          |PLUS         -> printf "The token + which is an operator is found at line %d \n"  line;
          |UPLUS        -> printf "The token ++ which is an operator is found at line %d \n"  line;
          |PLUSASSIGN   -> printf "The token += which is an operator is found at line %d \n"  line;
          |MINUS        -> printf "The token - which is an operator is found at line %d \n"  line;
          |UMINUS       -> printf "The token -- which is an operator is found at line %d \n"  line;
          |MINUSASSIGN  -> printf "The token -= which is an operator is found at line %d \n"  line;
          |MULT         -> printf "The token * which is an operator is found at line %d \n"  line;
          |MULTASSIGN   -> printf "The token *= which is an operator is found at line %d \n"  line;
          |DIV          -> printf "The token / which is an operator is found at line %d \n"  line;
          |DIVASSIGN    -> printf "The token /= which is an operator is found at line %d \n"  line;
          |MOD          -> printf "The token mod which is an operator is found at line %d \n"  line;
          |MODASSIGN    -> printf "The token mod= which is an operator is found at line %d \n"  line;
          |POWER        -> printf "The token ** which is an operator is found at line %d \n"  line;
          |POWERASSIGN  -> printf "The token **= which is an operator is found at line %d \n"  line;
          |ACCESS       -> printf "The token ACCESS(~>) which is an operator is found at line %d \n"  line;
          |ASSIGN       -> printf "The token = which is an operator is found at line %d \n"  line;
          |EQUAL        -> printf "The token ==  which is an operator is found at line %d \n"  line;
          |NEQUAL       -> printf "The token != which is an operator is found at line %d \n"  line;
          |LT           -> printf "The token < which is an operator is found at line %d \n"  line;
          |LTE          -> printf "The token <= which is an operator is found at line %d \n"  line;
          |GT           -> printf "The token > which is an operator is found at line %d \n"  line;
          |GTE          -> printf "The token GTE >= which is an operator is found at line %d \n"  line;
          |AND          -> printf "The token and which is an operator is found at line %d \n"  line;
          |OR           -> printf "The token or which is an operator is found at line %d \n"  line;
          |NOT          -> printf "The token not which is an operator is found at line %d \n"  line;
          |IF           -> printf "The token if which is a reserved keyword is found at line %d \n"  line;
          |ELSE         -> printf "The token else which is a reserved keyword is found at line %d \n"  line;
          |WHILE        -> printf "The token while which is a reserved keyword is found at line %d \n"  line;
          |FOR          -> printf "The token for which is a reserved keyword is found at line %d \n"  line;
          |SWITCH       -> printf "The token switch which is a reserved keyword is found at line %d \n"  line;
          |CASE         -> printf "The token case which is a reserved keyword is found at line %d \n"  line;
          |DEFAULT      -> printf "The token default which is a reserved keyword is found at line %d \n"  line;
          |INT          -> printf "The token int which is a reserved keyword is found at line %d \n"  line;
          |DOUBLE       -> printf "The token double which is a reserved keyword is found at line %d \n"  line;
          |STRING       -> printf "The token string which is a reserved keyword is found at line %d \n"  line;
          |BOOL         -> printf "The token bool which is a reserved keyword is found at line %d \n"  line;
          |MICROWAVE    -> printf "The token Microwave which is a reserved keyword is found at line %d \n"  line;
          |BAKE         -> printf "The token Bake which is a reserved keyword is found at line %d \n"  line;
          |CONTAINER    -> printf "The token Container which is a reserved keyword is found at line %d \n"  line;
          |STOVE        -> printf "The token Stove which is a reserved keyword is found at line %d \n"  line;
          |VESSEL       -> printf "The token Vessel which is a reserved keyword is found at line %d \n"  line;
          |CAULDRON     -> printf "The token Cauldron which is a reserved keyword is found at line %d \n"  line;
          |BOWL         -> printf "The token Bowl which is a reserved keyword is found at line %d \n"  line;
          |NONSTICKPAN  -> printf "The token NonStickPan which is a reserved keyword is found at line %d \n"  line;
          |PAN          -> printf "The token Pan which is a reserved keyword is found at line %d \n"  line;
          |GLASS        -> printf "The token Glass which is a reserved keyword is found at line %d \n"  line;
          |SAUCEPAN     -> printf "The token SaucePan which is a reserved keyword is found at line %d \n"  line;
          |PRESSURECOOKER -> printf "The token PressureCooker which is a reserved keyword is found at line %d \n"  line;
          |SKILLET      -> printf "The token Skillet which is a reserved keyword is found at line %d \n"  line;
          |WOK          -> printf "The token Wok which is a reserved keyword is found at line %d \n"  line;
          |INGREDIENT   -> printf "The token Ingredient which is a reserved keyword is found at line %d \n"  line;
          |TOOLS        -> printf "The token Tools which is a reserved keyword is found at line %d \n"  line;
          |LID          -> printf "The token Lid which is a reserved keyword is found at line %d \n"  line;
          |STIRRER      -> printf "The token Stirrer which is a reserved keyword is found at line %d \n"  line;
          |SPOON        -> printf "The token Spoon which is a reserved keyword is found at line %d \n"  line;
          |KNIFE        -> printf "The token Knife which is a reserved keyword is found at line %d \n"  line;
          |PEELER       -> printf "The token Peeler which is a reserved keyword is found at line %d \n"  line;
          |TABLESPOON   -> printf "The token Tablespoon which is a reserved keyword is found at line %d \n"  line;
          |TEASPOON     -> printf "The token Teaspoon which is a reserved keyword is found at line %d \n"  line;
          |GRATER       -> printf "The token Grater which is a reserved keyword is found at line %d \n"  line;
          |WHISK        -> printf "The token Whisk which is a reserved keyword is found at line %d \n"  line;
          |FOODPROCESSOR -> printf "The token FoodProcessor which is a reserved keyword is found at line %d \n"  line;
          |CLASS        -> printf "The token class which is a reserved keyword is found at line %d \n"  line;
          |PUBLIC       -> printf "The token public which is a reserved keyword is found at line %d \n"  line;
          |PRIVATE      -> printf "The token private which is a reserved keyword is found at line %d \n"  line;
          |PROTECTED    -> printf "The token protected which is a reserved keyword is found at line %d \n"  line;
          |UNIT         -> printf "The token unit which is a reserved keyword is found at line %d \n"  line;
          |MASS         -> printf "The token mass which is a reserved keyword is found at line %d \n"  line;
          |VOLUME       -> printf "The token volume which is a reserved keyword is found at line %d \n"  line;
          |NEW          -> printf "The token new which is a reserved keyword is found at line %d \n"  line;
          |READY        -> printf "The token ready which is a reserved keyword is found at line %d \n"  line;
          |RETURN       -> printf "The token return which is a reserved keyword is found at line %d\n"  line;
          |RECIPE       -> printf "The token recipe which is a reserved keyword is found at line %d\n"  line;
          |BOOL_LIT(i)  -> printf "The token %b which is a BOOLEAN_CONSTANT is found at line %d \n" i line;
          |INT_CONST(i) -> printf "The token %d which is an INTEGER_CONSTANT is found at line %d \n" i line;
          |DEC_CONST(i) -> printf "The token %f which is a DECIMAL_CONSTANT is found at line %d \n" i line;
          |STR_LIT(i)   -> printf "The token %s which is a STRING_LITERAL is found at line %d \n" i line;
          |IDENTIFIER(i)-> printf "The token %s which is an IDENTIFIER is found at line %d \n" i line;
      done
  with Lexer.Eof -> exit 0
