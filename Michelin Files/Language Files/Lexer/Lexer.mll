{
    open Printf
    let  _number = ref 1
    type token =
        |PERIOD
        |COLON
        |DQOUTES
        |LPARAN
        |RPARAN
        |LCURLY
        |RCURLY
        |LBRAC
        |RBRAC
        |COMMA
        |SEMICOLON
        |PLUS
        |PLUSASSIGN
        |MINUS
        |MINUSASSIGN
        |MULT
        |MULTASSIGN
        |DIV
        |DIVASSIGN
        |MOD
        |MODASSIGN
        |POWER
        |POWERASSIGN
        |ACCESS
        |ASSIGN
        |EQUAL
        |NEQUAL
        |LT
        |LTE
        |GT
        |GTE
        |AND
        |OR
        |NOT
        |IF
        |ELSE
        |WHILE
        |FOR
        |SWITCH
        |CASE
        |DEFAULT
        |INT
        |DOUBLE
        |STRING
        |BOOL
        |PLATE
        |MICROWAVE
        |BAKE
        |CONTAINER
        |STOVE
        |VESSEL
        |CAULDRON
        |BOWL
        |NONSTICKPAN
        |PAN
        |GLASS
        |SAUCEPAN
        |PRESSURECOOKER
        |SKILLET
        |WOK
        |INGREDIENT
        |TOOLS
        |LID
        |STIRRER
        |SPOON
        |KNIFE
        |PEELER
        |TABLESPOON
        |TEASPOON
        |GRATER
        |WHISK
        |FOODPROCESSOR
        |CLASS
        |INHERITS
        |PUBLIC
        |PRIVATE
        |PROTECTED
        |UNIT
        |MASS
        |VOLUME
        |NEW
        |READY
        |RETURN
        |SERVE
        |RECIPE
        |BOOL_LIT of bool
        |INT_CONST of int
        |DEC_CONST of float
        |STR_LIT of string
        |IDENTIFIER of string
    exception Eof
}

(* Aliases for building the token-matching rules.*)
let digit = ['0'-'9']
let character = ['A'-'Z' 'a'-'z' '_']
let integer_cons = '-'?(digit)+
let decimal_const = '-'?(digit)+ '.'(digit)+
let string_lit = '"' [^'"' '\n']* '"'

(* Token-matching rules.*)
rule token = parse 
    [' ' '\t']                  {token lexbuf}
    |'\n'                       {incr  _number; token lexbuf}
    |':'                        {! _number, COLON}
    |'.'                        {! _number, PERIOD} 
    |'"'                        {! _number, DQOUTES}
    |'('                        {! _number, LPARAN}
    |')'                        {! _number, RPARAN}
    |'{'                        {! _number, LCURLY}
    |'}'                        {! _number, RCURLY}
    |'['                        {! _number, LBRAC}
    |']'                        {! _number, RBRAC}
    |','                        {! _number, COMMA}
    |';'                        {! _number, SEMICOLON}
    |'+'                        {! _number, PLUS}
    |"+="                       {! _number, PLUSASSIGN}
    |'-'                        {! _number, MINUS}
    |"-="                       {! _number, MINUSASSIGN}
    |'*'                        {! _number, MULT}
    |"*="                       {! _number, MULTASSIGN}
    |'/'                        {! _number, DIV}
    |"/="                       {! _number, DIVASSIGN}
    |'%'                        {! _number, MOD}
    |"%="                       {! _number, MODASSIGN}
    |"**"                       {! _number, POWER}
    |"**="                      {! _number, POWERASSIGN}
    |"~>"                       {! _number, ACCESS}
    |'='                        {! _number, ASSIGN}
    |"=="                       {! _number, EQUAL}
    |"!="                       {! _number, NEQUAL}
    |'<'                        {! _number, LT}
    |"<="                       {! _number, LTE}
    |">"                        {! _number, GT}
    |">="                       {! _number, GTE}
    |"and"                      {! _number, AND}
    |"or"                       {! _number, OR}
    |"not"                      {! _number, NOT}
    |"if"                       {! _number, IF}
    |"else"                     {! _number, ELSE}
    |"while"                    {! _number, WHILE}
    |"for"                      {! _number, FOR}
    |"switch"                   {! _number, SWITCH}
    |"case"                     {! _number, CASE}
    |"default"                  {! _number, DEFAULT}
    |"int"                      {! _number, INT}
    |"double"                   {! _number, DOUBLE}
    |"string"                   {! _number, STRING}
    |"bool"                     {! _number, BOOL}
    |"Plate"                    {! _number, PLATE}
    |"Microwave"                {! _number, MICROWAVE}
    |"Bake"                     {! _number, BAKE}
    |"Container"                {! _number, CONTAINER}
    |"Stove"                    {! _number, STOVE}
    |"Vessel"                   {! _number, VESSEL}
    |"Cauldron"                 {! _number, CAULDRON}
    |"Bowl"                     {! _number, BOWL}
    |"NonStickPan"              {! _number, NONSTICKPAN}
    |"Pan"                      {! _number, PAN}
    |"Glass"                    {! _number, GLASS}
    |"SaucePan"                 {! _number, SAUCEPAN}
    |"PressureCooker"           {! _number, PRESSURECOOKER}
    |"Skillet"                  {! _number, SKILLET}
    |"Wok"                      {! _number, WOK}
    |"Ingredient"               {! _number, INGREDIENT}
    |"Tools"                    {! _number, TOOLS}
    |"Lid"                      {! _number, LID}
    |"Stirrer"                  {! _number, STIRRER}
    |"Spoon"                    {! _number, SPOON}
    |"Knife"                    {! _number, KNIFE}
    |"Peeler"                   {! _number, PEELER}
    |"TableSpoon"               {! _number, TABLESPOON}
    |"TeaSpoon"                 {! _number, TEASPOON}
    |"Grater"                   {! _number, GRATER}
    |"Whisk"                    {! _number, WHISK}
    |"FoodProcessor"            {! _number, FOODPROCESSOR}
    |"class"                    {! _number, CLASS}
    |"inherits"                 {! _number, INHERITS}
    |"public"                   {! _number, PUBLIC}
    |"private"                  {! _number, PRIVATE}
    |"protected"                {! _number, PROTECTED}
    |"unit"                     {! _number, UNIT}
    |"mass"                     {! _number, MASS}
    |"volume"                   {! _number, VOLUME}
    |"new"                      {! _number, NEW}
    |"ready"			        {! _number, READY}
    |"serve"                    {! _number, SERVE}
    |"return"                   {! _number, RETURN}
    |"recipe"                   {! _number, RECIPE}
    |"true" as lexemme          {! _number, BOOL_LIT(bool_of_string lexemme)}
    |"false" as lexemme         {! _number, BOOL_LIT(bool_of_string lexemme)}
    |integer_cons as lexemme    {! _number, INT_CONST(int_of_string lexemme)}
    |decimal_const as lexemme   {! _number, DEC_CONST(float_of_string lexemme)}
    |character(character|digit)* as lexemme {! _number, IDENTIFIER(lexemme)}
    |'"' [^'"''\n']+             {printf "Invalid string literal found at number %d.\n" ! _number;exit(1)}
    |string_lit as lexemme      {! _number, STR_LIT(lexemme)}
    |eof                        {raise Eof}
    |_                          {printf "Invalid token encountered at number %d.\n" ! _number;exit(1)}
    |"\\*"                      {multi_comment lexbuf}
                      
and multi_comment = parse
    "*\\"    {token lexbuf}
    |eof    {printf "No matching *\\ for the comment at number %d.\n" ! _number;exit(1)}
    |_      {multi_comment lexbuf}

{
open Printf
let main () = begin
    
    try
        let file_name = Sys.argv.(1) in
        let file_handle = open_in file_name in
        let lexbuf = Lexing.from_channel file_handle in 
        while true do
            let  line, result = token lexbuf in
            Printf.printf "%d\n" line;
            match result with 
            |PERIOD       -> printf "PERIOD\n";
            |COLON        -> printf "COLON\n";
            |DQOUTES      -> printf "DQOUTES\n";
            |LPARAN       -> printf "LPARAN\n";
            |RPARAN       -> printf "RPARAN\n";
            |LCURLY       -> printf "LCURLY\n";
            |RCURLY       -> printf "RCURLY\n";
            |LBRAC        -> printf "LBRAC\n";
            |RBRAC        -> printf "RBRAC\n";
            |COMMA        -> printf "COMMA\n";
            |SEMICOLON    -> printf "SEMICOLON\n";
            |PLUS         -> printf "PLUS\n";
            |PLUSASSIGN   -> printf "PLUSASSIGN\n";
            |MINUS        -> printf "MINUS\n";
            |MINUSASSIGN  -> printf "MINUSASSIGN\n";
            |MULT         -> printf "MULT\n";
            |MULTASSIGN   -> printf "MULTASSIGN\n";
            |DIV          -> printf "DIV\n";
            |DIVASSIGN    -> printf "DIVASSIGN\n";
            |MOD          -> printf "MOD\n";
            |MODASSIGN    -> printf "MODASSIGN\n";
            |POWER        -> printf "POWER\n";
            |POWERASSIGN  -> printf "POWERASSIGN\n";
            |ACCESS       -> printf "ACCESS\n";
            |ASSIGN       -> printf "ASSIGN\n";
            |EQUAL        -> printf "EQUAL\n";
            |NEQUAL       -> printf "NEQUAL\n";
            |LT           -> printf "LT\n";
            |LTE          -> printf "LTE\n";
            |GT           -> printf "GT\n";
            |GTE          -> printf "GTE\n";
            |AND          -> printf "AND\n";
            |OR           -> printf "OR\n";
            |NOT          -> printf "NOT\n";
            |IF           -> printf "IF\n";
            |ELSE         -> printf "ELSE\n";
            |WHILE        -> printf "WHILE\n";
            |FOR          -> printf "FOR\n";
            |SWITCH       -> printf "SWITCH\n";
            |CASE         -> printf "CASE\n";
            |DEFAULT      -> printf "DEFAULT\n";
            |INT          -> printf "INT\n";
            |DOUBLE       -> printf "DOUBLE\n";
            |STRING       -> printf "STRING\n";
            |BOOL         -> printf "BOOL\n";
            |MICROWAVE    -> printf "MICROWAVE\n";
            |BAKE         -> printf "BAKE\n";
            |CONTAINER    -> printf "CONTAINER\n";
            |STOVE        -> printf "STOVE\n";
            |VESSEL       -> printf "VESSEL\n";
            |CAULDRON     -> printf "CAULDRON\n";
            |BOWL         -> printf "BOWL\n";
            |NONSTICKPAN  -> printf "NONSTICKPAN\n";
            |PLATE        -> printf "PLATE\n";
            |PAN          -> printf "PAN\n";
            |GLASS        -> printf "GLASS\n";
            |SAUCEPAN     -> printf "SAUCEPAN\n";
            |PRESSURECOOKER -> printf "PRESSURECOOKER\n";
            |SKILLET      -> printf "SKILLET\n";
            |WOK          -> printf "WOK\n";
            |INGREDIENT   -> printf "INGREDIENT\n";
            |TOOLS        -> printf "TOOLS\n";
            |LID          -> printf "LID\n";
            |STIRRER      -> printf "STIRRER\n";
            |SPOON        -> printf "SPOON\n";
            |KNIFE        -> printf "KNIFE\n";
            |PEELER       -> printf "PEELER\n";
            |TABLESPOON   -> printf "TABLESPOON\n";
            |TEASPOON     -> printf "TEASPOON\n";
            |GRATER       -> printf "GRATER\n";
            |WHISK        -> printf "WHISK\n";
            |FOODPROCESSOR -> printf "FOODPROCESSOR\n";
            |CLASS        -> printf "CLASS\n";
            |INHERITS     -> printf "INHERITS\n";
            |PUBLIC       -> printf "PUBLIC\n";
            |PRIVATE      -> printf "PRIVATE\n";
            |PROTECTED    -> printf "PROTECTED\n";
            |UNIT         -> printf "UNIT\n";
            |MASS         -> printf "MASS\n";
            |VOLUME       -> printf "VOLUME\n";
            |NEW          -> printf "NEW\n";
            |READY        -> printf "READY\n";
            |SERVE        -> printf "SERVE\n";
            |RETURN       -> printf "RETURN\n";
            |RECIPE       -> printf "RECIPE\n";
            |BOOL_LIT(i)  -> printf "BOOL_LIT\n%b\n" i;
            |INT_CONST(i) -> printf "INT_CONST\n%d\n" i;
            |DEC_CONST(i) -> printf "DEC_CONST\n%f\n" i;
            |STR_LIT(i)   -> printf "STR_LIT\n%s\n" i;
            |IDENTIFIER(i)-> printf "IDENTIFIER\n%s\n" i;
        done
    with Eof -> exit 0
end;;
main();;
}
