%name KeisankiLexer;

%let digit = [0-9];
%let num   = {digit}+;
%let alpha = [a-zA-Z];
%let id    = {alpha}({alpha}|{digit})*;
%let ws    = [ \t\n];

%defs (
  structure T = KeisankiTokens
  type lex_result = T.token
  fun eof () = T.EOF
);

let   => (T.LET);
in    => (T.IN);
{id}  => (T.ID yytext);
{num} => (T.NUM (valOf (Int.fromString yytext)));
"="   => (T.EQ);
"+"   => (T.PLUS);
"-"   => (T.MINUS);
"*"   => (T.TIMES);
"("   => (T.LPAREN);
")"   => (T.RPAREN);
{ws}  => (continue ());
.     => (print "Error"; continue ());
