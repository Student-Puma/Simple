%{
  // Autor: Diego Enrique Fontan Lorenzo

  #include <stdio.h>
  #include <string.h>
  #include "simple.tab.h"

  void logging(char * type)
  { if(1) fprintf(stdout, "linea %d, %s: %s\n", yylineno, type, yytext); }

  void ignorecase()
  { for(char *p = yytext; *p; ++p) *p = *p>0x40 && *p<0x5b ? *p|0x60 : *p; }
%}

%option yylineno

digit       [0-9]
letter      [a-zA-Z]

b8          [0-7]
b16         [0-9a-fA-F]

%x commentml
comment     ##.*\n
commentbgn  #\{
commentend  #\}

char        [^\n'% \t\b\r]
ascii8      [oO][0-3]?{b8}{1,2}
ascii10     [01]?{digit}{1,2}|2[0-4]{digit}|25[0-5]
ascii16     [xX]{b16}{1,2}

character   {char}|%(['"%nrt]|{ascii8}|{ascii10}|{ascii16})
string      \"([^"]|%\")*\"

octal       %[oO]({b8}+)?(\.)?{b8}+(\^[+-]?{b8}+)?
decimal     ({digit}+)?(\.)?{digit}+(\^[+-]?{digit}+)?
hexadecimal %[xX]({b16}+)?(\.)?{b16}+(\^[+-]?{b16}+)?

mathnbit    [-+*/\\^~]|\+{2}|-{2}|<-|->|\/\\|\\\/
assignment  :[-+=/<>\\^]
memory      [\[{.}\]]|:{2}
relation    [<>]|[<>~]?=
operator    {assignment}|{mathnbit}|{memory}|{relation}
delimiter   [(:;,|)]|\.{2}|=>

keywords    abstracto|booleano|bucle|caracter|casos|clase|como|constante|constructor|corto|cuando|de|descendente|destructor|devolver|diccionario|en|entero|entonces|enumeracion|es|especifico|excepcion|exportar|falso|fin|final|finalmente|generico|importar|largo|lanza|libreria|lista|mientras|objeto|otro|para|principio|privado|programa|protegido|prueba|publico|rango|real|referencia|registro|repetir|salir|si|signo|siguiente|sino|subprograma|tabla|tipo|ultima|valor|verdadero
ickeyword   (?i:{keywords})

identifier  [a-zA-Z_][a-zA-Z0-9_]*

%%

{string}                            { logging("cadena"); return(CTC_CADENA); }
{ickeyword}                         { logging("palabra reservada");
                                      ignorecase();
                                      if(strcmp(yytext, "como") == 0) return(COMO);
                                      else if(strcmp(yytext, "de") == 0) return(DE);
                                      else if(strcmp(yytext, "exportar") == 0) return(EXPORTAR);
                                      else if(strcmp(yytext, "fin") == 0) return(FIN);
                                      else if(strcmp(yytext, "importar") == 0) return(IMPORTAR);
                                      else if(strcmp(yytext, "libreria") == 0) return(LIBRERIA);
                                      else if(strcmp(yytext, "programa") == 0) return(PROGRAMA);
                                    }
\'{character}\'                     { logging("caracter"); return(CTC_CARACTER); }
{comment}|[\r\n\t\b ]               ;
{commentbgn}                        BEGIN(commentml);
<commentml>.|\n                     ;
<commentml>{commentend}             BEGIN(INITIAL);
{identifier}                        { logging("identificador"); return(IDENTIFICADOR); }
{b8}+|{digit}+|{b16}+               { logging("ctc entera"); return(CTC_ENTERA); }
{octal}|{decimal}|{hexadecimal}     { logging("ctc real"); return(CTC_REAL); }
{delimiter}                         { logging("delimitador");
                                      if(strcmp(yytext, ",") == 0) return(COMA);
                                      else if(strcmp(yytext, ";") == 0) return(PUNTO_COMA);
                                    }
{operator}                          { logging("operador");
                                      if(strcmp(yytext, "::") == 0) return(CUATRO_PUNTOS);
                                    }

.                                   { logging("ERROR: caracter incorrecto"); }

%%

/*
int yywrap() { return(1); }

int main(int argc, char *argv[]) {

  if (argc < 2)
  {
    fprintf(stderr, "Uso: ./simple <archivo>\n");
    exit(1);
  }
  else
  {
    yyin = fopen(argv[1],"r");
    yylex();
  }

  return 0;
}
*/
