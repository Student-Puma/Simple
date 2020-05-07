%{
  // Autor: Diego Enrique Fontan Lorenzo

  #include <stdio.h>
  #include <string.h>
  #include "simple.tab.h"

  extern int enabledlogging;

  void logging(char * type)
  { if(enabledlogging) fprintf(stdout, "linea %d, %s: %s\n", yylineno, type, yytext); }

  void ignorecase()
  { for(char *p = yytext; *p; ++p) *p = *p>0x60 && *p<0x7b ? *p - 0x20 : *p; }
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
                                      if(strcmp(yytext, "BOOLEANO") == 0)       return BOOLEANO;
                                      if(strcmp(yytext, "CARACTER") == 0)       return CARACTER;
                                      if(strcmp(yytext, "COMO") == 0)           return COMO;
                                      if(strcmp(yytext, "CONSTANTE") == 0)      return CONSTANTE;
                                      if(strcmp(yytext, "CORTO") == 0)          return CORTO;
                                      if(strcmp(yytext, "DE") == 0)             return DE;
                                      if(strcmp(yytext, "ENTERO") == 0)         return ENTERO;
                                      if(strcmp(yytext, "ES") == 0)             return ES;
                                      if(strcmp(yytext, "EXPORTAR") == 0)       return EXPORTAR;
                                      if(strcmp(yytext, "FALSO") == 0)          return FALSO;
                                      if(strcmp(yytext, "FIN") == 0)            return FIN;
                                      if(strcmp(yytext, "IMPORTAR") == 0)       return IMPORTAR;
                                      if(strcmp(yytext, "LARGO") == 0)          return LARGO;
                                      if(strcmp(yytext, "LIBRERIA") == 0)       return LIBRERIA;
                                      if(strcmp(yytext, "PROGRAMA") == 0)       return PROGRAMA;
                                      if(strcmp(yytext, "RANGO") == 0)          return RANGO;
                                      if(strcmp(yytext, "REAL") == 0)           return REAL;
                                      if(strcmp(yytext, "SIGNO") == 0)          return SIGNO;
                                      if(strcmp(yytext, "TIPO") == 0)           return TIPO;
                                      if(strcmp(yytext, "VERDADERO") == 0)      return VERDADERO;
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
                                      else if(strcmp(yytext, "..") == 0) return(DOS_PUNTOS_H);
                                      else if(strcmp(yytext, ":") == 0) return(DOS_PUNTOS_V);
                                      else if(strcmp(yytext, ";") == 0) return(PUNTO_COMA);
                                    }
{operator}                          { logging("operador");
                                      if(strcmp(yytext, ":=") == 0) return(ASIGNACION);
                                      else if(strcmp(yytext, "::") == 0) return(CUATRO_PUNTOS);
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
