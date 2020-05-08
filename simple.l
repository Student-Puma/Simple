%{
  // Autor: Diego Enrique Fontan Lorenzo

  #include <stdio.h>
  #include <string.h>
  #include "simple.tab.h"

  extern int enabledlogging;

  int es(char * token)
  { return strcmp(yytext, token) == 0; }

  void ignorecase()
  { for(char *p = yytext; *p; ++p) *p = *p>0x60 && *p<0x7b ? *p - 0x20 : *p; }

  void logging(char * type)
  { if(enabledlogging) fprintf(stdout, "linea %d, %s: %s\n", yylineno, type, yytext); }
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

{string}                            { logging("cadena");        return CTC_CADENA; }
{ickeyword}                         { logging("palabra reservada");
                                      ignorecase();
                                      if(es("ABSTRACTO"))       return ABSTRACTO;
                                      if(es("BOOLEANO"))        return BOOLEANO;
                                      if(es("BUCLE"))           return BUCLE;
                                      if(es("CARACTER"))        return CARACTER;
                                      if(es("CASOS"))           return CASOS;
                                      if(es("CLASE"))           return CLASE;
                                      if(es("COMO"))            return COMO;
                                      if(es("CONSTANTE"))       return CONSTANTE;
                                      if(es("CONSTRUCTOR"))     return CONSTRUCTOR;
                                      if(es("CORTO"))           return CORTO;
                                      if(es("CUANDO"))          return CUANDO;
                                      if(es("DE"))              return DE;
                                      if(es("DESCENDENTE"))     return DESCENDENTE;
                                      if(es("DESTRUCTOR"))      return DESTRUCTOR;
                                      if(es("DEVOLVER"))        return DEVOLVER;
                                      if(es("DICCIONARIO"))     return DICCIONARIO;
                                      if(es("EN"))              return EN;
                                      if(es("ENTERO"))          return ENTERO;
                                      if(es("ENTONCES"))        return ENTONCES;
                                      if(es("ENUMERACION"))     return ENUMERACION;
                                      if(es("ES"))              return ES;
                                      if(es("ESPECIFICO"))      return ESPECIFICO;
                                      if(es("EXCEPCION"))       return EXCEPCION;
                                      if(es("EXPORTAR"))        return EXPORTAR;
                                      if(es("FALSO"))           return FALSO;
                                      if(es("FIN"))             return FIN;
                                      if(es("FINAL"))           return FINAL;
                                      if(es("FINALMENTE"))      return FINALMENTE;
                                      if(es("GENERICO"))        return GENERICO;
                                      if(es("IMPORTAR"))        return IMPORTAR;
                                      if(es("LANZA"))           return LANZA;
                                      if(es("LARGO"))           return LARGO;
                                      if(es("LIBRERIA"))        return LIBRERIA;
                                      if(es("LISTA"))           return LISTA;
                                      if(es("MIENTRAS"))        return MIENTRAS;
                                      if(es("OTRO"))            return OTRO;
                                      if(es("PARA"))            return PARA;
                                      if(es("PRINCIPIO"))       return PRINCIPIO;
                                      if(es("PRIVADO"))         return PRIVADO;
                                      if(es("PROGRAMA"))        return PROGRAMA;
                                      if(es("PROTEGIDO"))       return PROTEGIDO;
                                      if(es("PRUEBA"))          return PRUEBA;
                                      if(es("PUBLICO"))         return PUBLICO;
                                      if(es("RANGO"))           return RANGO;
                                      if(es("REAL"))            return REAL;
                                      if(es("REFERENCIA"))      return REFERENCIA;
                                      if(es("REGISTRO"))        return REGISTRO;
                                      if(es("REPETIR"))         return REPETIR;
                                      if(es("SALIR"))           return SALIR;
                                      if(es("SI"))              return SI;
                                      if(es("SIGUIENTE"))       return SIGUIENTE;
                                      if(es("SINO"))            return SINO;
                                      if(es("SIGNO"))           return SIGNO;
                                      if(es("SUBPROGRAMA"))     return SUBPROGRAMA;
                                      if(es("TABLA"))           return TABLA;
                                      if(es("TIPO"))            return TIPO;
                                      if(es("ULTIMA"))          return ULTIMA;
                                      if(es("VALOR"))           return VALOR;
                                      if(es("VERDADERO"))       return VERDADERO;
                                    }
\'{character}\'                     { logging("caracter");      return CTC_CARACTER; }
{comment}|[\r\n\t\b ]               ;
{commentbgn}                        BEGIN(commentml);
<commentml>.|\n                     ;
<commentml>{commentend}             BEGIN(INITIAL);
{identifier}                        { logging("identificador"); return IDENTIFICADOR; }
{b8}+|{digit}+|{b16}+               { logging("ctc entera");    return CTC_ENTERA; }
{octal}|{decimal}|{hexadecimal}     { logging("ctc real");      return CTC_REAL; }
{delimiter}                         { logging("delimitador");
                                      if(es(","))               return COMA;
                                      if(es("["))               return CORCHETE_A;
                                      if(es("]"))               return CORCHETE_C;
                                      if(es(".."))              return DOS_PUNTOS_H;
                                      if(es(":"))               return DOS_PUNTOS_V;
                                      if(es("{"))               return LLAVE_A;
                                      if(es("}"))               return LLAVE_C;
                                      if(es("("))               return PARENT_A;
                                      if(es(")"))               return PARENT_C;
                                      if(es("."))               return PUNTO;
                                      if(es(";"))               return PUNTO_COMA;
                                    }
{operator}                          { logging("operador");
                                      if(es(":="))              return ASIGNACION;
                                      if(es("::"))              return CUATRO_PUNTOS;
                                      if(es("=>"))              return FLECHA;
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
