%{
  #include <stdio.h>
  #include <string.h>

  int yyerror(char * s);
  extern FILE *yyin;
  extern int yylex();
  extern char * yytext;

  #define YYDEBUG 1
%}

%token PROGRAMA FIN
%token IMPORTAR LIBRERIA COMO
%token CUATRO_PUNTOS PUNTO_COMA
%token IDENTIFICADOR
%token CTC_CADENA CTC_CARACTER CTC_ENTERA CTC_REAL

%union {
  char * texto;
}

%type<texto> IDENTIFICADOR id libreria

%%

programa  : PROGRAMA IDENTIFICADOR PUNTO_COMA codigo FIN                        { printf("Programa completado\n"); }
          ;
codigo    : importar
          ;
importar  : importar PUNTO_COMA
          | IMPORTAR LIBRERIA libreria COMO id                                  { printf("Importando como\n"); }
          | IMPORTAR LIBRERIA libreria                                          { printf("Importando\n"); }
          ;
libreria  : libreria CUATRO_PUNTOS id
          | id
          ;
id        : IDENTIFICADOR
          ;
%%

/*
%token ABSTRACTO BOOLEANO BUCLE CARACTER CASOS CLASE COMO CONSTANTE CONSTRUCTOR CORTO
%token CUANDO DE DESCENDENTE DESTRUCTOR DEVOLVER DICCIONARIO EN ENTERO ENTONCES
%token ENUMERACION ES ESPECIFICO EXCEPCION EXPORTAR FALSO FIN FINAL FINALMENTE GENERICO
%token IMPORTAR LARGO LANZA LIBRERIA LISTA MIENTRAS OBJETO OTRO PARA PRINCIPIO PRIVADO
%token PROGRAMA PROTEGIDO PRUEBA PUBLICO RANGO REAL REFERENCIA REGISTRO REPETIR SALIR
%token SI SIGNO SIGUIENTE SINO SUBPROGRAMA TABLA TIPO ULTIMA VALOR VERDADERO CTC_CARACTER
%token CTC_CADENA IDENTIFICADOR CTC_ENTERA CTC_REAL DOS_PUNTOS CUATRO_PUNTOS
%token ASIGNACION FLECHA INC DEC DESPI DESPD LEQ GEQ NEQ AND OR ASIG_SUMA ASIG_RESTA
%token ASIG_MULT ASIG_DIV ASIG_RESTO ASIG_POT ASIG_DESPI ASIG_DESPD
*/

int yyerror (char *s) {
  fflush(stdout);
  printf("***************** %s\n",s);
}

int yywrap() {
  return(1);
}

int main(int argc, char *argv[]) {
  yydebug = 0;
  if (argc < 2) {
    printf("Uso: ./simple NombreArchivo\n");
  } else {
    yyin = fopen(argv[1],"r");
    yyparse();
  }
}
