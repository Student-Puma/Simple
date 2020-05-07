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
%token DE COMO EXPORTAR IMPORTAR LIBRERIA
%token COMA CUATRO_PUNTOS PUNTO_COMA
%token IDENTIFICADOR
%token CTC_CADENA CTC_CARACTER CTC_ENTERA CTC_REAL

%%

programa            : definicion_programa
                    | definicion_libreria
                    ;

definicion_programa : PROGRAMA IDENTIFICADOR PUNTO_COMA codigo_programa FIN
                    ;
codigo_programa     : importar
                    ;

definicion_libreria : LIBRERIA IDENTIFICADOR PUNTO_COMA codigo_libreria FIN
                    ;
codigo_libreria     : importar exportar
                    | importar
                    ;

exportar            : EXPORTAR ids PUNTO_COMA
importar            : importar libreria PUNTO_COMA
                    | libreria PUNTO_COMA
                    ;
libreria            : DE LIBRERIA nombre_libreria IMPORTAR IDENTIFICADOR
                    | IMPORTAR LIBRERIA nombre_libreria COMO IDENTIFICADOR
                    | IMPORTAR LIBRERIA nombre_libreria
                    ;
nombre_libreria     : IDENTIFICADOR CUATRO_PUNTOS nombre_libreria
                    | IDENTIFICADOR
                    ;

ids                 : IDENTIFICADOR COMA ids
                    | IDENTIFICADOR
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
  printf("***************** %s (token: %s)\n",s, yytext);
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
