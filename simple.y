%{
  #include <stdio.h>
  #include <string.h>

  int yyerror(char * s);
  extern FILE *yyin;
  extern int yylex();
  extern char * yytext;

  #define YYDEBUG 1
%}

%token COMA DOS_PUNTOS_H DOS_PUNTOS_V PUNTO_COMA
%token ASIGNACION CUATRO_PUNTOS

%token PROGRAMA FIN
%token DE COMO EXPORTAR IMPORTAR LIBRERIA

%token CORTO LARGO
%token BOOLEANO CARACTER ENTERO REAL
%token CONSTANTE ES RANGO SIGNO TIPO

%token IDENTIFICADOR
%token CTC_CADENA CTC_CARACTER CTC_ENTERA CTC_REAL FALSO VERDADERO

%%

/******************************** PROGRAMA ********************************/

programa              : definicion_programa
                      | definicion_libreria
                      ;
definicion_programa   : PROGRAMA IDENTIFICADOR PUNTO_COMA codigo_programa FIN
                      ;
codigo_programa       : importar
                      ;


definicion_libreria   : LIBRERIA IDENTIFICADOR PUNTO_COMA codigo_libreria FIN
                      ;
codigo_libreria       : imexportar declaraciones
                      ;
declaraciones         : declaracion declaraciones
                      | declaracion
                      ;
declaracion           : declaracion_objeto
                      | declaracion_tipo
                      ;

/************************** IMPORTAR / EXPORTAR ***************************/

imexportar            : importar exportar
                      | importar
                      | exportar
                      | /* vacio */
                      ;
exportar              : EXPORTAR ids PUNTO_COMA
importar              : importar libreria PUNTO_COMA
                      | libreria PUNTO_COMA
                      ;
libreria              : DE LIBRERIA nombre_libreria IMPORTAR IDENTIFICADOR
                      | IMPORTAR LIBRERIA nombre_libreria COMO IDENTIFICADOR
                      | IMPORTAR LIBRERIA nombre_libreria
                      ;
nombre_libreria       : IDENTIFICADOR CUATRO_PUNTOS nombre_libreria
                      | IDENTIFICADOR
                      ;


declaracion_objeto    : ids DOS_PUNTOS_V CONSTANTE tipo_basico ASIGNACION IDENTIFICADOR PUNTO_COMA
                      ;

/************************** DECLARACIÓN DE TIPOS **************************/

declaracion_tipo      : TIPO IDENTIFICADOR ES tipo_no_estructurado PUNTO_COMA
                      ;
tipo_no_estructurado  : tipo_escalar
                      ;

/************************ ESPECIFICACIÓN DE TIPOS *************************/

tipo_escalar          : SIGNO tipo_basico longitud rango
                      | SIGNO tipo_basico longitud
                      | SIGNO tipo_basico rango
                      | SIGNO tipo_basico
                      | tipo_basico longitud rango
                      | tipo_basico longitud
                      | tipo_basico rango
                      | tipo_basico
                      ;
tipo_basico           : BOOLEANO
                      | CARACTER
                      | ENTERO
                      | REAL
                      ;
longitud              : CORTO
                      | LARGO
                      ;
rango                 : RANGO numerico DOS_PUNTOS_H numerico DOS_PUNTOS_H numerico
                      | RANGO numerico DOS_PUNTOS_H numerico
                      ;

/******************************* PRIMARIOS ********************************/

ids                   : IDENTIFICADOR COMA ids
                      | IDENTIFICADOR
                      ;
numerico              : CTC_ENTERA | CTC_REAL
                      ;
literal               : VERDADERO
                      | FALSO
                      | CTC_CADENA
                      | CTC_CARACTER
                      | CTC_ENTERA
                      | CTC_REAL
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
