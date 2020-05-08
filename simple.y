%{
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>

  int enabledlogging;
  int yyerror(char * s);
  
  extern FILE *yyin;
  extern int yylex();
  extern int yylineno;
  extern char * yytext;

  #define YYDEBUG 1
%}

%token COMA CORCHETE_A CORCHETE_C DOS_PUNTOS_H DOS_PUNTOS_V LLAVE_A LLAVE_C PARENT_A PARENT_C PUNTO PUNTO_COMA
%token ASIGNACION CUATRO_PUNTOS FLECHA

%token FIN PRINCIPIO PROGRAMA SUBPROGRAMA
%token DE COMO EXPORTAR IMPORTAR LIBRERIA

%token CORTO LARGO
%token BOOLEANO CARACTER ENTERO REAL
%token CONSTANTE DICCIONARIO ENUMERACION ES LISTA RANGO REGISTRO SIGNO TABLA TIPO

%token PUBLICO PROTEGIDO PRIVADO
%token ABSTRACTO CLASE CONSTRUCTOR DESTRUCTOR ESPECIFICO FINAL GENERICO ULTIMA

%token SI ENTONCES SINO CASOS CUANDO OTRO
%token PARA EN REPETIR MIENTRAS DESCENDENTE BUCLE
%token SALIR SIGUIENTE
%token LANZA PRUEBA EXCEPCION FINALMENTE
%token DEVOLVER REFERENCIA VALOR

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
declaraciones_op      : declaraciones
                      | /* opcional */
declaracion           : declaracion_objeto | declaracion_tipo | declaracion_sprograma
                      ;

/************************** IMPORTAR / EXPORTAR ***************************/

imexportar            : importar exportar
                      | importar
                      | exportar
                      | /* vacio */
                      ;
exportar              : EXPORTAR nombre_librerias PUNTO_COMA
importar              : importar libreria PUNTO_COMA
                      | libreria PUNTO_COMA
                      ;
libreria              : DE LIBRERIA nombre_libreria IMPORTAR nombre_librerias;
                      | IMPORTAR LIBRERIA nombre_libreria COMO IDENTIFICADOR
                      | IMPORTAR LIBRERIA nombre_libreria
                      ;
nombre_librerias      : nombre_libreria COMA nombre_librerias
                      | nombre_libreria
                      ;
nombre_libreria       : IDENTIFICADOR CUATRO_PUNTOS nombre_libreria
                      | IDENTIFICADOR
                      ;

/************************* DECLARACIÓN DE OBJETOS *************************/

declaracion_objeto    : declaracion_constante | declaracion_variable
                      ;
declaracion_constante : ids DOS_PUNTOS_V CONSTANTE especificacion_tipo ASIGNACION expresion PUNTO_COMA
                      ;
declaracion_variable  : ids DOS_PUNTOS_V especificacion_tipo ASIGNACION expresion PUNTO_COMA
                      | ids DOS_PUNTOS_V especificacion_tipo PUNTO_COMA
                      ;

/************************** DECLARACIÓN DE TIPOS **************************/

declaracion_tipo      : TIPO IDENTIFICADOR ES tipo_no_estructurado PUNTO_COMA
                      | TIPO IDENTIFICADOR ES tipo_estructurado
                      ;

/************************ ESPECIFICACIÓN DE TIPOS *************************/

especificacion_tipo_op: DOS_PUNTOS_V especificacion_tipo
                      | /* opcional */
                      ;
especificacion_tipo   : IDENTIFICADOR | tipo_no_estructurado
                      ;
tipo_estructurado     : tipo_registro | tipo_enumerado | clase
                      ;
tipo_registro         : REGISTRO campos FIN REGISTRO
                      ;
tipo_enumerado        : ENUMERACION DE tipo_escalar elementos_enumeracion FIN ENUMERACION  
                      | ENUMERACION elementos_enumeracion FIN ENUMERACION       
                      ;
tipo_no_estructurado  : tipo_escalar
                      | tipo_tabla
                      | tipo_diccionario
                      ;
tipo_escalar          : SIGNO tipo_basico longitud rango
                      | SIGNO tipo_basico longitud
                      | SIGNO tipo_basico rango
                      | SIGNO tipo_basico
                      | tipo_basico longitud rango
                      | tipo_basico longitud
                      | tipo_basico rango
                      | tipo_basico
                      ;
tipo_tabla            : TABLA PARENT_A expresion DOS_PUNTOS_H expresion PARENT_C DE especificacion_tipo
                      | LISTA DE especificacion_tipo
                      ;
tipo_diccionario      : DICCIONARIO DE especificacion_tipo
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

/********************************* CLASES *********************************/

clase                 : CLASE ULTIMA superclases decl_componentes FIN CLASE
                      | CLASE superclases decl_componentes FIN CLASE
                      ;
superclases           : PARENT_A ids PARENT_C
                      | /* opcional */
                      ;
decl_componentes      : decl_componente decl_componentes
                      | decl_componente
                      ;
decl_componente       : visibilidad componente
                      ;
componente            : declaracion_tipo
                      | declaracion_objeto
                      | modificadores declaracion_sprograma
                      ;
visibilidad           : PUBLICO | PROTEGIDO | PRIVADO
                      | /* opcional */
                      ;
modificadores         : modificador modificadores
                      | modificador
                      | /* opcional */
                      ;
modificador           : CONSTRUCTOR | DESTRUCTOR | GENERICO | ABSTRACTO | ESPECIFICO | FINAL
                      ;

/****************************** SUBPROGRAMAS ******************************/

declaracion_sprograma : SUBPROGRAMA cabecera_subprograma cuerpo_subprograma SUBPROGRAMA
                      ;
cabecera_subprograma  : IDENTIFICADOR parametrizacion tipo_resultado
                      ;
cuerpo_subprograma    : declaraciones_op PRINCIPIO instrucciones FIN
                      ;
definicion_parametros : definicion_parametro definicion_parametros
                      | definicion_parametro
                      | /* opcional */
                      ;
definicion_parametro  : IDENTIFICADOR ASIGNACION expresion
                      | expresion
                      ;
parametrizacion       : PARENT_A declaracion_parametrs PARENT_C
                      | /* opcional */
                      ;
declaracion_parametrs : paramtros PUNTO_COMA declaracion_parametrs
                      | paramtros
                      ;
paramtros             : ids DOS_PUNTOS_V modo especificacion_tipo ASIGNACION expresion
                      | ids DOS_PUNTOS_V modo especificacion_tipo
                      ;
modo                  : VALOR | REFERENCIA
                      | /* opcional */
                      ;
tipo_resultado        : DEVOLVER especificacion_tipo
                      | /* opcional */
                      ;
llamada_subprograma   : nombre_libreria PARENT_A definicion_parametros PARENT_C

/***************************** INSTRUCCIONES ******************************/

instrucciones         : instruccion instrucciones
                      | instruccion
                      ;
instruccion           : instr_asignacion | instr_devolver | instr_llamada
                      | instr_si | instr_casos | instr_bucle | instr_interrupcion
                      | instr_lanzar | instr_capturar
                      ;
instr_asignacion      : objeto op_asignacion expresion PUNTO_COMA
                      ;
instr_devolver        : DEVOLVER expresion PUNTO_COMA
                      | DEVOLVER PUNTO_COMA
                      ;
instr_llamada         : llamada_subprograma PUNTO_COMA
                      ;
instr_si              : SI expresion ENTONCES instrucciones SINO instrucciones FIN SI
                      | SI expresion ENTONCES instrucciones FIN SI
                      ;
instr_casos           : CASOS expresion ES casos FIN CASOS
                      ;
instr_bucle           : IDENTIFICADOR DOS_PUNTOS_V clasula_iteracion instrucciones FIN BUCLE
                      | clasula_iteracion instrucciones FIN BUCLE
                      ;
instr_interrupcion    : SIGUIENTE cuando PUNTO_COMA
                      | SALIR DE IDENTIFICADOR cuando PUNTO_COMA
                      | SALIR cuando PUNTO_COMA
                      ;
instr_lanzar          : LANZA nombre_libreria PUNTO_COMA
                      ;
instr_capturar        : PRUEBA instrucciones clausulas FIN PRUEBA
                      ;
                      
casos                 : caso casos
                      | caso
                      ;
caso                  : CUANDO entradas FLECHA instrucciones
                      ;
entradas              : entrada DOS_PUNTOS_V entradas
                      | entrada
                      ;
entrada               : expresion DOS_PUNTOS_H expresion
                      | expresion
                      | OTRO
                      ;
clasula_iteracion     : PARA IDENTIFICADOR especificacion_tipo_op EN expresion
                      | REPETIR IDENTIFICADOR especificacion_tipo_op
                      | EN rango descendiente_op
                      | MIENTRAS expresion
descendiente_op       : DESCENDENTE
                      | /* opcional */
                      ;
cuando                : CUANDO expresion
                      | /* opcional */
                      ;
clausulas             : clausula_excepcion clausula_finalmente
                      | clausula_finalmente
                      | clausula_excepcion
                      ;
clausula_excepcion    : clausulas_especificas clausula_general
                      ;
clausulas_especificas : clausula_especifica clausulas_especificas
                      | clausula_especifica
                      | /* opcional */
                      ;
clausula_especifica   : EXCEPCION PARENT_A nombre_libreria PARENT_C instrucciones
                      ;
clausula_general      : EXCEPCION instrucciones
                      ;
clausula_finalmente   : FINALMENTE instrucciones
                      ;

/****************************** OPERADORES ********************************/

op_asignacion         : /* TODO */
                      ;

/****************************** EXPRESIONES *******************************/

expresiones           : expresion expresiones
                      | expresion
                      ;
expresion             : literal
                      | IDENTIFICADOR
                      ;

/******************************* PRIMARIOS ********************************/

primario              : PARENT_A expresion PARENT_C
                      | objeto llamada_subprograma
                      | llamada_subprograma
                      | enumeracion
                      | objeto
                      | literal
                      ;
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
cadenas               : CTC_CADENA cadenas
                      | cadenas
                      ;
objeto                : nombre_libreria
                      | objeto PUNTO IDENTIFICADOR
                      | objeto CORCHETE_A expresiones CORCHETE_C
                      | objeto LLAVE_A cadenas LLAVE_C
                      ;
campos                : declaracion_variable campos
                      | declaracion_variable
                      ;
enumeracion           : /* TODO */
                      ;
elementos_enumeracion : elemento_enumeracion elementos_enumeracion
                      | elemento_enumeracion
                      ;
elemento_enumeracion  : IDENTIFICADOR ASIGNACION expresion
                      | IDENTIFICADOR
                      ;
clave_valor           : CTC_CADENA FLECHA expresion
                      ;
campo_valor           : IDENTIFICADOR FLECHA expresion
                      ;

%%

/*
%token ENUMERACION EXCEPCION FINALMENTE
%token LANZA
%token PRUEBA SALIR
%token SIGUIENTE VALOR
%token INC DEC DESPI DESPD LEQ GEQ NEQ AND OR ASIG_SUMA ASIG_RESTA
%token ASIG_MULT ASIG_DIV ASIG_RESTO ASIG_POT ASIG_DESPI ASIG_DESPD
*/

int yyerror (char *msg) {
  fflush(stdout);
  printf("***************** Error en la línea %d cerca de '%s': %s\n", yylineno, yytext, msg);
}

int yywrap() { return(1); }

int main(int argc, char *argv[]) {

  /* Debug options */
  yydebug = 0;        // Bison
  enabledlogging = 1; // Mensajes del lexer

  /* Argument validator */
  if (argc < 2) {
    fprintf(stderr, "Uso: ./simple <archivo>\n");
    exit(1);
  }

  /* Program */
  yyin = fopen(argv[1],"r");
  yyparse();
  fclose(yyin);

  return 0;
}
