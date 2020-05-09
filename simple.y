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

/* %token '=' '+' '-' '*' '/' '~' '<' '>' */
/* %token ',' '[' ']' ':' '{' '}' '(' ')' '.' ';' */
%token POT RESTO

%token CUATRO_PUNTOS FLECHA DOS_PUNTOS
%token INC DEC DESPI DESPD LEQ GEQ NEQ AND OR
%token ASIGNACION ASIG_SUMA ASIG_RESTA ASIG_MULT ASIG_DIV ASIG_RESTO ASIG_POT ASIG_DESPI ASIG_DESPD

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
definicion_programa   : PROGRAMA IDENTIFICADOR ';' codigo_programa
                      ;
codigo_programa       : importar cuerpo_subprograma
                      | cuerpo_subprograma
                      ;

definicion_libreria   : LIBRERIA IDENTIFICADOR ';' codigo_libreria
                      ;
codigo_libreria       : imexportar declaraciones
                      ;
declaraciones         : declaracion declaraciones
                      | declaracion
                      ;
declaracionesop       : declaraciones
                      | /* opcional */
declaracion           : declaracion_objeto | declaracion_tipo | declaracion_sprograma
                      ;


imexportar            : importar exportar
                      | importar
                      | exportar
                      | /* vacio */
                      ;
exportar              : EXPORTAR nombre_librerias ';'
importar              : importar libreria ';'
                      | libreria ';'
                      ;
libreria              : DE LIBRERIA nombre_libreria IMPORTAR nombre_librerias
                      | IMPORTAR LIBRERIA nombre_libreria COMO IDENTIFICADOR
                      | IMPORTAR LIBRERIA nombre_libreria
                      ;
nombre_librerias      : nombre_libreria ',' nombre_librerias
                      | nombre_libreria
                      ;
nombre_libreria       : IDENTIFICADOR CUATRO_PUNTOS nombre_libreria
                      | IDENTIFICADOR
                      ;

/************************* DECLARACIÓN DE OBJETOS *************************/

declaracion_objeto    : declaracion_constante | declaracion_variable
                      ;
declaracion_constante : ids ':' CONSTANTE especificacion_tipo ASIGNACION expresion ';'
                      ;
declaracion_variable  : ids ':' especificacion_tipo ASIGNACION expresion ';'
                      | ids ':' especificacion_tipo ';'
                      ;

/************************** DECLARACIÓN DE TIPOS **************************/

declaracion_tipo      : TIPO IDENTIFICADOR ES tipo_no_estructurado ';'
                      | TIPO IDENTIFICADOR ES tipo_estructurado
                      ;

/************************ ESPECIFICACIÓN DE TIPOS *************************/

especificacion_tipo   : nombre_libreria | tipo_no_estructurado
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
tipo_tabla            : TABLA '(' expresion DOS_PUNTOS expresion ')' DE especificacion_tipo
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
rango                 : RANGO expresion DOS_PUNTOS expresion DOS_PUNTOS expresion
                      | RANGO expresion DOS_PUNTOS expresion
                      ;

/********************************* CLASES *********************************/

clase                 : CLASE ULTIMA superclases decl_componentes FIN CLASE
                      | CLASE superclases decl_componentes FIN CLASE
                      ;
superclases           : '(' ids ')'
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
                      | declaracion_sprograma
                      ;
visibilidad           : PUBLICO | PROTEGIDO | PRIVADO
                      | /* opcional */
                      ;
modificadores         : modificador modificadores
                      | modificador
                      ;
modificador           : CONSTRUCTOR | DESTRUCTOR | GENERICO | ABSTRACTO | ESPECIFICO | FINAL
                      ;

/****************************** SUBPROGRAMAS ******************************/

declaracion_sprograma : SUBPROGRAMA cabecera_subprograma cuerpo_subprograma SUBPROGRAMA
                      ;
cabecera_subprograma  : IDENTIFICADOR parametrizacion tipo_resultado
                      | IDENTIFICADOR parametrizacion
                      ;
cuerpo_subprograma    : declaracionesop PRINCIPIO instrucciones FIN
                      ;
parametrizacion       : '(' declaracion_parametrs ')'
                      | /* opcional */
                      ;
definicion_parametros : definicion_parametro ',' definicion_parametros
                      | definicion_parametro
                      ;
definicion_parametro  : IDENTIFICADOR ASIGNACION expresion
                      | expresion
                      ;
declaracion_parametrs : paramtros ';' declaracion_parametrs
                      | paramtros
                      ;
paramtros             : ids ':' modo especificacion_tipo ASIGNACION expresion
                      | ids ':' modo especificacion_tipo
                      ;
modo                  : VALOR | REFERENCIA
                      | /* opcional */
                      ;
tipo_resultado        : DEVOLVER especificacion_tipo
                      ;
llamada_subprograma   : nombre_libreria '(' definicion_parametros ')'
                      | nombre_libreria '(' ')'
                      ;

/***************************** INSTRUCCIONES ******************************/

instrucciones         : instruccion instrucciones
                      | instruccion
                      ;
instruccion           : instr_llamada | instr_bucle | instr_capturar | instr_asignacion | instr_lanzar | instr_devolver | instr_vacia | instr_si
                      ;
instr_llamada         : llamada_subprograma ';'
                      ;
instr_asignacion      : objeto ASIGNACION expresion ';' /* FIXME: Falla con operaciones aritmeticas */
                      ;
instr_lanzar          : LANZA nombre_libreria ';'
                      ;
instr_devolver        : DEVOLVER expresion ';'
                      | DEVOLVER ';'
                      ;
instr_vacia           : ';'
                      ;

/************************* BUCLES Y CONDICIONALES *************************/

instr_si              : SI expresion ENTONCES instrucciones SINO instrucciones FIN SI
                      | SI expresion ENTONCES instrucciones FIN SI
                      ;
instr_bucle           : IDENTIFICADOR ':' clausula_iteracion instrucciones FIN BUCLE
                      | clausula_iteracion instrucciones FIN BUCLE
                      ;
clausulas_iteracion   : clausula_iteracion clausulas_iteracion
                      | clausula_iteracion
                      ;
clausula_iteracion    : PARA IDENTIFICADOR ':' especificacion_tipo EN expresion
                      | PARA IDENTIFICADOR EN expresion
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN rango DESCENDENTE
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN rango
                      | REPETIR IDENTIFICADOR EN rango DESCENDENTE
                      | REPETIR IDENTIFICADOR EN rango
                      | MIENTRAS expresion
                      ;

/******************************** PRUEBAS *********************************/

instr_capturar        : PRUEBA instrucciones clausulas FIN PRUEBA
                      ;
clausulas             : clausulas_excepcion clausula_finalmente
                      | clausulas_excepcion
                      | clausula_finalmente
                      ;
clausulas_excepcion   : clausulas_especificas
                      ;
clausulas_especificas : clausula_excepcion clausulas_especificas
                      | clausula_excepcion
                      ;
clausula_excepcion    : EXCEPCION '(' nombre_libreria ')' instrucciones
                      | EXCEPCION instrucciones /* FIXME: Exc General aparte */
                      ;
clausula_finalmente   : FINALMENTE instrucciones
                      ;

/****************************** EXPRESIONES *******************************/

expresiones           : expresion ',' expresiones
                      | expresion
                      ;
expresion_condicional : SI expresion ENTONCES expresion SINO expresion
                      | SI expresion ENTONCES expresion
                      ;

/* TODO: Refractor */
                      
expresion
		: and_logico
		| expresion OR and_logico		 { printf ("  or_logico ->  or_logico '\\/' and_logico\n"); }
		;

and_logico
		: negacion
		| and_logico AND negacion		 { printf ("  and_logico ->  and_logico '/\\' negacion\n"); }
		;

negacion
		: comparacion
		| '~' comparacion { printf ("  negacion -> '~' comparacion\n"); }
		;

comparacion
		: desplazamiento
		| desplazamiento '>' desplazamiento { printf ("  comparacion ->  desplazamiento '>' desplazamiento\n"); }
		| desplazamiento '<' desplazamiento { printf ("  comparacion ->  desplazamiento '<' desplazamiento\n"); }
		| desplazamiento '=' desplazamiento { printf ("  comparacion ->  desplazamiento '=' desplazamiento\n"); }
		| desplazamiento LEQ desplazamiento { printf ("  comparacion ->  desplazamiento '<=' desplazamiento\n"); }
		| desplazamiento GEQ desplazamiento { printf ("  comparacion ->  desplazamiento '>=' desplazamiento\n"); }
		| desplazamiento NEQ desplazamiento { printf ("  comparacion ->  desplazamiento '<>' desplazamiento\n"); }
		;

desplazamiento
		:	suma_resta
		| desplazamiento DESPD suma_resta { printf ("  desplazamiento ->  desplazamiento '->' suma_resta\n"); }
		| desplazamiento DESPI suma_resta { printf ("  desplazamiento ->  desplazamiento '<-' suma_resta\n"); }
		;

suma_resta
		: multi_div 
		| suma_resta '-' multi_div { printf ("  suma_resta ->  suma_resta '-' multi_div\n"); }
		| suma_resta '+' multi_div { printf ("  suma_resta ->  suma_resta '+' multi_div\n"); }
		;

multi_div
		: potencia
		| multi_div '*' potencia { printf ("  multi_div ->  multi_div '*' potencia\n"); }
		| multi_div '/' potencia { printf ("  multi_div ->  multi_div '/' potencia\n"); } 
		| multi_div RESTO potencia { printf ("  multi_div ->  multi_div '\\' potencia\n"); } 
		;

potencia
		: expresion_posfija
		| expresion_posfija POT potencia { printf ("  potencia ->  expr_posfija '**' potencia\n"); }
		;

expresion_posfija 
		: expresion_unaria
		| expresion_unaria operador_posfijo	{ printf ("  expresion posfija -> expr_unaria op_posfijo\n"); }
		;

operador_posfijo 
		: INC		{ printf ("  operador posfijo -> '++'\n"); } 
		| DEC		{ printf ("  operador posfijo -> '--'\n"); }
		;

expresion_unaria 		
		: primario			{ printf ("  expresion unaria -> primario\n"); }
		| '-' primario	{ printf ("  expresion unaria -> '-' primario\n"); }
		;

/******************************* PRIMARIOS ********************************/

primario              : '(' expresion ')'
                      | objeto llamada_subprograma
                      | llamada_subprograma
                      | objeto
                      | enumeracion
                      | literal
                      ;
objeto                : objeto '.' nombre_libreria
                      | objeto '[' expresiones ']'
                      | nombre_libreria
                      ;
ids                   : IDENTIFICADOR ',' ids
                      | IDENTIFICADOR
                      ;
literal               : VERDADERO
                      | FALSO
                      | CTC_CADENA
                      | CTC_CARACTER
                      | CTC_ENTERA
                      | CTC_REAL
                      ;
campos                : declaracion_variable campos
                      | declaracion_variable
                      ;

enumeracion           : '[' expresion_condicional clausulas_iteracion ']'
                      | '[' expresiones ']'
                      | '{' campos_enum '}'
                      | '{' claves_enum '}'
                      ;
elementos_enumeracion : elemento_enumeracion ',' elementos_enumeracion
                      | elemento_enumeracion
                      ;
elemento_enumeracion  : IDENTIFICADOR ASIGNACION expresion
                      | IDENTIFICADOR
                      ;
campos_enum           : campo_valor ',' campos_enum
                      | campo_valor
                      ;
claves_enum           : clave_valor ',' claves_enum
                      | clave_valor
                      ;
clave_valor           : CTC_CADENA FLECHA expresion
                      ;
campo_valor           : IDENTIFICADOR FLECHA expresion
                      ;

%%

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
