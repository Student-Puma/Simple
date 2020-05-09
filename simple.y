%{
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>

  int yyerror(char * s); 
  void reduction(char * a, char * b);
  
  extern FILE *yyin;
  extern int yylex();
  extern int yylineno;
  extern char * yytext;

  #define YYDEBUG 1

  /* Debug options */
  int yydebug = 0;          // Bison
  int enabledlogging = 1;   // Mensajes del lexer
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


%left ASIGNACION ASIG_SUMA ASIG_RESTA ASIG_MULT ASIG_DIV ASIG_RESTO ASIG_POT ASIG_DESPI ASIG_DESPD
%left '+' '-'
%left '*' '/'
%right UMINUS

%%

/******************************** PROGRAMA ********************************/

programa              : definicion_programa                                                         { fprintf(stdout, "EXITO :: programa -> definicion_programa\n"); }
                      | definicion_libreria                                                         { fprintf(stdout, "EXITO :: programa -> definicion_libreria\n"); }
                      ;
definicion_programa   : PROGRAMA IDENTIFICADOR ';' codigo_programa                                  { reduction("definicion_programa", "PROGRAMA ID ; codigo_programa"); }
                      ;
codigo_programa       : importar cuerpo_subprograma                                                 { reduction("codigo_programa", "importar cuerpo_subprograma"); }
                      | cuerpo_subprograma                                                          { reduction("codigo_programa", "cuerpo_subprograma"); }
                      ;
definicion_libreria   : LIBRERIA IDENTIFICADOR ';' codigo_libreria                                  { reduction("definicion_libreria", "LIBRERIA ID ; codigo_libreria"); }
                      ;
codigo_libreria       : imexportar declaraciones                                                    { reduction("codigo_libreria", "importar|exportar declaraciones"); }
                      ;
declaraciones         : declaracion declaraciones                                                   { /* reduction("declaraciones", "declaracion declaraciones"); */ }
                      | declaracion                                                                 { /* reduction("declaraciones", "declaracion"); */ }
                      ;
declaracionesop       : declaraciones                                                               { /* reduction("declaracionesop", "declaraciones"); */ }
                      | /* opcional */                                                              { /* reduction("declaracionesop", "< vacio >"); */ }
                      ;
declaracion           : declaracion_objeto                                                          { reduction("declaracion", "declaracion_objeto"); }
                      | declaracion_tipo                                                            { reduction("declaracion", "declaracion_tipo"); }
                      | declaracion_sprograma                                                       { reduction("declaracion", "declaracion_subprograma"); }
                      ;
imexportar            : importar exportar                                                           { /* reduction("importar|exportar", "importar exportar"); */ }
                      | importar                                                                    { /* reduction("importar|exportar", "importar"); */ }
                      | exportar                                                                    { /* reduction("importar|exportar", "exportar"); */ }
                      | /* vacio */                                                                 { /* reduction("importar|exportar", "< vacio >"); */ }
                      ;
exportar              : EXPORTAR nombre_librerias ';'                                               { reduction("exportar", "EXPORTAR nombres ;"); }
                      ;
importar              : importar libreria ';'                                                       { /* reduction("importar", "importar libreria ;"); */ }
                      | libreria ';'                                                                { /* reduction("importar", "libreria ;"); */ }
                      ;
libreria              : DE LIBRERIA nombre_libreria IMPORTAR nombre_librerias                       { reduction("libreria", "DE LIBRERIA nombre_libreria IMPORTAR nombres"); }
                      | IMPORTAR LIBRERIA nombre_libreria COMO IDENTIFICADOR                        { reduction("libreria", "IMPORTAR LIBRERIA nombre_libreria COMO ID"); }
                      | IMPORTAR LIBRERIA nombre_libreria                                           { reduction("libreria", "IMPORTAR LIBRERIA nombre_libreria"); }
                      ;
nombre_librerias      : nombre_libreria ',' nombre_librerias                                        { /* reduction("nombres", "nombre , nombres"); */ }
                      | nombre_libreria                                                             { /* reduction("nombres", "nombre"); */ }
                      ;
nombre_libreria       : IDENTIFICADOR CUATRO_PUNTOS nombre_libreria                                 { /* reduction("nombre", "ID :: nombre"); */ }
                      | IDENTIFICADOR                                                               { /* reduction("nombre", "ID"); */ }
                      ;

/************************* DECLARACIÓN DE OBJETOS *************************/

declaracion_objeto    : declaracion_constante                                                       { reduction("declaracion_objeto", "declaracion_constante"); }
                      | declaracion_variable                                                        { reduction("declaracion_objeto", "declaracion_variable"); }
                      ;
declaracion_constante : ids ':' CONSTANTE especificacion_tipo ASIGNACION expresion ';'              { reduction("declaracion_constante", "ID : CONSTANTE especificacion_tipo ASIGNACION expresion ;"); }
                      ;
declaracion_variable  : ids ':' especificacion_tipo op_asignacion expresion ';'                     { reduction("declaracion_variable", "ID : especificacion_tipo ASIGNACION expresion ;"); }
                      | ids ':' especificacion_tipo ';'                                             { reduction("declaracion_variable", "ID : especificacion_tipo ;"); }
                      ;

/************************** DECLARACIÓN DE TIPOS **************************/

declaracion_tipo      : TIPO IDENTIFICADOR ES tipo_no_estructurado ';'                              { reduction("declaracion_tipo", "TIPO ID ES tipo_no_estructurado ;"); }
                      | TIPO IDENTIFICADOR ES tipo_estructurado                                     { reduction("declaracion_tipo", "TIPO ID ES tipo_estructurado"); }
                      ;

/************************ ESPECIFICACIÓN DE TIPOS *************************/

especificacion_tipo   : nombre_libreria                                                             { reduction("especificacion_tipo", "nombre"); }
                      | tipo_no_estructurado                                                        { reduction("especificacion_tipo", "tipo_no_estructurado"); }
                      ;
tipo_estructurado     : tipo_registro                                                               { reduction("tipo_estructurado", "tipo_registro"); }
                      | tipo_enumerado                                                              { reduction("tipo_estructurado", "tipo_enumerado"); }
                      | clase                                                                       { reduction("tipo_estructurado", "clase"); }
                      ;
tipo_registro         : REGISTRO campos FIN REGISTRO                                                { reduction("tipo_registro", "REGISTRO campos FIN REGISTRO"); }
                      ;
tipo_enumerado        : ENUMERACION DE tipo_escalar elementos_enumeracion FIN ENUMERACION           { reduction("tipo_enumerado", "ENUMERACION DE tipo_escalar elementos_enumeracion FIN ENUMERACION"); }
                      | ENUMERACION elementos_enumeracion FIN ENUMERACION                           { reduction("tipo_enumerado", "ENUMERACION elementos_enumeracion FIN ENUMERACION"); }
                      ;
tipo_no_estructurado  : tipo_escalar                                                                { reduction("tipo_no_estructurado", "tipo_escalar"); }
                      | tipo_tabla                                                                  { reduction("tipo_no_estructurado", "tipo_tabla"); }
                      | tipo_diccionario                                                            { reduction("tipo_no_estructurado", "tipo_diccionario"); }
                      ;
tipo_escalar          : SIGNO tipo_basico longitud rango                                            { reduction("tipo_escalar", "SIGNO tipo_basico longitud rango"); }
                      | SIGNO tipo_basico longitud                                                  { reduction("tipo_escalar", "SIGNO tipo_basico longitud"); }
                      | SIGNO tipo_basico rango                                                     { reduction("tipo_escalar", "SIGNO tipo_basico rango"); }
                      | SIGNO tipo_basico                                                           { reduction("tipo_escalar", "SIGNO tipo_basico"); }
                      | tipo_basico longitud rango                                                  { reduction("tipo_escalar", "tipo_basico longitud rango"); }
                      | tipo_basico longitud                                                        { reduction("tipo_escalar", "tipo_basico longitud"); }
                      | tipo_basico rango                                                           { reduction("tipo_escalar", "tipo_basico rango"); }
                      | tipo_basico                                                                 { reduction("tipo_escalar", "tipo_basico"); }
                      ;
tipo_tabla            : TABLA '(' expresion DOS_PUNTOS expresion ')' DE especificacion_tipo         { reduction("tipo_tabla", "TABLA ( expresion .. expresion ) DE especificacion_tipo"); }
                      | LISTA DE especificacion_tipo                                                { reduction("tipo_tabla", "LISTA DE especificacion_tipo"); }
                      ;
tipo_diccionario      : DICCIONARIO DE especificacion_tipo                                          { reduction("tipo_diccionario", "DICCIONARIO DE especificacion_tipo"); }
                      ;
tipo_basico           : BOOLEANO                                                                    { reduction("tipo_basico", "BOOLEANO"); }
                      | CARACTER                                                                    { reduction("tipo_basico", "CARACTER"); }
                      | ENTERO                                                                      { reduction("tipo_basico", "ENTERO"); }
                      | REAL                                                                        { reduction("tipo_basico", "REAL"); }
                      ;
longitud              : CORTO                                                                       { reduction("longitud", "CORTO"); }
                      | LARGO                                                                       { reduction("longitud", "LARGO"); }
                      ;
rango                 : RANGO expresion DOS_PUNTOS expresion DOS_PUNTOS expresion                   { reduction("rango", "RANGO expresion .. expresion .. expresion"); }
                      | RANGO expresion DOS_PUNTOS expresion                                        { reduction("rango", "RANGO expresion .. expresion"); }
                      ;

/********************************* CLASES *********************************/

clase                 : CLASE ULTIMA superclases decl_componentes FIN CLASE                         { reduction("clase", "CLASE ULTIMA superclases? decl_componentes FIN CLASE"); }
                      | CLASE superclases decl_componentes FIN CLASE                                { reduction("clase", "CLASE superclases? decl_componentes FIN CLASE"); }
                      ;
superclases           : '(' ids ')'                                                                 { /* reduction("superclases", "( IDs )"); */ }
                      | /* opcional */                                                              { /* reduction("superclases", "< vacio >"); */ }
                      ;
decl_componentes      : decl_componente decl_componentes                                            { /* reduction("decl_componentes", "decl_componente decl_componentes"); */ }
                      | decl_componente                                                             { /* reduction("decl_componentes", "decl_componente"); */ }
                      ;
decl_componente       : visibilidad componente                                                      { reduction("decl_componente", "visibilidad? componente"); }
                      ;
componente            : declaracion_tipo                                                            { reduction("componente", "declaracion_tipo"); }
                      | declaracion_objeto                                                          { reduction("componente", "declaracion_objeto"); }
                      | modificadores declaracion_sprograma                                         { reduction("componente", "modificadores declaracion_subprograma"); }
                      | declaracion_sprograma                                                       { reduction("componente", "declaracion_subprograma"); }
                      ;
visibilidad           : PUBLICO                                                                     { reduction("visibilidad", "PUBLICO"); }
                      | PROTEGIDO                                                                   { reduction("visibilidad", "PROTEGIDO"); }
                      | PRIVADO                                                                     { reduction("visibilidad", "PRIVADO"); }
                      | /* opcional */                                                              { /* reduction("visibilidad", "< vacio >"); */ }
                      ;
modificadores         : modificador modificadores                                                   { /* reduction("modificadores", "modificador modificadores"); */ }
                      | modificador                                                                 { /* reduction("modificadores", "modificador"); */ }
                      ;
modificador           : CONSTRUCTOR                                                                 { reduction("modificador", "CONSTRUCTOR"); }
                      | DESTRUCTOR                                                                  { reduction("modificador", "DESTRUCTOR"); }
                      | GENERICO                                                                    { reduction("modificador", "GENERICO"); }
                      | ABSTRACTO                                                                   { reduction("modificador", "ABSTRACTO"); }
                      | ESPECIFICO                                                                  { reduction("modificador", "ESPECIFICO"); }
                      | FINAL                                                                       { reduction("modificador", "FINAL"); }
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
instr_asignacion      : objeto op_asignacion expresion ';' /* FIXME: Cualquier asignacion */
                      ;
op_asignacion         : ASIGNACION | ASIG_SUMA | ASIG_RESTA | ASIG_MULT | ASIG_DIV | ASIG_RESTO | ASIG_POT | ASIG_DESPI | ASIG_DESPD
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

expresion             : and_logico
		              | expresion OR and_logico		 
		              ;
and_logico            : negacion
		              | and_logico AND negacion		 
		              ;
negacion              : comparacion
		              | '~' comparacion 
		              ;
comparacion           : desplazamiento
		              | desplazamiento '>' desplazamiento 
		              | desplazamiento '<' desplazamiento 
		              | desplazamiento '=' desplazamiento 
		              | desplazamiento LEQ desplazamiento 
		              | desplazamiento GEQ desplazamiento 
		              | desplazamiento NEQ desplazamiento 
		              ;
desplazamiento        :	suma_resta
		              | desplazamiento DESPD suma_resta 
		              | desplazamiento DESPI suma_resta 
		              ;
suma_resta            : multi_div 
		              | suma_resta '-' multi_div 
		              | suma_resta '+' multi_div 
		              ;
multi_div             : potencia
		              | multi_div '*' potencia 
		              | multi_div '/' potencia  
		              | multi_div RESTO potencia  
		              ;
potencia              : expresion_posfija
		              | expresion_posfija POT potencia 
		              ;
expresion_posfija     : expresion_unaria
		              | expresion_unaria operador_posfijo	
		              ;
operador_posfijo      : INC		 
		              | DEC		
		              ;
expresion_unaria      : primario			
		              | '-' primario %prec UMINUS	
		              ;

/******************************* PRIMARIOS ********************************/

primario              : '(' expresion ')'
                   /* | objeto  FIXME: 1s/r conflict */ 
                      | objeto
                      | objeto llamada_subprograma
                      | llamada_subprograma
                      | enumeracion
                      | literal
                      ;
objeto                : objeto '.' nombre_libreria
                      | objeto '[' expresiones ']'
                      | objeto '{' cadenas '}'
                      | nombre_libreria
                      ;
ids                   : IDENTIFICADOR ',' ids
                      | IDENTIFICADOR
                      ;
cadenas               : CTC_CADENA ',' cadenas
                      | CTC_CADENA
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

void reduction(char * a, char * b) {
  fflush(stdout);
  fprintf(stdout, "\t%s -> %s\n", a, b);
}

int yyerror (char *msg) {
  fflush(stderr);
  fprintf(stderr, "***************** Error en la línea %d cerca de '%s': %s\n", yylineno, yytext, msg);
}

int yywrap() {
    return 1;
}

int main(int argc, char *argv[]) {
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
