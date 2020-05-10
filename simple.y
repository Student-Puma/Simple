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

%token CUATRO_PUNTOS FLECHA DOS_PUNTOS
%token INC DEC DESPI DESPD LEQ GEQ NEQ AND OR

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


%left ','
%right ASIGNACION ASIG_SUMA ASIG_RESTA ASIG_MULT ASIG_DIV ASIG_RESTO ASIG_POT ASIG_DESPI ASIG_DESPD

%left OR AND
%left '<' '>' '=' LEQ GEQ NEQ
%left DESPD DESPI
%left '+' '-'
%left '*' '/' RESTO
%left INC DEC
%right POT
%right '~'

%nonassoc UMINUS

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

declaracion_sprograma : SUBPROGRAMA cabecera_subprograma cuerpo_subprograma SUBPROGRAMA             { reduction("declaracion_subprograma", "SUBPROGRAMA cabecera_subprograma cuerpo_subprograma SUBPROGRAMA"); }
                      ;
cabecera_subprograma  : IDENTIFICADOR parametrizacion tipo_resultado                                { reduction("cabecera_subprograma", "ID paramtros? tipo_resultado"); }
                      | IDENTIFICADOR parametrizacion                                               { reduction("cabecera_subprograma", "ID paramtros?"); }
                      ;
cuerpo_subprograma    : declaracionesop PRINCIPIO instrucciones FIN                                 { reduction("cuerpo_subprograma", "declaraciones? PRINCIPIO instrucciones FIN"); }
                      ;
parametrizacion       : '(' declaracion_parametrs ')'                                               { /* reduction("parametrizacion", "( declaracion_parametrs )"); */ }
                      | /* opcional */                                                              { /* reduction("parametrizacion", "< vacio >"); */ }
                      ;
definicion_parametros : definicion_parametro ',' definicion_parametros                              { /* reduction("definicion_parametros", "definicion_parametro , definicion_parametros"); */ }
                      | definicion_parametro                                                        { /* reduction("definicion_parametros", "definicion_parametros"); */ }
                      ;
definicion_parametro  : IDENTIFICADOR ASIGNACION expresion                                          { reduction("definicion_parametro", "ID ASIGNACION expresion"); }
                      | expresion                                                                   { reduction("definicion_parametro", "expresion"); }
                      ;
declaracion_parametrs : paramtros ';' declaracion_parametrs                                         { /* reduction("declaracion_parametrs", "paramtros ; declaracion_parametrs"); */ }
                      | paramtros                                                                   { /* reduction("declaracion_parametrs", "paramtros"); */ }
                      ;
paramtros             : ids ':' modo especificacion_tipo ASIGNACION expresion                       { reduction("paramtros", "ID : modo? especificacion_tipo ASIGNACION expresion"); }
                      | ids ':' modo especificacion_tipo                                            { reduction("paramtros", "ID : modo? especificacion_tipo"); }
                      ;
modo                  : VALOR                                                                       { reduction("modo", "VALOR"); }
                      | REFERENCIA                                                                  { reduction("modo", "REFERENCIA"); }
                      | /* opcional */                                                              { /* reduction("modo", "< vacio >"); */ }
                      ;
tipo_resultado        : DEVOLVER especificacion_tipo                                                { reduction("tipo_resultado", "DEVOLVER especificacion_tipo"); }
                      ;
llamada_subprograma   : nombre_libreria '(' definicion_parametros ')'                               { reduction("llamada_subprograma", "nombre ( definicion_parametros )"); }
                      | nombre_libreria '(' ')'                                                     { reduction("llamada_subprograma", "nombre ( )"); }
                      ;

/***************************** INSTRUCCIONES ******************************/

instrucciones         : instruccion instrucciones                                                   { reduction("instrucciones", "instruccion instrucciones"); }
                      | instruccion                                                                 { reduction("instrucciones", "instruccion"); }
                      ;
instruccion           : instr_asignacion                                                            { reduction("instruccion", "instr_asignacion"); }
                      | instr_bucle                                                                 { reduction("instruccion", "instr_bucle"); }
                      | instr_capturar                                                              { reduction("instruccion", "instr_capturar"); }
                      | instr_casos                                                                 { reduction("instruccion", "instr_casos"); }
                      | instr_devolver                                                              { reduction("instruccion", "instr_devolver"); }
                      | instr_interrupcion                                                          { reduction("instruccion", "instr_interrupcion"); }
                      | instr_lanzar                                                                { reduction("instruccion", "instr_lanzar"); }
                      | instr_llamada                                                               { reduction("instruccion", "instr_llamada"); }
                      | instr_si                                                                    { reduction("instruccion", "instr_si"); }
                      | instr_vacia                                                                 { reduction("instruccion", "instr_vacia"); }
                      ;
instr_llamada         : llamada_subprograma ';'                                                     { reduction("instr_llamada", "llamada_subprograma ;"); }
                      ;
instr_asignacion      : objeto op_asignacion expresion ';'                                          { reduction("instr_asignacion", "objeto ASIGNACION expresion ;"); }
                      ;
op_asignacion         : ASIGNACION
                      | ASIG_SUMA | ASIG_RESTA | ASIG_MULT | ASIG_DIV
                      | ASIG_RESTO | ASIG_POT | ASIG_DESPI | ASIG_DESPD
                      ;
instr_lanzar          : LANZA nombre_libreria ';'                                                   { reduction("instr_lanzar", "LANZA nombre ;"); }
                      ;
instr_devolver        : DEVOLVER expresion ';'                                                      { reduction("instr_devolver", "DEVOLVER expresion ;"); }
                      | DEVOLVER ';'                                                                { reduction("instr_devolver", "DEVOLVER ;"); }
                      ;
instr_interrupcion    : SIGUIENTE CUANDO expresion                                                  { reduction("instr_interrupcion", "SIGUIENTE CUANDO expresion"); }
                      | SIGUIENTE                                                                   { reduction("instr_interrupcion", "SIGUIENTE"); }
                      | SALIR DE IDENTIFICADOR CUANDO expresion                                     { reduction("instr_interrupcion", "SALIR DE ID CUANDO expresion"); }
                      | SALIR DE IDENTIFICADOR                                                      { reduction("instr_interrupcion", "SALIR DE ID"); }
                      | SALIR CUANDO expresion                                                      { reduction("instr_interrupcion", "SALIR CUANDO expresion"); }
                      ;
instr_vacia           : ';'                                                                         { reduction("instr_vacia", ";"); }
                      ;

/************************* BUCLES Y CONDICIONALES *************************/

instr_casos           : CASOS expresion ES casos FIN CASOS                                          { reduction("instr_casos", "CASOS expresion ES casos FIN CASOS"); }
                      ;
casos                 : caso casos                                                                  { /* reduction("casos", "caso casos"); */ }
                      | caso                                                                        { /* reduction("casos", "caso"); */ }
                      ;
caso                  : CUANDO entradas FLECHA instrucciones                                        { reduction("caso", "CUANDO entradas => instrucciones"); }
                      ;
entradas              : entradas ':' entrada                                                        { /* reduction("entradas", "entradas : entrada"); */ }
                      | entrada                                                                     { /* reduction("entradas", "entrada"); */ }
                      ;
entrada               : expresion DOS_PUNTOS expresion                                              { reduction("entrada", "expresion .. expresion"); }
                      | expresion                                                                   { reduction("entrada", "expresion"); }
                      | OTRO                                                                        { reduction("entrada", "OTRO"); }
                      ;
instr_si              : SI expresion ENTONCES instrucciones SINO instrucciones FIN SI               { reduction("instr_si", "SI expresion ENTONCES instrucciones SINO instrucciones FIN SI"); }
                      | SI expresion ENTONCES instrucciones FIN SI                                  { reduction("instr_si", "SI expresion ENTONCES instruccionesFIN SI"); }
                      ;
instr_bucle           : IDENTIFICADOR ':' clausula_iteracion instrucciones FIN BUCLE                { reduction("instr_bucle", "ID ':' clausula_iteracion instrucciones FIN BUCLE"); }
                      | clausula_iteracion instrucciones FIN BUCLE                                  { reduction("instr_bucle", "clausula_iteracion instrucciones FIN BUCLE"); }
                      ;
clausulas_iteracion   : clausula_iteracion clausulas_iteracion                                      { /* reduction("clausulas_iteracion", "clausula_iteracion clausulas_iteracion"); */ }
                      | clausula_iteracion                                                          { /* reduction("clausulas_iteracion", "clausula_iteracion"); */ }
                      ;
clausula_iteracion    : PARA IDENTIFICADOR ':' especificacion_tipo EN expresion                     { reduction("clausula_iteracion", "PARA ID : especificacion_tipo EN expresion"); }
                      | PARA IDENTIFICADOR EN expresion                                             { reduction("clausula_iteracion", "PARA ID EN expresion"); }
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN expresion DOS_PUNTOS expresion DOS_PUNTOS expresion DESCENDENTE
                                                                                                    { reduction("clausula_iteracion", "REPETIR ID : especificacion_tipo EN rango DESCENDENTE"); }
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN expresion DOS_PUNTOS expresion DOS_PUNTOS expresion
                                                                                                    { reduction("clausula_iteracion", "REPETIR ID : especificacion_tipo EN rango"); }
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN expresion DOS_PUNTOS expresion DESCENDENTE
                                                                                                    { reduction("clausula_iteracion", "REPETIR ID : especificacion_tipo EN rango DESCENDENTE"); }
                      | REPETIR IDENTIFICADOR ':' especificacion_tipo EN expresion DOS_PUNTOS expresion
                                                                                                    { reduction("clausula_iteracion", "REPETIR ID : especificacion_tipo EN rango"); }
                      | REPETIR IDENTIFICADOR EN expresion DOS_PUNTOS expresion DESCENDENTE         { reduction("clausula_iteracion", "REPETIR ID EN rango DESCENDENTE"); }
                      | REPETIR IDENTIFICADOR EN expresion DOS_PUNTOS expresion                     { reduction("clausula_iteracion", "REPETIR ID EN rango"); }
                      | MIENTRAS expresion                                                          { reduction("clausula_iteracion", "MIENTRAS expresion"); }
                      ;

/******************************** PRUEBAS *********************************/

instr_capturar        : PRUEBA instrucciones clausulas FIN PRUEBA                                   { reduction("instr_capturar", "PRUEBA instrucciones clausulas FIN PRUEBA"); }
                      ;
clausulas             : clausulas_excepcion clausula_finalmente                                     { reduction("clausulas", "clausulas_excepcion clausula_finalmente"); }
                      | clausulas_excepcion                                                         { reduction("clausulas", "clausulas_excepcion"); }
                      | clausula_finalmente                                                         { reduction("clausulas", "clausula_finalmente"); }
                      ;
clausulas_excepcion   : clausulas_especificas                                                       { /* reduction("clausulas_excepcion", "clausulas_especificas"); */ }
                      ;
clausulas_especificas : clausula_excepcion clausulas_especificas                                    { /* reduction("clausulas_especificas", "clausula_excepcion clausulas_especificas"); */ }
                      | clausula_excepcion                                                          { /* reduction("clausulas_especificas", "clausula_excepcion"); */ }
                      ;
clausula_excepcion    : EXCEPCION '(' nombre_libreria ')' instrucciones                             { reduction("clausula_excepcion", "EXCEPCION ( nombre ) instrucciones"); }
                      | EXCEPCION instrucciones                                                     { reduction("clausula_excepcion", "EXCEPCION instrucciones"); } /* FIXME: Exc. General aparte */               
                      ;
clausula_finalmente   : FINALMENTE instrucciones                                                    { reduction("clausula_finalmente", "FINALMENTE instrucciones"); }
                      ;

/****************************** EXPRESIONES *******************************/

expresiones           : expresion ',' expresiones                                                   { /* reduction("expresiones", "expresion , expresiones"); */ }
                      | expresion                                                                   { /* reduction("expresiones", "expresiones"); */ }
                      ;
expresion_condicional : SI expresion ENTONCES expresion SINO expresion                              { reduction("expresion_condicional", "SI expresion ENTONCES expresion SINO expresion"); }
                      | SI expresion ENTONCES expresion                                             { reduction("expresion_condicional", "SI expresion ENTONCES expresion"); }
                      ;

/* Resolución de conflictos de shift/reduce y reduce/reduce */

expresion             : or_logico                                                                   { ; }
                      ;
or_logico             : and_logico                                                                  { ; }
                      | expresion OR and_logico		                                                  { reduction("expresion", "expresion \\/ expresion"); }
                      ;
and_logico            : negacion                                                                    { ; }
                      | and_logico AND negacion		                                                  { reduction("expresion", "expresion /\\ expresion"); }
                      ;
negacion              : comparacion                                                                 { ; }
                      | '~' comparacion                                                             { reduction("expresion", "~ expresion"); }
                      ;
comparacion           : desplazamiento                                                              { ; }
                      | desplazamiento '>' desplazamiento                                           { reduction("expresion", "expresion > expresion"); }
                      | desplazamiento '<' desplazamiento                                           { reduction("expresion", "expresion < expresion"); }
                      | desplazamiento '=' desplazamiento                                           { reduction("expresion", "expresion = expresion"); }
                      | desplazamiento LEQ desplazamiento                                           { reduction("expresion", "expresion <= expresion"); }
                      | desplazamiento GEQ desplazamiento                                           { reduction("expresion", "expresion >= expresion"); }
                      | desplazamiento NEQ desplazamiento                                           { reduction("expresion", "expresion ~= expresion"); }
                      ;
desplazamiento        :	suma_resta                                                                  { ; }
                      | desplazamiento DESPD suma_resta                                             { reduction("expresion", "expresion -> expresion"); }
                      | desplazamiento DESPI suma_resta                                             { reduction("expresion", "expresion <- expresion"); }
                      ;
suma_resta            : multi_div                                                                   { ; }
                      | suma_resta '-' multi_div                                                    { reduction("expresion", "expresion - expresion"); }
                      | suma_resta '+' multi_div                                                    { reduction("expresion", "expresion + expresion"); }
                      ;
multi_div             : potencia                                                                    { ; }
                      | multi_div '*' potencia                                                      { reduction("expresion", "expresion * expresion"); }
                      | multi_div '/' potencia                                                      { reduction("expresion", "expresion / expresion"); }
                      | multi_div RESTO potencia                                                    { reduction("expresion", "expresion \\ expresion"); }
                      ;
potencia              : expresion_posfija                                                           { ; }
                      | expresion_posfija POT potencia                                              { reduction("expresion", "expresion ^ expresion"); }
                      ;
expresion_posfija     : expresion_unaria                                                            { ; }
                      | expresion_unaria INC                                                        { reduction("expresion", "expresion ++"); }
                      | expresion_unaria DEC                                                        { reduction("expresion", "expresion --"); }
                      ;
expresion_unaria      : primario	                                                                  { reduction("expresion", "primario"); }		
                      | '-' primario %prec UMINUS	                                                  { reduction("expresion", "- primario"); }
                      ;

/******************************* PRIMARIOS ********************************/

primario              : '(' expresion ')'                                                           { reduction("primario", "( expresion )"); }
                      | objeto llamada_subprograma                                                  { reduction("primario", "objeto llamada_subprograma"); }
                      | llamada_subprograma                                                         { reduction("primario", "llamada_subprograma"); }
                      | enumeracion                                                                 { reduction("primario", "enumeracion"); }
                      | literal                                                                     { reduction("primario", "literal"); }
                      | objeto                                                                      { reduction("primario", "objeto"); } /* FIXME: Exc. General aparte */   
                      ;
objeto                : objeto '.' nombre_libreria                                                  { reduction("objeto", "objeto . nombre"); }
                      | objeto '[' expresiones ']'                                                  { reduction("objeto", "objeto [ expresiones ]"); }
                      | objeto '{' cadenas '}'                                                      { reduction("objeto", "objeto { CTC_CADENA }"); }
                      | nombre_libreria                                                             { reduction("objeto", "nombre"); }
                      ;
ids                   : IDENTIFICADOR ',' ids                                                       { /* reduction("ids", "ID , ids"); */ }
                      | IDENTIFICADOR                                                               { /* reduction("ids", "ID"); */ }
                      ;
cadenas               : CTC_CADENA ',' cadenas                                                      { /* reduction("cadenas", "CTC_CADENA , cadenas"); */ }
                      | CTC_CADENA                                                                  { /* reduction("cadenas", "CTC_CADENA"); */ }
                      ;
literal               : VERDADERO                                                                   { reduction("literal", "VERDADERO"); }
                      | FALSO                                                                       { reduction("literal", "FALSO"); }
                      | CTC_CADENA                                                                  { reduction("literal", "CTC_CADENA"); }
                      | CTC_CARACTER                                                                { reduction("literal", "CTC_CARACTER"); }
                      | CTC_ENTERA                                                                  { reduction("literal", "CTC_ENTERA"); }
                      | CTC_REAL                                                                    { reduction("literal", "CTC_REAL"); }
                      ;
campos                : declaracion_variable campos                                             { /* reduction("campos", "declaracion_variable ; campos"); */ }
                      | declaracion_variable                                                       { /* reduction("campos", "declaracion_variable"); */ }
                      ;

enumeracion           : '[' expresion_condicional clausulas_iteracion ']'                           { reduction("enumeracion", "[ expresion_condicional clausulas_iteracion ]"); }
                      | '[' expresiones ']'                                                         { reduction("enumeracion", "[ expresiones ]"); }
                      | '{' campos_enum '}'                                                         { reduction("enumeracion", "{ campos }"); }
                      | '{' claves_enum '}'                                                         { reduction("enumeracion", "{ claves }"); }
                      ;
elementos_enumeracion : elemento_enumeracion ',' elementos_enumeracion                              { /* reduction("elementos_enumeracion", "elemento_enumeracion , elementos_enumeracion"); */ }
                      | elemento_enumeracion                                                        { /* reduction("elementos_enumeracion", "elemento_enumeracion"); */ }
                      ;
elemento_enumeracion  : IDENTIFICADOR ASIGNACION expresion                                          { reduction("elemento_enumeracion", "ID ASIGNACION expresion"); }
                      | IDENTIFICADOR                                                               { reduction("elemento_enumeracion", "ID"); }
                      ;
campos_enum           : campo_valor ',' campos_enum                                                 { /* reduction("campos_enum", "campo_valor , campos_enum"); */ }
                      | campo_valor                                                                 { /* reduction("campos_enum", "campo_valor"); */ }
                      ;
claves_enum           : clave_valor ',' claves_enum                                                 { /* reduction("claves_enum", "clave_valor , claves_enum"); */ }
                      | clave_valor                                                                 { /* reduction("claves_enum", "clave_valor"); */ }
                      ;
clave_valor           : CTC_CADENA FLECHA expresion                                                 { reduction("clave_valor", "CTC_CADENA => expresion"); }
                      ;
campo_valor           : IDENTIFICADOR FLECHA expresion                                              { reduction("campo_valor", "ID => expresion"); }
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