opcion1 macro 
	limpiarBuffer bufferLectura
	limpiarBuffer nombrePadre
	limpiarBuffer nombreReporte
	limpiarBuffer namePadre
	getRuta rutaArchivo
	openF rutaArchivo, handleFichero
	leerF SIZEOF bufferLectura, bufferLectura, handleFichero
	closeF handleFichero  	;print bufferLectura
	leyendoJSON bufferLectura
	print msgArchivoLeido ; archivo leido con exito
endm 

print macro cadena
		MOV ah, 09h
		MOV dx,offset cadena
		INT 21h
endm 

print2 macro cadena
		MOV ah, 09h
		MOV dx, cadena
		INT 21h
endm 




leyendoJSON  macro buffer
		LOCAL SALIR, BuscarID, guardarID, guardarPadre, ObtenerNumero, BuscarNumero , Operacion, Multiplicacion 
		LOCAL IngresarID, CONTINUE, EndNumero, abrirLlave, cerrarLlave, FinOperacion, Division, Suma, Resta
		LOCAL operarFIN, guardaIDoperacion, Csuma, Cresta, Cmultiplicacion, Cdivision, guardarOperaciones 
		LOCAL numeroNegativo
		limpiarBuffer bufferOperaciones
		limpiarBuffer nombrePadre
		limpiarBuffer nombreReporte
		limpiarBuffer bufferAux
		limpiarBuffer nombreOperacion
		limpiarBuffer operaciones 
		limpiarBuffer bufferMediaR
		limpiarBuffer bufferMedianaR
		limpiarBuffer bufferMayorR
		limpiarBuffer bufferMenorR
		limpiarBuffer bufferModaR

		XOR si, si 
		XOR cx, cx 
		XOR ax, ax 
		XOR bx, bx 
		XOR dx, dx 
		MOV contadorPadre, 0
		MOV finOpe, 0
		MOV contadorLLaves, 0
		MOV contadorNumero, 0
		MOV totalOperaciones, 0
		MOV contadorguardar, 0

		CICLO:
			MOV dh, buffer[si]

			CMP dh, 22h ;  "
			JE  BuscarID 
			CMP dh, 7Bh ; {
			JE abrirLlave 
			CMP dh, 7Dh ; }
			JE cerrarLlave
			JMP CONTINUE

		CONTINUE:		
			CMP dh, 24h ; $ 
			JE SALIR 
			INC si 
			JMP CICLO
			

		abrirLlave:
		;print tllavea
		;print saltoLinea
			INC si 
			CMP contadorPadre, 0
			JE CICLO
			ADD contadorLLaves, 1 
			JMP CICLO

		cerrarLlave:
		;print llave1
		;print saltoLinea
			SUB contadorLLaves, 1
			CMP contadorLLaves, 2
			JE FinOperacion 
			
			INC si 
			JMP CICLO

		FinOperacion:
			MOV finOpe, 0
			;print msgResultado 
			;print nombreOperacion 
			;print espacio 
			; este es el resultado de la operacion 
			JMP guardarOperaciones
			
		
		guardarOperaciones:
			XOR ax, ax 
			POP ax 
			MOV auxiliar, 0
			MOV auxiliar, ax 
			; guardar ID de la operacion 

			limpiarBuffer bufferAux
			ConvertirString bufferAux
			;print bufferAux
			;print saltoLinea
			;print padre  ;nose --------------
			llenarOperacionesR bufferOperaciones, inicioOR 
			llenarOperacionesR bufferOperaciones, nombreOperacion
			llenarOperacionesR bufferOperaciones, cierreComillas2
			llenarOperacionesR bufferOperaciones, bufferAux
			llenarOperacionesR pivote, bufferAux
			llenarOperacionesR bufferOperaciones, finOR 
			llenarOperacionesR bufferOperaciones, coma2
			ADD totalOperaciones, 1
			ingresarOperaciones operaciones

			MOV ax, auxiliar 
			PUSH ax 
			
			XOR ax, ax 
			XOR cx, cx 
			INC si 
			JMP validar_guardado
			;JMP CICLO
		
		validar_guardado:
			ADD contadorguardar, 1

			CMP contadorguardar, 1
			JE base1 
			CMP contadorguardar, 2
			JE base2
			CMP contadorguardar, 3
			JE base3
			CMP contadorguardar, 4
			JE base4
			CMP contadorguardar, 5
			JE base5

			JMP CICLO










.model small
.stack 100h
.data 
; -------------------- SEGMENTO DE DATOS ---------------------------
encabezado db  0ah, 0dh, ' ============================================================================',
			   0ah, 0dh, '  UNIVERSIDAD DE SAN CARLOS DE GUATEMALA', 
			   0ah, 0dh, '  Facultad de Ingenieria',
			   0ah, 0dh, '  Ciencias y Sistemas', 
			   0ah, 0dh, '  Arquitectura de Computadores y Ensambladores 1',
			   0ah, 0dh, '  Nombre: John Henry Lopez Mijangos',
			   0ah, 0dh, '  Carne: 201710392', 
			   0ah, 0dh, '  Seccion: A', 
			   0ah, '$'
menu db 0ah, 0dh, ' ============================================================================', 
		0ah, 0dh, '  1.  CARGAR ARCHIVO', 
		0ah, 0dh, '  2.  CONSOLA', 
		0ah, 0dh, '  3.  SALIR',
		0ah, 0dh, ' ============================================================================',
		0ah, 0dh, ' Ingrese Opcion:','  $'

encCargarArchivo db 0ah, 0dh, ' ============================= CARGAR ARCHIVO =============================== ', ' $'
encConsola db 0ah, 0dh, ' ===============================  CONSOLA   ================================= ','$'
inicioConsole db 0ah, 0dh, ' >>', ' $'
espacio db 20h, '$'

saltoLinea db 0ah, 0dh,  '$'
finObjeto db 0
comandoConsola db 20 dup('$')
comandoConsola2 db 30 dup('$') 
showMedia db 'media', '$' 

llave1 db '123', '$' 
padre db 'operaciones', '$' 
corchete db 'media', '$' 
llave2 db 'media', '$' 
hijo db 'media', '$' 
llave3 db 'media', '$' 
operacion db 'media', '$' 

showMediana db 'mediana','$'
showModa db 'moda','$'
showMenor db 'menor','$'
showMayor db 'mayor','$'
showMayor2 db 'mayor','$'
media dw 0
mediana dw 0
moda dw 0
menor dw 0
mayor dw 0 

bufferMediaR dw 10 dup('$')
bufferMedianaR dw 10 dup('$')
bufferModaR dw 10 dup('$')
bufferMenorR dw 10 dup('$')
bufferMayorR dw 10 dup('$') 



; =============================VARIABLES PARA REPORTE =======================================================
comillas db '"' 
finReporte db 0ah,  '	}', 
			  0ah,  '}'
inicioReporte db '{', 
				 0ah,  '	"reporte":',
				 0ah,  '	{', 
				 0ah,  '		"alumno":', 
				 0ah,  '		{', 
				 0ah,  '			"Nombre":"John Henry Lopez Mijangos",', 
				 0ah,  '			"Carne":"201710392",', 
				 0ah,  '			"Seccion":"A",', 
				 0ah,  '			"Curso":"Arquitectura de Computadores y Ensambladores 1"',
				 0ah,  '		},',
				 0ah,  '		"fecha":',
				 0ah,  '		{',
				 0ah,  '			"Dia":'
bufferMes db 0ah,      '			"Mes":'
bufferAnio db 0ah,     '			"Año":'
cerrarObject1 db 0ah,  '		}'
inicioHora db 0ah,     '		"hora":', 
			  0ah, 	   '		{', 
			  0ah, 	   '			"Hora":'
bufferMinutos db 0ah,  '			"Minutos":'
bufferSegundos db 0ah, '			"Segundos":'
inicioResultados db 0ah, '		"resultados":',
			        0ah, '		{', 
			        0ah, '			"Media": '
bufferMediana db 0ah,    ' 			"Mediana": "Aqui estuviera si tan solo lo hubiera hecho"'
bufferModa db 0ah,       '			"Moda": "Aqui estuviera si tan solo lo hubiera hecho"'
bufferMenor db 0ah,      '			"Menor": '
bufferMayor db 0ah,      '			"Mayor": '
inicioPadre db 0ah,    '		"operaciones":'
comilla db 0ah, '		"'
cierreComillas db  '":'
cierreComillas2 db '":', '$'
inicioArreglo db 0ah,  '		['
finArreglo db 0ah,     '		]'
coma db ','
coma2 db ',','$'
inicioOR db 0ah, 09h, 09h, 09h,'{',
			0ah, 09h, 09h, 09h, 09h, '"', '$'
finOR db    0ah, 09h, 09h, 09h ,'}', '$'


numeroEscribir dw 0


bufferFecha db 2 dup(' ')
handleFicheroReporte dw ?

msgErrorAbrir db 0ah, 0dh,'Error al Abrir el archivo hijop', '$'
msgErrorLeer db 0ah, 0dh,'Error al leer el archivo', '$'
msgErrorCrear db 0ah, 0dh,'Error al crear el archivo', '$'
msgErrorEscribir db 0ah, 0dh, 'Error al escribir archivo', '$'

bufferOperaciones db 200 dup('$')

name1 db 20 dup('$')
r1 db 30 dup('$')
name2 db 20 dup('$')
r2 db 30 dup('$')
name3 db 20 dup('$')
r3 db 30 dup('$')
name4 db 20 dup('$')
r4 db 30 dup('$')
name5 db 20 dup('$')
r5 db 30 dup('$')


pivote db 30 dup('$')



msgNoIguales db 0ah, 0dh, ' La cadena no es igual ','$'
msgIguales db 0ah, 0dh, ' La cadena es igual ','$'


;===================== Comandos =====================
CommandShowMedia db 0ah, 0dh, 'Resultado estadistico media:  ', '$'
CommandShowMediana db 0ah, 0dh, 'Comando Show Mediana', '$'
CommandShowModa db 0ah, 0dh, 'Comando Show Moda', '$'
CommandShowMayor db 0ah, 0dh, 'Resultado estadistico mayor:  ', '$'
CommandShowMenor db 0ah, 0dh, 'Resultado estadistico menor:  ', '$'
CommandID db 0ah, 0dh, 'Buscar ID ', '$'
errorConsole db 0ah, 0dh, ' No se reconoce el comando ', '$'
probando db 0ah, 0dh, 'Probando que entro aqui','$'
msgnombrePadre db 0ah, 0dh, 'Nombre del Objeto Padre: ', '$'
msgTotalOperaciones db 0ah, 0dh, 'Total de Operaciones: ', '$'


;------LECTURA DEL ARCHIVO .JSON ------------------------------------------
msgErrorOpen db 0ah, 0dh,'Error al Abrir el archivo', '$'
msgErrorRead db 0ah, 0dh,'Error al leer el archivo', '$'
msgErrorCreate db 0ah, 0dh,'Error al crear el archivo', '$'
msgErrorWrite db 0ah, 0dh, 'Error al escribir archivo', '$'
msgArchivoLeido db 0ah, 0ah, 0dh, '> Archivo Leido con exito! ', '$'
msgFinOperacion db 0ah, 0dh, 'FIN DE OPERACION ', '$'
msgCargarArchivo db 0ah, 0dh, ' INGRESE RUTA: ','$'
rutaArchivo db 100 dup(?)
bufferLectura db 10000 dup('$')
limpiarD db 21 dup('$')
bufferEscritura db 20 dup(' ')
handleFichero dw ?
dividirpor db 0

suma1 db 0ah, 0dh, 'SUMA', '$'
resta1 db 0ah, 0dh, 'RESTA', '$'
multiplicacion1 db 0ah, 0dh, 'MULTIPLICACION', '$'
division1 db 0ah, 0dh, 'DIVISION', '$'

;============Variables para Lectura de JSON (ANALIZADOR) ================
 
estado db 0
contadorPadre db 0
contadorNumero db 0
inicioArchivo db 0
contadorLlaves db 0
contadorguardar db 0

bufferAux db 30 dup('$')
finOpe db 0
msgRegresarPila db 0ah, 0dh, 'Regresando registros a la Pila', '$'
msgRevisarPila db 0ah, 0dh, 'Revisando pila ', '$'
msgIDoperacion db 0ah, 0dh, 'Este es el ID de una nueva operacion: ','$' 
msgResultado db 0ah, 0dh, 'Resultado ', '$'
msgnumeroNegativo db 0ah, 0dh, 'Es un numero negativo', '$'
negativo db 0
auxiliar dw 0



tllavea db 0ah, 0dh, '{ ', '$'
tidpadre db 0ah, 0dh, 'token padre ', '$'
tid db 0ah, 0dh, 'token id ', '$'
tope db 0ah, 0dh, 'operacion simbolo', '$'

tnumero db 0ah, 0dh, '#', '$'






;==============OPERACIONES==============================================
resultados db 30 dup(0)
totalOperaciones dw 0
operaciones dw 20 dup('$')
total dw 0
imprimirNumero db 30 dup('$')
numero1 db 100 dup('$')
numero2 db 100 dup('$')
nombrePadre db 20 dup('$'), '$' ; AQUI LO TENGO COMO LO GUARDO DESDE EL ARCHIVO 
extension db '.json', '$'
nombreReporte db 20 dup('$'),'$'  ; NOMBRE DEL REPORTE CON LA EXTENSION 
nombreOperacion db 12 dup('$')
finnnn db '$'
namePadre db 30 dup('$'), '$'
contadorbufferPadre db 0
auxMayor dw 0
auxMenor dw 0





.code 
main proc 

		MOV ah,09h
		MOV dx,@data
		MOV ds, dx		
		print encabezado
		
	Ingresar:		
		print menu
		XOR al, al
		MOV ah, 01h 
		INT 21h 
		CMP al, 31h  ; 1
		JE CargarArchivo
		CMP al, 32h  ; 2
		JE Consola
		CMP al, 33h  ; 3
		JE Salir
		XOR al, al 
		JMP Ingresar


	CargarArchivo:
		print encCargarArchivo
		print msgCargarArchivo
		opcion1
		;print tid
		;calcularMedia operaciones
		;print tid
		;calcularMayor operaciones
		;print tid
		;calcularMenor operaciones
		JMP Ingresar

	Consola:
		print encConsola 
		obtenerComandos
		JMP Ingresar

	Salir:
		MOV ah,4ch
		INT 21h	

main endp
end

		;crearReporte
		;calcularMedia 
		;getNumero numero1
		;getNumero numero2
		;ConvertirAscii numero1
		;MOV bx, ax 
		;PUSH bx 
		;ConvertirAscii numero2
		;POP bx 

		;ADD ax,bx 
		;ConvertirString imprimirNumero
		;print imprimirNumero