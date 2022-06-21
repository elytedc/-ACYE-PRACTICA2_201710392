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

printChar macro char
	MOV dl, char
	MOV ah, 02h 
	int 21h 
endm 

printNumero macro entrada
	mov ah, 02h 
	xor dl, dl
	mov dl, entrada
	add dl, 48
	int 21h 
endm 

limpiarBuffer macro fila
	LOCAL CICLO, SALIR
	PUSH cx 
	PUSH si 
	PUSH dx 

	mov cx, SIZEOF fila
	xor si, si
	MOV si, 0
	CICLO:
		xor dl, dl
		MOV fila[si], 24h		
		inc si	
		dec cx 
		cmp cx, 0
		je SALIR
		JMP CICLO

	SALIR:
		POP dx 
		POP si
		POP cx 
endm 

revisarBuffer macro 
	LOCAL CONTINUE, Imprimir, SALIR
	PUSH cx

	MOV cx, 20
	XOR si, si 
	MOV si, offset bufferLectura

	CONTINUE:
		XOR dl, dl
		MOV dl, [si]
		;printChar[si]
		analizador   ; aqui cambie para probar el analizador
		inc si
		LOOP CONTINUE
		JMP SALIR
	
	Imprimir:
		printChar [si]
		inc si 
		JMP CONTINUE

	SALIR:
		POP cx 	
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

		base1:
			llenarOperacionesR name1, nombreOperacion
			llenarOperacionesR r1, pivote
			limpiarBuffer pivote
			JMP CICLO
		base2:
			llenarOperacionesR name2, nombreOperacion
			llenarOperacionesR r2, pivote
			limpiarBuffer pivote
			JMP CICLO

		base3:
			llenarOperacionesR name3, nombreOperacion
			llenarOperacionesR r3, pivote
			limpiarBuffer pivote
			JMP CICLO	

		base4:
			llenarOperacionesR name4, nombreOperacion
			llenarOperacionesR r4, pivote
			limpiarBuffer pivote
			JMP CICLO

		base5:
			llenarOperacionesR name5, nombreOperacion
			llenarOperacionesR r5, pivote
			limpiarBuffer pivote
			JMP CICLO


		BuscarID:
		
			INC si 
			MOV dh, buffer[si]
			CMP dh, 22h ; para guardar EL ID necesito otra "
			JE guardarID 

			CMP dh,23h ; # 
			JE BuscarNumero

			PUSH si 
			XOR si, si 
			MOV si, cx 
			MOV bufferAux[si], dh 
			inc cx 
			POP si 
			JMP BuscarID

		guardarID:
		;print padre
		;print saltoLinea
			XOR cx, cx
			XOR ax, ax 
			MOV ah, bufferAux
			;PUSH ax 

			CMP contadorPadre, 0
			JE guardarPadre 

			MOV contadorNumero, 0

			CMP finOpe, 0
			JE guardaIDoperacion

			CMP ah, 61h ; a 
			JE Csuma 
			CMP ah, 41h ; A 
			JE Csuma 
			CMP ah, 64h ; d
			JE Cdivision
			CMP ah, 44h ; D 
			JE Cdivision
			CMP ah, 73h ; s 
			JE Cresta 
			CMP ah, 53h ; S 
			JE Cresta
			CMP ah, 6Dh ; m 
			JE Cmultiplicacion
			CMP ah, 4Dh ; M 
			JE Cmultiplicacion

			PUSH ax 
			;print saltoLinea
			;print bufferAux 
			limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO

		Csuma:
			XOR ax, ax 
			MOV ah, 2Bh ; +
			limpiarBuffer bufferAux
			MOV bufferAux, ah 
			PUSH ax 

			;print saltoLinea
			;print bufferAux
			limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO

		Cresta:
			XOR ax, ax 
			MOV ah, 2Dh ; - 
			limpiarBuffer bufferAux
			MOV bufferAux, ah 
			PUSH ax 

			;print saltoLinea
			;print bufferAux
			limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO

		Cmultiplicacion:
			XOR ax, ax 
			MOV ah, 2Ah ; *
			limpiarBuffer bufferAux
			MOV bufferAux, ah 
			PUSH ax 
			;print tope
			;print saltoLinea
			;print saltoLinea
			;print bufferAux
			limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO

		Cdivision:
			XOR ax, ax 
			MOV ah, 2Fh ; /
			limpiarBuffer bufferAux
			MOV bufferAux, ah 
			PUSH ax 

			;print saltoLinea
			;print bufferAux
			limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO

		guardaIDoperacion:
			MOV finOpe, 1
			;print msgIDoperacion
			;print saltoLinea
			;print bufferAux  ;;;  pasa cuando lee el pimer id................operacion1
			transferir bufferAux, nombreOperacion
			;ConvertirString bufferAux
			;print saltoLinea
		   ; print bufferAux ;;;                            imrprime el dato de de id de operacion----
			limpiarBuffer bufferAux
			;print tid
			;print saltoLinea
			XOR cx, cx
			INC si 
			JMP CICLO 		

		guardarPadre:
			MOV contadorPadre, 2
			;getChar
			;print msgnombrePadre 
			;print saltoLinea
			transferir bufferAux, nombrePadre
			;print nombrePadre
			limpiarBuffer bufferAux
			;print tidpadre
			;print saltoLinea
			XOR cx, cx
			INC si 
			JMP CICLO 

		BuscarNumero:
			limpiarBuffer bufferAux
			MOV negativo, 0 ; aqiiii 
			INC si 
			MOV dh, buffer[si]
			CMP dh, 22h ; busca ""
			JE BuscarNumero
			CMP dh, 3Ah    ; nose que es
			JE BuscarNumero
			CMP dh, 20h  ; espacio
			JE BuscarNumero
			JMP ObtenerNumero

		ObtenerNumero:
			MOV dh, buffer[si]
			CMP dh, 2Ch ; , 
			JE EndNumero 
			CMP dh, 7Dh ; }
			JE EndNumero
			;CMP dh, 0ah ; \n 
			;JE EndNumero 
			;CMP dh, 20h ; espacio 
			;JE EndNumero

			CMP dh, 2Dh ; - 
			JE numeroNegativo

			PUSH si 
			XOR si, si 
			MOV si, cx 
			MOV bufferAux[si], dh 
			inc cx 
			POP si 
			INC si 
			JMP ObtenerNumero

		numeroNegativo:
			MOV negativo, 1
			INC si 
			MOV dh, buffer[si]

			PUSH si 
			XOR si, si 
			MOV si, cx 
			MOV bufferAux[si], dh 
			INC cx 
			POP si 
			INC si 
			JMP ObtenerNumero

		EndNumero:
			XOR cx, cx 
			XOR ax, ax 
			CMP contadorNumero, 0
			JE primerNumero 
			CMP contadorNumero, 1
			JE segundoNumero
			JMP CICLO

		primerNumero:
			MOV contadorNumero, 1
			;getChar
			;print saltoLinea
			;print bufferAux

			ConvertirAscii bufferAux
			PUSH ax 

			;limpiarBuffer bufferAux
			XOR cx, cx 
			INC si 
			JMP CICLO 

		segundoNumero:
			MOV contadorNumero, 0

			;getChar
			;print saltoLinea 
			;print bufferAux
			ConvertirAscii bufferAux
			PUSH ax 
			;limpiarBuffer bufferAux
			JMP  Operacion 
			XOR cx, cx 
			INC si 
			JMP CICLO

		Operacion:
			XOR bx, bx 
			XOR cx, cx 
			XOR ax, ax 
			POP ax  
			MOV bx, ax 
			POP ax  
			MOV cx, ax 
			POP ax 

			CMP ah, 2Ah 
			JE Multiplicacion
			CMP ah, 2Fh 
			JE Division 
			CMP ah, 2Bh 
			JE Suma
			CMP ah, 2Dh 
			JE Resta			

		Suma:			
			;print suma1
			MOV ax, cx 
			ADD ax, bx 

			XOR cx, cx 
			MOV cx, ax 
			PUSH ax 
			MOV ax, cx
			;ConvertirString bufferAux
			;print saltoLinea
		    ;print bufferAux
		    MOV contadorNumero, 1
		    JMP revisarPila
			
			XOR cx, cx 
			INC si 
			JMP CICLO

		Resta:
			;print resta1 
			MOV ax, cx 
			SUB ax, bx 

			XOR cx, cx 
			MOV cx, ax 
			PUSH ax 
			MOV ax, cx
			;ConvertirString bufferAux
			;print saltoLinea
		    ;print bufferAux

			POP ax 
			MOV bx, ax 
			;limpiarBuffer bufferAux
			; bufferAux
			;print saltoLinea
			;print bufferAux
			MOV ax, bx 
			PUSH ax 
			MOV contadorNumero, 1
			JMP revisarPila
			XOR cx, cx 
			INC si 
			JMP CICLO

		Multiplicacion:
			;print multiplicacion1 
			MOV ax, cx 
			IMUL bx 		

			XOR cx, cx 
			MOV cx, ax 
			PUSH ax 
			MOV ax, cx		    

			;ConvertirString bufferAux
			;print saltoLinea
		    ;print bufferAux  ; imprime numero en string-----------------------------------------

		    MOV contadorNumero, 1
		    JMP revisarPila 
			XOR cx, cx
			INC si 
			JMP CICLO

		Division:
			;print division1  ; aqio va bien JAJAJA 
			XOR dx, dx 
			MOV ax, cx 
			CWD 
			IDIV bx		

			XOR cx, cx 
			MOV cx, ax 
			PUSH ax 
			MOV ax, cx
			;ConvertirString bufferAux
			;print saltoLinea
		    ;print bufferAux

		    MOV contadorNumero, 1
			JMP revisarPila 

			XOR cx, cx 			
			INC si 
			JMP CICLO

		revisarPila:
			;print msgRevisarPila 
		 	XOR cx, cx
		 	XOR bx, bx 
		 	XOR ax, ax 
		 	POP ax  
		 	MOV bx, ax 
		 	POP ax  

		 	CMP ah, 2Ah  ; *
			JE regresarPila
			CMP ah, 2Fh  ; /
			JE regresarPila
			CMP ah, 2Bh  ; +
			JE regresarPila
			CMP ah, 2Dh  ; -
			JE regresarPila
			
			MOV cx, ax 
			POP ax 

			CMP ah, 2Ah 
			JE Multiplicacion
			CMP ah, 2Fh 
			JE Division 
			CMP ah, 2Bh 
			JE Suma
			CMP ah, 2Dh 
			JE Resta

		regresarPila:
			PUSH ax 
			MOV ax, bx 
			PUSH ax 
			INC si 
			JMP CICLO

		SALIR:
			XOR ax, ax
			MOV ax, totalOperaciones
			;print msgTotalOperaciones
			;print bufferAux
			;print bufferAux  ; miro que hay en la pila
			print saltoLinea
			print name1
			print r1
			print saltoLinea
			print name2
			print r2
			print saltoLinea
			print name3
			print r3
			print bufferOperaciones  ; imprime la cadena que tiene los
endm 

ingresarOperaciones macro buffer 
	LOCAL CICLO, INGRESAR
	PUSH si 
	PUSH ax 
	PUSH bx 
	PUSH cx 
	
	limpiarBuffer bufferAux
	LEA si, buffer 
	MOV cx, totalOperaciones

		CICLO:
			MOV ax, [si]
			CMP al, 24h 
			JE INGRESAR
			INC si 
			INC si 
			JMP CICLO

		INGRESAR:
			MOV ax, auxiliar
			MOV [si],ax
			MOV auxiliar, 0
			;ConvertirString bufferAux
			;print saltoLinea 
			;print bufferAux
			POP cx 
			POP bx 
			POP ax 
			POP si 
endm 

toString macro buffer 
	LOCAL NEGATIVE, LIMPIAR, DIVIDIR, GUARDAR, FIN
	PUSH si 
	PUSH cx 
	PUSH bx 
	PUSH dx 

	XOR si, si 
	XOR cx, cx 
	XOR bx, bx 
	XOR dx, dx 

	MOV bx, 0ah 
	TEST ax, ax 
	JNZ NEGATIVE
	JMP DIVIDIR 

	NEGATIVE:
		NEG ax
		MOV buffer[si], 45
		INC si 
		JMP DIVIDIR 

	LIMPIAR:
		XOR dx, dx 

	DIVIDIR:
		DIV bx 
		INC cx 
		PUSH dx 
		CMP ax, 00h 
		JE GUARDAR
		JMP LIMPIAR

	GUARDAR:
		POP ax 
		ADD ax, 30h 
		MOV buffer[si], ah 

		INC si 
		LOOP GUARDAR

		MOV ax, 24h 
		MOV buffer[si], ah 

	FIN:
		POP dx
		POP bx 
		POP cx 
		POP si 
endm 

printChar1 macro char
	MOV dl, char
	MOV ah, 02h 
	int 21h 
endm 

leerF macro numbytes, buffer, handle ;; ESTE FUNCIONA 
	LOCAL ErrorRead, SALIR
	MOV ah,3fh 
	MOV bx,handle
	MOV cx,numbytes
	LEA dx, buffer
	INT 21h 
	JC ErrorRead
	JMP SALIR

	ErrorRead:
		print msgErrorRead

	SALIR:	
		;print buffer
endm 

openF macro ruta, handle
	LOCAL SALIR, ErrorOpen
	MOV ah, 3dh
	MOV al, 10h 
	LEA dx, ruta 
	INT 21h 
	MOV handle, ax 
	JC ErrorOpen
	JMP SALIR

	ErrorOpen:
		print msgErrorOpen
		JMP Ingresar

	SALIR:
endm 

closeF macro handle
	MOV ah, 3eh
	MOV handle, bx
	INT 21h 
endm 

writeF macro numbytes, buffer, handle
	
	LOCAL ErrorEscribir, SALIR 

	PUSH cx 
	mov ah, 40h 
	mov bx, handle
	mov cx, numbytes
	lea dx, buffer
	int 21h 
	jc ErrorEscribir
	JMP SALIR

	ErrorEscribir:
		print msgErrorEscribir 

	SALIR:
		POP cx 
endm 

crearF macro buffer, handle
	LOCAL ErrorCrear, SALIR 
	mov ah, 3ch 
	mov cx,00h 
	lea dx, buffer
	int 21h 
	mov handle, ax
	jc ErrorCrear
	JMP SALIR

	ErrorCrear:
		print msgErrorCrear

	SALIR:		
endm

getRuta macro buffer 
	LOCAL INICIO, SALIR 
	XOR si,si 

	INICIO:
		getChar
		cmp al, 0dh 
		je SALIR
		MOV buffer[si], al
		inc si 
		JMP INICIO

	SALIR:
		mov buffer[si], 00h 
endm

getTexto macro buffer
	LOCAL CONTINUE, FIN
	PUSH SI
	PUSH AX

	xor si,si
	CONTINUE:
		getChar
		cmp al,0dh
		je FIN
		mov buffer[si],al
		inc si
		jmp CONTINUE

	FIN:
		mov al,'$'
		mov buffer[si],al
		;print buffer

	POP AX
	POP SI
endm

ShowCommand macro 
	LOCAL SALIR, Media, Mediana, Moda, Mayor, Menor, BuscarID
	
	getTexto comandoConsola
	compararCadenas comandoConsola, showMediana, 8
	CMP al, 1
	JE Mediana
	XOR al, al
	compararCadenas comandoConsola, showMedia, 6
	CMP al, 1
	JE Media
	XOR al, al
	compararCadenas comandoConsola, showModa, 5
	CMP al, 1
	JE Moda
	XOR al, al
	compararCadenas comandoConsola, showMenor, 6
	CMP al, 1
	JE Menor
	XOR al, al
	compararCadenas comandoConsola, showMayor2, 6
	CMP al, 1
	JE Mayor
	XOR al, al
	JMP BuscarID

	Media:
		print CommandShowMedia
		print bufferMediaR
		JMP SALIR

	Mediana:
		print CommandShowMediana
		JMP SALIR

	Moda:
		print CommandShowModa
		JMP SALIR

	Menor:
		print CommandShowMenor
		print bufferMenorR
		JMP SALIR

	Mayor:
		print CommandShowMayor
		print bufferMayorR
		JMP SALIR

	BuscarID:
		print CommandID
		buscandoID
		JMP SALIR

	SALIR:
endm 



buscandoID macro 
	LOCAL Reporte, SALIR
	compararCadenas comandoConsola, nombrePadre, 12
	cmp al, 1
	JE Reporte 

	compararCadenas comandoConsola, name1, 20
	cmp al, 1
	JE iname1

	compararCadenas comandoConsola, name2, 20
	cmp al, 1
	JE iname2

	compararCadenas comandoConsola, name3, 20
	cmp al, 1
	JE iname3

	compararCadenas comandoConsola, name4, 12
	cmp al, 1
	JE iname4

	compararCadenas comandoConsola, name5, 12
	cmp al, 1
	JE iname5

	
	JMP SALIR

	iname1:
		print saltoLinea
		print r1
		limpiarBuffer comandoConsola
		JMP SALIR

	iname2:
		print saltoLinea
		print r2
		limpiarBuffer comandoConsola
		JMP SALIR

	iname3:
		print saltoLinea
		print r3
		limpiarBuffer comandoConsola
		JMP SALIR

	iname4:
		print saltoLinea
		print r4
		limpiarBuffer comandoConsola
		JMP SALIR

	iname5:
		print saltoLinea
		print r5
		limpiarBuffer comandoConsola
		JMP SALIR

	Reporte:
		transferirExtension nombrePadre, nombreReporte, extension
		transferir2 nombrePadre, namePadre, SIZEOF nombrePadre
		print saltoLinea
		print nombrePadre
		print saltoLinea
		print nombreReporte
		crearReporte
		limpiarBuffer comandoConsola
		JMP SALIR

	SALIR:
endm 




compararCadenas macro cadena1, cadena2, cantidad
	LOCAL SALIR

	MOV al, 0
	LEA si, cadena1
	LEA di, cadena2
	PUSH ds
	POP es 
	CLD 
	MOV cx, cantidad
	REPE CMPSB 
	JA SALIR
	JB SALIR
	MOV al, 1

	SALIR:	
endm 

obtenerComandos macro 
	LOCAL INICIO, SALIR, E, Ex, Exi, Exit,S, Sh, Sho, Show,CShow, Error 

	INICIO:
		print inicioConsole
		getChar 
		CMP al, 65h 
		JE E
		CMP al, 73h 
		JE S
		JMP Error

	E:
		getChar 
		CMP al, 78h 
		JE Ex
		JMP Error

	Ex:
		getChar
		CMP al, 69h 
		JE Exi
		JMP Error

	Exi:
		getChar 
		CMP al, 74h 
		JE Exit 
		JMP Error

	Exit:
		JMP Ingresar
		JMP SALIR

	S:
		getChar
		CMP al, 68h 
		JE Sh 
		JMP Error

	Sh:
		getChar
		CMP al, 6fh
		JE Sho
		JMP Error

	Sho:
		getChar
		CMP al,77h 
		JE Show 
		JMP Error

	Show:
		getChar 
		CMP al, 20h 
		JE CShow
		JMP Error

	CShow:
		ShowCommand
		JMP SALIR

	Error:
		print errorConsole
		JMP INICIO

	SALIR:
endm 

getChar macro
	MOV ah,01h
	int 21h
endm 

calcularMedia macro arreglo 
	LOCAL CONTEO, SALIR

	XOR ax, ax 
	MOV cx, 0
	MOV cx,ax
	MOV ax, 0
	LEA si, arreglo

	CONTEO:
		CMP cx,totalOperaciones
		JE FIN 
		INC cx 
		XOR bx, bx 
		MOV bx, [si]
		ADD ax, bx 
		INC si
		INC si 
		JMP CONTEO

	FIN:		
		CWD
		IDIV totalOperaciones ; el promedio esta en al 
		MOV media, ax 

		limpiarBuffer bufferAux
		ConvertirString bufferAux
		transferirCadenas bufferAux, bufferMediaR
endm 

calcularMayor macro arreglo
	LOCAL CICLO, FIN, MAYOR
	MOV ax, 0
	MOV bx, 0
	MOV si, 0
	MOV cx, 0

	MOV ax, arreglo[si]
	MOV bx, totalOperaciones
	DEC bx 
	SAL bx, 1
	XOR si, si 

		CICLO:
			CMP si,bx 
			JAE FIN
			INC si 
			INC si
			CWD
			CMP ax, arreglo[si]
			JNLE CICLO
			MOV ax, arreglo[si]

			JMP CICLO 

		FIN:
			MOV auxMayor, ax 

			limpiarBuffer bufferAux
			ConvertirString bufferAux
			transferirCadenas bufferAux, bufferMayorR
endm 

calcularMenor macro arreglo
	LOCAL CICLO, FIN, MAYOR
	MOV ax, 0
	MOV bx, 0
	MOV si, 0
	MOV cx, 0

	MOV ax, arreglo[si]
	MOV bx, totalOperaciones
	DEC bx 
	SAL bx, 1
	XOR si, si 

		CICLO:
			CMP si,bx 
			JAE FIN
			INC si 
			INC si
			CWD
			CMP ax, arreglo[si]
			JNGE CICLO  ; MENOR CON SIGNO 
			MOV ax, arreglo[si]

			JMP CICLO 

		FIN:
			MOV auxMenor, ax 

			limpiarBuffer bufferAux
			ConvertirString bufferAux
			transferirCadenas bufferAux, bufferMenorR
endm 


ConvertirString macro buffer
	LOCAL Dividir,Dividir2,FinCr3,NEGATIVO,FIN2,FIN
	PUSH si 
	PUSH cx 
	PUSH bx 
	PUSH dx 
	xor si,si
	xor cx,cx
	xor bx,bx
	xor dx,dx
	mov dl,0ah
	test ax,1000000000000000
	jnz NEGATIVO
	jmp Dividir2

	NEGATIVO:
		neg ax
		mov buffer[si],45
		inc si
		jmp Dividir2

	Dividir:
		xor ah,ah
	Dividir2:
		div dl
		inc cx
		push ax
		cmp al,00h
		je FinCr3
		jmp Dividir
	FinCr3:
		pop ax
		add ah,30h
		mov buffer[si],ah
		inc si
		loop FinCr3
		mov ah,24h
		mov buffer[si],ah
		inc si
	FIN:
		POP dx 
		POP bx 
		POP cx 
		POP si 
endm

ConvertirAscii macro numero
	LOCAL INICIO,FIN, negar, SALIR 
	PUSH bx 
	PUSH cx 
	PUSH si 

	xor ax,ax
	xor bx,bx
	xor cx,cx
	mov bx,10	
	xor si,si
	INICIO:
		mov cl,numero[si] 
		cmp cl,48
		jl FIN
		cmp cl,57
		jg FIN
		inc si
		sub cl,48	
		mul bx		
		add ax,cx	
		jmp INICIO
	FIN:
		CMP negativo, 1
		JE negar 
		POP si
		POP cx 
		POP bx 
		JMP SALIR 
	negar:
		MOV negativo, 0
		NEG ax 
		POP si 
		POP cx 
		POP bx 

	SALIR:
endm

getChar macro
	mov ah,01h
	int 21h
endm

getNumero macro buffer
	LOCAL INICIO,FIN, numeroNegativo
	MOV negativo, 0
	xor si,si
	INICIO:
		getChar
		cmp al,0dh
		je FIN
		CMP al,45 
		je numeroNegativo 
		mov buffer[si],al
		inc si
		jmp INICIO

	numeroNegativo:
		MOV negativo, 1
		JMP INICIO 
	FIN:
		mov buffer[si],00h
endm

crearReporte macro 
	crearF nombreReporte, handleFicheroReporte
	writeF SIZEOF inicioReporte, inicioReporte, handleFicheroReporte 
	ObtenerFecha
	ObtenerResultados
	ObtenerOperaciones
	writeF SIZEOF finReporte, finReporte, handleFicheroReporte
	closeF handleFicheroReporte
endm 

Obtenerfecha macro 
	LOCAL HORA, MINUTOS, DIA, MES, ANIO, SALIR 

		DIA:
		MOV si, 0
		MOV AH,2AH    ; To get System Date
		INT 21H
		;ADD DL, 1
		MOV AL,DL     ; Day is in DL
		AAM
		MOV BX,AX	
		showDate	
		writeF SIZEOF coma, coma, handleFicheroReporte

		MES:
		writeF SIZEOF bufferMes, bufferMes, handleFicheroReporte
		MOV si,0
		MOV AH,2AH    
		INT 21H
		MOV AL,DH     ; Month is in DH
		AAM
		MOV BX,AX
		showDate
		writeF SIZEOF coma, coma, handleFicheroReporte

		ANIO:
		writeF SIZEOF bufferAnio, bufferAnio, handleFicheroReporte
		MOV si, 0
		MOV AH,2AH    
		INT 21H
		ADD CX,0F830H ; To negate the effects of 16bit value,
		MOV AX,CX     ; since AAM is applicable only for AL (YYYY -> YY)
		AAM
		MOV BX,AX
		showDate
		writeF SIZEOF cerrarObject1, cerrarObject1, handleFicheroReporte
		

		HORA:
		writeF SIZEOF inicioHora, inicioHora, handleFicheroReporte
		MOV si, 0
		MOV AH, 2CH
		int 21h
		mov AL,CH ; Hora en CH  
		aam 
		mov BX, AX
		showDate
		writeF SIZEOF coma, coma, handleFicheroReporte


		MINUTOS:
		writeF SIZEOF bufferMinutos, bufferMinutos, handleFicheroReporte
		MOV si, 0
		MOV AH,2CH    ; To get System Time
		INT 21H
		MOV AL,CL     ; Minutes is in CL
		AAM
		MOV BX,AX
		showDate
		writeF SIZEOF coma, coma, handleFicheroReporte

		SEGUNDOS:
		writeF SIZEOF bufferSegundos, bufferSegundos, handleFicheroReporte
		MOV si, 0
		MOV AH , 2CH     ; Para obtener la hora del sistema 
		INT 21H 
		MOV AL , DH      ; Los segundos están en DH 
		AAM
		MOV BX , AX 
		showDate
		writeF SIZEOF cerrarObject1, cerrarObject1, handleFicheroReporte
endm 

ObtenerResultados macro 
	writeF SIZEOF inicioResultados, inicioResultados, handleFicheroReporte

	;MEDIA 
	contador bufferMediaR
	writeF numeroEscribir, bufferMediaR, handleFicheroReporte	
	writeF SIZEOF coma, coma, handleFicheroReporte
	
	;MEDIANA  --- NO SE HA HECHO  ---
	writeF SIZEOF bufferMediana, bufferMediana, handleFicheroReporte
	writeF SIZEOF coma, coma, handleFicheroReporte

	; MODA  --- NO SE HA HECHO  --- 
	writeF SIZEOF bufferModa, bufferModa, handleFicheroReporte	
	writeF SIZEOF coma, coma, handleFicheroReporte

	;MENOR   
	writeF SIZEOF bufferMenor, bufferMenor, handleFicheroReporte
	contador bufferMenorR
	writeF numeroEscribir, bufferMenorR, handleFicheroReporte
	writeF SIZEOF coma, coma, handleFicheroReporte

	;MAYOR 
	writeF SIZEOF bufferMayor, bufferMayor, handleFicheroReporte
	contador bufferMayorR
	writeF numeroEscribir, bufferMayorR, handleFicheroReporte
	
	writeF SIZEOF cerrarObject1, cerrarObject1, handleFicheroReporte
endm 

ObtenerOperaciones macro 
	writeF SIZEOF comilla, comilla, handleFicheroReporte
	contador namePadre
	writeF numeroEscribir, namePadre, handleFicheroReporte
	writeF SIZEOF cierreComillas, cierreComillas, handleFicheroReporte
	writeF SIZEOF inicioArreglo, inicioArreglo, handleFicheroReporte
	contador bufferOperaciones
	dec numeroEscribir
	writeF numeroEscribir, bufferOperaciones, handleFicheroReporte
	; =============AQUI TODAS LAS OPERACIONES QUE SE REALIZEN CON SU RESULTADO =========
	writeF SIZEOF finArreglo, finArreglo, handleFicheroReporte
endm 

showDate macro
		MOV DL,BH      ; Since the values are in BX, BH Part
		ADD DL,30H     ; ASCII Adjustment

		MOV bufferFecha[si],dl 
		inc si 

		MOV DL,BL      ; BL Part 
		ADD DL,30H     ; ASCII Adjustment
		MOV bufferFecha[si],dl 
		inc si 
		writeF SIZEOF bufferFecha, bufferFecha, handleFicheroReporte
endm 

fecha2 macro 

	HORA:
		MOV si, 0
		MOV ah, 2ch 
		int 21h 
		mov al, ch 
		aam 
		mov bx, ax

		mov dl, ':'
		MOV bufferFecha[si],dl 
		inc si 

	MINUTOS:
		MOV AH,2CH    ; To get System Time
		INT 21H
		MOV AL,CL     ; Minutes is in CL
		AAM
		MOV BX,AX

		mov dl, '-'
		MOV bufferFecha[si],dl 
		inc si

	MES:
		MOV AH,2AH    ; To get System Date
		INT 21H
		MOV AL,DH     ; Month is in DH
		AAM
		MOV BX,AX	
	
		mov dl, '/'
		MOV bufferFecha[si],dl 
		inc si 

	ANIO:
		MOV AH,2AH    ; To get System Date
		INT 21H
		ADD CX,0F830H ; To negate the effects of 16bit value,
		MOV AX,CX     ; since AAM is applicable only for AL (YYYY -> YY)
		AAM
		MOV BX,AX

	SALIR:
endm

transferirExtension macro arreglo1, arreglo2, arreglo3,
	
	LEA si, arreglo1
	LEA bx, arreglo2
	MOV cx, 12

	CONTINUE:
		MOV ax, [si] 
		MOV [bx], ax
		INC si 
		INC bx
		LOOP CONTINUE

	LEA si, arreglo3
	MOV cx,4 

	OTRO: 
		MOV ax, [si]
		MOV [bx],ax
		INC si 
		INC bx 
		LOOP OTRO
endm

transferir macro arreglo1, arreglo2 
	LOCAL INGRESAR
	PUSH si 
	PUSH cx
	PUSH ax 
	PUSH bx
	LEA si, arreglo1
	LEA bx, arreglo2
	MOV cx, 12

	INGRESAR:
		MOV ax, [si] 
		MOV [bx], ax
		INC si 
		INC bx
		LOOP INGRESAR

	POP bx 
	POP ax 
	POP cx 
	POP si 
endm

transferirCadenas macro arreglo1, arreglo2
	LOCAL INGRESAR
	PUSH si 
	PUSH cx
	PUSH ax 
	PUSH bx
	LEA si, arreglo1
	LEA bx, arreglo2
	MOV cx, 10

	INGRESAR:
		MOV ax, [si] 
		MOV [bx], ax
		INC si 
		INC bx
		LOOP INGRESAR

	POP bx 
	POP ax 
	POP cx 
	POP si 
endm

transferir2 macro arreglo1, arreglo2, cantidad 
	LOCAL INGRESAR, SALIR
	PUSH si 
	PUSH cx
	PUSH ax 
	PUSH bx
	LEA si, arreglo1
	LEA bx, arreglo2
	MOV cx, cantidad

	INGRESAR:
		MOV ax, [si] 
		CMP ax, 24h 
		JE SALIR
		MOV [bx], ax
		INC si 
		INC bx
		LOOP INGRESAR

	SALIR:

	MOV ax, 00h 
	MOV [bx], ax 

	POP bx 
	POP ax 
	POP cx 
	POP si 
endm

llenarOperacionesR macro arreglo, arreglo2
	LOCAL CICLO, INGRESAR, Fin 
	PUSH si 
	PUSH ax 
	PUSH bx 
	PUSH cx 

	MOV cx, SIZEOF arreglo
	LEA bx, arreglo2  ; este es el que quiero ingresar 
	LEA si, arreglo

		CICLO:	
			XOR ax, ax 
			MOV ax, [si]
			CMP al, 24h 
			JE INGRESAR
			INC si 
			JMP CICLO

		INGRESAR:
			XOR ax, ax 
			MOV ax, [bx]
			CMP al, 24h 
			JE Fin 
			MOV [si], ax 
			INC bx 
			INC si 
			JMP INGRESAR

		Fin:

	POP cx
	POP bx 
	POP ax 
	POP si 
endm 

contador macro buffer
	LOCAL CICLO, FIN
	PUSH si 
	PUSH ax 
	PUSH bx 
	PUSH cx 
	MOV numeroEscribir, 0
	
	LEA si, buffer
	MOV cx, 0

	CICLO:
		MOV ax, ax 
		MOV ax, [si]
		CMP al, 24h 
		JE FIN 
		INC si 
		INC cx 
		JMP CICLO 

	FIN:
		MOV numeroEscribir, cx 
		POP cx
		POP bx
		POP ax 
		POP si 
endm 






























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