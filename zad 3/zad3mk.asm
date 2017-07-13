DATA1 segment
	parsed_args		db	128 dup(?)
	parsed_args_length	db	?
	nr_args			db 	?
	iternum			db	?
	len				dw	?
	
	args_info		db	"Liczba argumentow: $"
	new_line		db	10, 13, '$'
	wrong_args_err	db	"ERROR: Podano nieprawidlowa liczbe argumentow.", 10, 13, "Nalezy podac:", 10, 13, " liczbe iteracji l-systemu", 10, 13, " dlugosc pojedynczego odcinka krzywej$"
	wrong_1arg_err	db	"ERROR: Pierwszy argument jest niepoprawny.", 10, 13, "Nalezy podac liczbe calkowita z przedzialu 1-4.$"
	wrong_2arg_err	db	"ERROR: Drugi argument jest niepoprawny.", 10, 13, "Nalezy podac dodatnia liczbe calkowita.$"
	comparison_err	db	"ERROR: Wystapil blad podczas wykonywania operacji porownania na koprocesorze.$"
	
	base_string		db	"F++F++F"		;początkowy stan
	insert_string	db	"F-F++F-F"		;ciąg którym zostaje zastąpiona każda litera F przy iteracji l-systemu
	moves_1			db	2000 dup(0)			
	moves_2			db	2000 dup(0)		;tablice służące do generowania sekwencji ruchów przy iteracji l-systemu
	
	x1		dw	70
	x2		dw	0
	y1		dw	150
	y2		dw	0
	dist_x	dw	0
	dist_y	dw	0
	kx		dw	0
	ky		dw	0
	two		dw	2
	three	dw	3
	
	trash	dw	?

DATA1 ends

CODE1 segment

  .386

START:
  	assume cs: CODE1, ss:STOS1, ds:DATA1
	
	mov ax, DATA1
  	mov ds, ax  
  
	mov ax, seg top1				;ustawienie stosu
    mov ss, ax
    mov sp, offset top1
	
	call ParseArgs
	call ExtractArgs
	call IterateLsystem
	call Draw
		
	mov	ax,04c00h
	int	21h
	
	
	
;--------------------------------------------------------------------------------------------------------
;	Procedura scalająca argumenty z linii poleceń (zapisuje je jako jeden ciąg znaków, 
;	poszczególne argumenty rozdzielone znakami '$') 
;	wyjście: 	al-długość scalonych argumentów
;				ah-liczba argumentów 
;--------------------------------------------------------------------------------------------------------

 ParseArgs proc
	push	dx
	push	es
	push	si
	push	cx
	
	xor	dx, dx
	xor	si, si
	xor	cx, cx
	xor	ax, ax
	
	mov	di, offset parsed_args			;robi miejsce na tablicę znaków z linii poleceń
	
	mov	ah, 62h			;pobiera adres Program Segment Prefix (PSP)
	int	21h				;zwraca adres PSP w BX
	mov	es, bx			;przenosi adres PSP do es  

	mov	si, 81h			;adres pierwszego znaku z linii poleceń
   parse:  
	call 	SkipWhiteSpaces	;zjada białe znaki
	cmp		ah, 0d			;gdy nie było białych znaków to znajdujemy się w tym samym argumencie
	je		wyraz			;skocz do 'wyraz'
	
	cmp		al, 13d 		;czy znak końca linii (enter)
	je		koniec_linii	;jeśli tak skocz do 'koniec_linii' (kończy procedurę)
	
	inc 	ch						;jeśli były białe znaki i nie jest to koniec linii to znaczy że napotkaliśmy nowy argument
	mov 	byte ptr ds:[di], '$'	;wstawia znak dolara, który oddziela od siebie argumenty
	inc 	di
	inc		dl					;zwiększa długość scalonych argumentów
	
	wyraz:
		cmp	al, 13d 		;czy znak końca linii (enter)
		je	koniec_linii	;jeśli tak skocz do 'koniec_linii' (kończy procedurę)
		mov	ds:[di], al
		inc	di
		inc	dl				;zwiększa długość scalonych argumentów
		inc 	si				;bierze kolejny znak z linii poleceń i skacze do 'parse' aby go sprawdzić
		jmp 	parse	
	
	koniec_linii:
		mov	byte ptr ds:[di], '$'
		inc	dl
		mov	al, dl				;po wykonaniu w al znajduje się długość scalonych argumentów
		mov	ah, ch				;w ah znajduje się liczba argumentów
		mov	byte ptr parsed_args_length, al
		mov	byte ptr nr_args, ch
	pop		cx
	pop		si
	pop		es
	pop		dx
	ret
  ParseArgs endp
	
;--------------------------------------------------------------------------------------------------------
;	Procedura 'zjadająca' białe znaki
;	wyjście:	al-niebiały znak
;				ah-liczba pominiętych białych znaków
;--------------------------------------------------------------------------------------------------------

  SkipWhiteSpaces proc
	xor	al, al
	xor	ah, ah
   sws_poczatek:
	mov	al, es:[si]
   spacje:
	cmp	al, ' '			;czy znak jest spacją
	je	bialy_znak
   tabulacje:
	cmp	al, 9d			;czy znak jest tabulacją
	je	bialy_znak
	ret
   bialy_znak:
	inc	ah				;ile białych znaków zostało pominiętych
	inc	si
	
	jmp	sws_poczatek
  SkipWhiteSpaces endp	
  

;--------------------------------------------------------------------------------------------------------
;	Wypisuje jeden znak z dl
;--------------------------------------------------------------------------------------------------------

  printC proc	
	push ax
	
	mov	ah,2h
	int	21h
	
	pop ax
	ret
  printC endp	
  
;--------------------------------------------------------------------------------------------------------
;	Przechodzi do nowej linii przy wypisywaniu
;--------------------------------------------------------------------------------------------------------
 
	printNL proc
		push	ax
		push	dx
		
		mov		ah, 9h
		mov		dx, offset new_line
		int		21h				
		
		pop		dx
		pop		ax
		ret
	printNL endp
	
;--------------------------------------------------------------------------------------------------------
;	Sprawdza liczbę i poprawność argumentów oraz wyciąga ze sparsowanych argumentów iternum i len.
;	wejście:	ds:parsed_args - sparsowane argumenty
;	wyjście:	ds:iternum, ds:len
;--------------------------------------------------------------------------------------------------------

	ExtractArgs proc
		push	dx
		push	di
		push	si
		push	cx
		push	bx
		
		xor	bx, bx
		xor	cl, cl
		mov	cl, 10d			;używane do mnożenia, aby liczyć wartość argumentu len
		
		;xor	di, di			
		xor	si, si			;wskaźnik na sparsowane argumenty
		mov	si, offset parsed_args
				
		cmp ds:nr_args, 2d
		JE arg1
			mov al,1d
			call Error			;błąd nieprawidłowej liczby argumentów
			
	   arg1:
		xor	dl, dl				;liczy ile znaków podano jako pierwszy argument
				
		arg1_read:
			inc	si					;zwiększam si bo sparsowane argumenty zaczynaja sie od '$'
			mov	al, ds:[si]			;pobieram kolejne znaki z parsed_arguments aż do napotkania dolara
			cmp	al, '$'				;po napotkaniu '$' przechodzę do następnego argumentu
			je	arg2
			cmp	al, '1'
			jb	arg1_err			;nieprawidłowy pierwszy argument
			cmp	al, '4'
			ja	arg1_err
			inc	dl
			sub	al, 48d
			mov byte ptr ds:[iternum], al		;zapisuje wartość iternum
	    jmp	arg1_read
		
	   arg2:
		cmp	dl, 1d				;sprawdza czy jako pierwszy argument podano tylko jedną cyfrę
		je	arg2_read_init
			arg1_err:
			mov	al, 2d
			call Error			;błąd nieprawidłowego argumentu
	   
		arg2_read_init:
			xor	di, di
			mov	di, offset len
			xor	dx, dx		;wartość argumentu len
		arg2_read:
			inc	si
			mov	al, ds:[si]						;pobieram kolejne znaki z parsed_arguments aż do napotkania dolara
			cmp	al, '$'							;po napotkaniu '$' przechodzę do końca procedury
			je	len_check
			cmp	al, '0'
			jb	arg2_err
			cmp	al, '9'
			ja	arg2_err
			
			
			sub	al, 48d
			push	ax
			mov	ax, dx
			mul	cl			;obecna wartość len * 10,	(ax=al*cl)
			mov	dx, ax
			pop	ax
			xor	ah, ah
			add	dx,	ax 		
		jmp arg2_read
		
	   len_check:
		cmp	dx, 0d
		ja	extract_args_end
			arg2_err:
			mov	al, 3d
			call Error
				
	   extract_args_end:
		mov	word ptr ds:[len], dx
		
		pop bx
		pop	cx
		pop	si
		pop	di
		pop	dx
		ret
	ExtractArgs endp
	
;--------------------------------------------------------------------------------------------------------
;	Iteruje l-system [iternum] razy. W każdej iteracji w miejsce 'F' wstawiany jest ciąg 'F-F++F-F'
;	wejście:	iternum - liczba iteracji l-systemu
;	wyjście:	ds:[moves_1] - tablica zawierająca wygenerowaną sekwencję ruchów
;--------------------------------------------------------------------------------------------------------

	IterateLsystem proc
	push si
	push di
	push cx
	push bx
	
		mov	si, offset moves_1
		mov	di, offset base_string
		mov	cx, 7d
		lsys_init:
			mov	al, byte ptr ds:[di]
			mov	byte ptr ds:[si], al
			inc	di
			inc	si
		loop lsys_init
		
		xor	bl, bl		;bedzie oznaczalo czy teraz będzie iteracja parzysta (0), czy nieparzysta (1)
		xor	cx, cx
		mov	cl, byte ptr ds:[iternum]
		
		iterate_lsys_loop:
			xor	bl, 1d			;zamienia 1 na 0 i vice versa 
			cmp	bl, 1d			;sprawdza czy iteracja parzysta (bl!=1)
			jne	even_iteration
				mov	si, offset moves_1		;tablica z obecnym l-systemem
				mov	di, offset moves_2		;do tej tablicy zapisany zostanie l-system po iteracji
				jmp	iteration_start
			even_iteration:
				mov	si, offset moves_2		;gdy teraz jest parzysta iteracja (2,4) to obecny l-system jest w tabeli moves_2
				mov	di, offset moves_1		;po ukończeniu tej iteracji wynik w moves_1
			
			iteration_start:
				mov	al, byte ptr ds:[si]	;pobieram znak z obecnego l-systemu
				cmp	al, 0d					;gdy trafimy na 0 to znaczy, że obecny l-system się skończył
				je	next_iteration
				cmp	al, 'F'						
				jne	notF					
					call InsertString		;w miejsce F wstawia 'F-F++F-F'
					inc si
					jmp	iteration_start
				notF:
				mov	byte ptr ds:[di], al	;przepisuje + lub - do drugiej tablicy
				inc	si
				inc	di
			jmp	iteration_start
				
			next_iteration:
		loop iterate_lsys_loop
		
		cmp	bl, 0d			;gdy liczba iteracji parzysta to wynik w moves_1
		je	iterate_lsys_end
		mov	si, offset moves_1
		mov	di, offset moves_2
		copy_table:			;w przeciwnym wypadku wynik z tabeli moves_2 zostaje przeniesiony do moves_1	
			mov	al, byte ptr ds:[di]
			cmp	al, 0d
			je	iterate_lsys_end
			mov	byte ptr ds:[si], al
			inc	si
			inc	di
			jmp	copy_table
			
		iterate_lsys_end:
	
	pop bx
	pop cx
	pop di
	pop si
	ret
	IterateLsystem endp
	
	InsertString proc
	push si
	push cx
	
		mov	cx, 8d
		mov	si, offset insert_string
		insert_string_loop:
			mov	al, byte ptr ds:[si]
			mov	byte ptr ds:[di], al
			inc	si
			inc	di
		loop insert_string_loop
		
	pop cx
	pop si
	ret
	InsertString endp

;--------------------------------------------------------------------------------------------------------
;	Inicjalizacja trybu graficznego
;--------------------------------------------------------------------------------------------------------

	EnterGraphicMode proc
	push ax
	
		mov ah, 0h			;funkcja uruchomienia trybu graficznego
		mov al, 12h			;tryb graficzny 320x200 w 256 kolorach
		int 10h				
		
	pop ax
	ret
	EnterGraphicMode endp
	
;--------------------------------------------------------------------------------------------------------
;	Wyjście z trybu graficznego
;--------------------------------------------------------------------------------------------------------

	ExitGraphicMode proc
	push ax
	
	exit_loop:
		mov	ah, 0h		;oczekiwanie na naciśnięcie klawisza
		int 16h
		cmp	ah, 1h		;czy naciśnięto esc
		jne	exit_loop
	
	mov ah, 0h
	mov al, 3h		; tryb tekstowy
	int 10h			; przejście w tryb tekstowy
	
	pop ax
	ret
	ExitGraphicMode endp
	
	ExitGraphicMode2 proc
	push ax
	
	mov ah, 0h
	mov al, 3h		; tryb tekstowy
	int 10h			; przejście w tryb tekstowy
	
	pop ax
	ret
	ExitGraphicMode2 endp
	
;--------------------------------------------------------------------------------------------------------
;	Rysuje krzywą Kocha
;	wejście:	ds:[moves_1] - tablica zawierająca sekwencję ruchów
;				ds:[len] - długość pojedynczego odcinka
;--------------------------------------------------------------------------------------------------------

	Draw proc
	push si
	push dx
		
		call	EnterGraphicMode
		
		mov	si, offset moves_1
		finit     
		
		fldpi		;załaduj pi
		fild word ptr ds:[three]
		fdivp	st(1), st			;st(0)=pi/3
		
		fldz								;st(0)=0 (kąt początkowy); st(1)=pi/3
		fild word ptr ds:[y1]
		fild word ptr ds:[x1]				;st(0)=x1 ; st(1)=y1 ; st(2)=angle ; st(3)=pi/3
		
		draw_loop:
			mov	al, byte ptr ds:[si]
			cmp	al, 'F'
			jne	check_plus			
				fld	st(2)				;kopia angle na wierzch
				fsincos					;st(0)=cos ; st(1)=sin ; st(2)=x1 ; st(3)=y1 ; st(4)=angle ; st(5)=pi/3
				fild word ptr ds:[len]			;st(0)=len ; st(1)=cos ; st(2)=sin ; st(3)=x1 ; st(4)=y1 ; st(5)=angle ; st(6)=pi/3
				fmulp st(1), st					;st(0)=len*cos ; st(1)=sin ; st(2)=x1 ; st(3)=y1 ; st(4)=angle ; st(5)=pi/3
				faddp st(2), st					;x2 = len * cos + x1
												;st(0)=sin ; st(1)=x2 ; st(2)=y1 ; st(3)=angle ; st(4)=pi/3
				
				fild word ptr ds:[len]			;st(0)=len ; st(1)=sin ; st(2)=x2 ; st(3)=y1 ; st(4)=angle ; st(5)=pi/3
				fmulp st(1), st					;st(0)=len*sin ; st(1)=x2 ; st(2)=y1 ; st(3)=angle ; st(4)=pi/3
				faddp st(2), st					;st(0)=x2 ; st(1)=y2 ; st(2)=angle ; st(3)=pi/3
				
				fld	st(0)
				frndint
				fistp word ptr ds:[x2]
				fld st(1)
				frndint
				fistp word ptr ds:[y2]
				
				call DrawLine
				
				mov	ax, word ptr ds:[x2]			
				mov	word ptr ds:[x1], ax		;zapisuje [x2] jako [x1] - początek dla rysowania następnej linii
				mov	ax, word ptr ds:[y2]			
				mov	word ptr ds:[y1], ax		;zapisuje [y2] jako [y1] - początek dla rysowania następnej linii				
				
				inc	si
				jmp	draw_loop
				
			check_plus:
			cmp	al, '+'
			jne	check_minus
				fxch st(2)
				fadd st, st(3)			;angle + pi/3
				fxch st(2)
				inc	si
				jmp	draw_loop
			check_minus:
			cmp	al, '-'
			jne	draw_end
				fxch st(2)
				fsub st, st(3)			;angle - pi/3
				fxch st(2)
				inc	si
				jmp	draw_loop
		
		draw_end:
			call ExitGraphicMode
	
	pop dx
	pop si
	ret
	Draw endp
	
;--------------------------------------------------------------------------------------------------------
;	Rysuje linię z punktu [x1,y1] do punktu [x2,y2] korzystając z algorytmu Bresenhama
;	wejście:	[x1],[y1],[x2],[y2] - współrzędne punktów między którymi rysujemy linię
;--------------------------------------------------------------------------------------------------------

	DrawLine proc
	push dx
	push bx
	push cx
	push di
	push si
	push ax
	
		xor	dx, dx
		xor	bx, bx
		
		mov ax, word ptr ds:[x2] 	;ax = x2
		sub ax, word ptr ds:[x1] 	;ax = x2 - x1
		cmp	ax, 0d
		jge dx_gt_0
			neg	ax
		dx_gt_0:
		mov word ptr ds:[dist_x], ax 	;dist_x = abs(x2 - x1)
		
		mov ax, word ptr ds:[y2] 	;ax = y2
		sub ax, word ptr ds:[y1] 	;ax = y2 - y1
		cmp	ax, 0d
		jge dy_gt_0
			neg	ax
		dy_gt_0:
		mov word ptr ds:[dist_y], ax 	;dist_y = abs(y2 - y1)
		
		;określamy krok w kierunku x (kx = czy poruszamy się w lewo, czy prawo); zapisany do kx
		mov	ax, word ptr ds:[x1]
		mov	bx, word ptr ds:[x2]
		xor	dx, dx
		cmp	ax, bx			
		jne	check_x1_x2		;gdy x1=x2 to kx wynosi 0
			jmp	x1_less_than_x2
		check_x1_x2:
		mov	dx, 1d			;gdy x1 < x2 krok w kierunku x wynosi +1
		cmp	ax, bx
		jl	x1_less_than_x2		;czy x1 < x2
			neg	dx			;gdy x1 > x2 to musimy poruszać się w lewo, czyli krok wynosi -1
		x1_less_than_x2:
		mov	word ptr ds:[kx], dx
		
		
		;określamy krok w kierunku y (ky = czy poruszamy się w dół, czy górę); zapisany do ky
		mov	ax, word ptr ds:[y1]
		mov	bx, word ptr ds:[y2]
		xor	dx, dx
		cmp	ax, bx
		jne	check_y1_y2		;gdy y1=y2 to ky wynosi 0
			jmp	y1_less_than_y2
		check_y1_y2:
		mov	dx, 1d			;gdy y1 < y2 krok w kierunku y wynosi +1
		cmp	ax, bx			
		jl y1_less_than_y2	;czy y1 < y2
			neg	dx			;gdy y1 > y2 to musimy poruszać się w górę, czyli krok wynosi -1
		y1_less_than_y2:
		mov	word ptr ds:[ky], dx
		
		mov si, word ptr ds:[x1]	;si = współrzędna x
		mov	di, word ptr ds:[y1]	;di = współrzędna y
		call	DrawPoint			;zapala piksel [x1,y1]
		
		mov	ax, word ptr ds:[dist_x]
		mov	bx, word ptr ds:[dist_y]
		cmp	ax, bx
		jle	dx_be_dy			;jeśli dist_x <= dist_y to skocz do wersji algorytmu z przestawionymi współrzędnymi
		fild word ptr ds:[dist_y]
		fisub word ptr ds:[dist_x]	;st(0) = dist_y - dist_x 
		fimul word ptr ds:[two]		;st(0) = (dist_y - dist_x) * 2		(ai)
		fild word ptr ds:[dist_y]
		fimul word ptr ds:[two]		;st(0) = dist_y * 2					(bi)
									;st(1) = (dist_y - dist_x) * 2		(ai)
		fld st(0)
		fisub word ptr ds:[dist_x]	;st(0) = 2 * dist_y - dist_x		(d)
									;st(1) = dist_y * 2					(bi)
									;st(2) = (dist_y - dist_x) * 2		(ai)
		
		draw_line_1:
			ftst              	;porównuje st(0) z 0
			fstsw ax          	;kopiuje Status Word zawierające wynik do ax
			fwait             	;zapewnia że poprzednia instrukcja się wykonała
			sahf              	;przenosi ah do flag procesora
			jpe error_handler 	;jump if parity equal - wynik porównania nieprawidłowy	
			jb	draw_line1_case2
				;if(d >= 0)
				add	si, word ptr ds:[kx]
				add	di, word ptr ds:[ky]
				fadd st(0), st(2) 			;d += ai
				jmp after_dl1_case2
			draw_line1_case2:
				;(d < 0)
				fadd st(0), st(1)			;d += bi
				add	si, word ptr ds:[kx]
			after_dl1_case2:
			
			call	DrawPoint			
			cmp	si, word ptr ds:[x2]
			jne	draw_line_1
			
		jmp	draw_line_end
			
	;-----------------------------------------------------------------------------------------	
			
		dx_be_dy:			;dist_x <= dist_y
		fild word ptr ds:[dist_x]
		fisub word ptr ds:[dist_y]	;st(0) = dist_x - dist_y 
		fimul word ptr ds:[two]		;st(0) = (dist_x - dist_y) * 2		(ai)
		fild word ptr ds:[dist_x]
		fimul word ptr ds:[two]		;st(0) = dist_x * 2					(bi)
									;st(1) = (dist_x - dist_y) * 2		(ai)
		fld st(0)
		fisub word ptr ds:[dist_y]	;st(0) = 2 * dist_x - dist_y		(d)
									;st(1) = dist_x * 2					(bi)
									;st(2) = (dist_x - dist_y) * 2		(ai)
									
		draw_line_2:
			ftst              	;porównuje st(0) z 0
			fstsw ax          	;kopiuje Status Word zawierające wynik do ax
			fwait             	;zapewnia że poprzednia instrukcja się wykonała
			sahf              	;przenosi ah do flag procesora
			jpe error_handler 	;jump if parity equal; wynik porównania nieprawidłowy	
			jb	draw_line2_case2
				;if(d >= 0)
				add	si, word ptr ds:[kx]
				add	di, word ptr ds:[ky]
				fadd st(0), st(2) 			;d += ai
				jmp	after_dl2_case2
			draw_line2_case2:
				;(d < 0)
				fadd st(0), st(1)
				add	di, word ptr ds:[ky]
			
			after_dl2_case2:
			
			call	DrawPoint
			cmp	di, word ptr ds:[y2]
			jne	draw_line_2
		
		jmp	draw_line_end
		
		error_handler:
			call ExitGraphicMode2
			mov	al, 4d
			call Error
		draw_line_end:
			fistp word ptr ds:[trash]
			fistp word ptr ds:[trash]
			fistp word ptr ds:[trash]
			
	pop	ax
	pop si
	pop di
	pop cx
	pop bx
	pop dx
	ret
	DrawLine endp
	
;--------------------------------------------------------------------------------------------------------
;	Rysuje punkt o współrzędnych [si,di]
;--------------------------------------------------------------------------------------------------------
	
	DrawPoint proc
	push cx
	push dx
	push ax
	
		mov	cx, si		;x
		cmp	cx, 639d
		jg	no_screen
		cmp	cx, 0d
		jl	no_screen
		mov	dx, di		;y
		cmp	dx, 479d
		jg	no_screen
		cmp	dx, 0d
		jl	no_screen
		mov	al, 1100b	;kolor
		mov	ah, 0Ch
		int	10h
	
		no_screen:
		
	pop ax
	pop dx
	pop cx
	ret
	DrawPoint endp

;--------------------------------------------------------------------------------------------------------
;	Obsługa błędów
;	wejście:	al-numer błędu
;--------------------------------------------------------------------------------------------------------
	
	Error proc
		
		xor	cx, cx
		mov	cl, dl
		
		cmp	al, 1d
		jne	error2
		mov	dx, offset wrong_args_err
		jmp	err_end
	  error2:
		cmp	al, 2d
		jne	error3
		mov dx, offset wrong_1arg_err
		jmp	err_end
	  error3:
		cmp	al, 3d
		jne error4
		mov dx, offset wrong_2arg_err
		jmp	err_end
	  error4:
		cmp	al, 4d
		jne	error5
		mov	dx, offset comparison_err
		jmp	err_end
	  error5:
	  err_end:
		call printNL
		mov	ah,	9h
		int	21h
		
		mov	ah,4ch
		int	21h
	Error endp
	
	
  
CODE1 ends


STOS1 	segment stack
	dw	200	dup(?)
top1 	dw	?
STOS1	ends


end START	