DATA1 segment
	parsed_args		db	128 dup(?)
	parsed_args_length	db	?
	nr_args			db 	?
	decompression	db	?
	input	 		db 	127	dup(?)
	output			db 	127	dup(?)
	input_handle	dw 	?
	output_handle	dw 	?
	input_buffer	db	4000h	dup(?)
	output_buffer	db	4000h	dup(?)
	buffer_size		dw	4000h				;16kB
	
	err_nr_info		db 	" Kod bledu: $"
	args_info		db	"Liczba argumentow: $"
	new_line		db	10, 13, '$'
	open_input_err	db	"ERROR: Blad przy otwieraniu pliku wejsciowego. $"
	open_output_err	db	"ERROR: Blad przy otwieraniu pliku wyjsciowego. $"
	writing_err		db	"ERROR: Blad przy zapisywaniu do pliku.$"
	closing_err		db	"ERROR: Blad przy zamykaniu plikow.$"
	read_file_err	db	"ERROR: Blad przy czytaniu pliku wejsciowego.$"
	write_file_err	db	"ERROR: Blad przy zapisie do pliku wyjsciowego.$"
	wrong_opt_err	db	"ERROR: Podano nieprawidlowa opcje.", 10, 13, "Dozwolone opcje: -d$"
	wrong_args_err	db	"ERROR: Podano nieprawidlowa liczbe argumentow. Nalezy podac nazwy 2 plikow.$"
  
DATA1 ends

CODE1 segment

  .286

START:
  	assume cs: CODE1, ss:STOS1, ds:DATA1
	
	mov ax, DATA1
  	mov ds, ax  
  
	mov ax, seg top1				;ustawienie stosu
    mov ss, ax
    mov sp, offset top1
	
	call ParseArgs
	;call PrintArgs
	call ExtractArgs
	;call PrintNames
	call OpenFiles
	cmp	byte ptr [decompression], 0d
	je	main_compression
		call	Decompress
		jmp	main_end
	main_compression:
		call	Compress
	main_end:
	call CloseFiles
	
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
;	Procedura wypisująca argumenty i ich liczbę - 
;--------------------------------------------------------------------------------------------------------

  PrintArgs proc
	push	di
	push    si
	push	dx
	push	cx
	push	ax
	
	xor	si, si
	mov	si, offset parsed_args
	
	xor	cx, cx
	mov	cl, al			;cl - długość scalonych argumentów (zawiera $)	
  	mov	ch, ah			;ch - liczba argumentów
  	
  	mov	ah, 9h
	mov	dx, offset args_info
	int	21h
   
	mov	dl, ch
	add	dl, 48d 		;dodaje kod ascii '0' do liczby argumentów
	call	printC		;wypisuje liczbę argumentów
	
	xor	ch, ch			;zeruje ch, będzie liczyło ile znaków już wypisano
	dec	si
   new_l:
    inc	ch				
    inc	si
    call	printNL
   printargs_begin:		
   	mov	dl, [si]
	cmp	dl, '$'		;jeśli napotka znak '$' skończył się argument i przechodzi do nowego wiersza
	je	new_l
	call	printC
	inc	si
	inc 	ch
	cmp	ch, cl
	jna	printargs_begin	;jeśli nie wypisano wszystkich znaków to skacze do printargs_begin
	 
	pop	ax
	pop	cx
	pop	dx
	pop 	si
	pop 	di
	ret
  PrintArgs endp

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
;	Wypisuje jeden znak z al
;--------------------------------------------------------------------------------------------------------

  printC2 proc	
	push ax
	push dx
	
	mov dl, al
	mov	ah,2h
	int	21h
	
	pop	dx
	pop ax
	ret
  printC2 endp	
  
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
  
 
 
 
	PrintNames proc
		push	si
		push	dx
		push	ax
		
			call printNL
			xor si, si
			mov	si, offset input
			print1:
			mov	al, ds:[si]
			cmp al, 0d
			je print1e
			call printC2
			inc si
			jmp print1
			
			print1e:
			call printNL
			
			xor si, si
			mov	si, offset output
			print2:
			mov	al, ds:[si]
			cmp al, 0d
			je print2e
			call printC2
			inc si
			jmp print2
			
			print2e:
			call printNL
			mov	al, byte ptr [decompression]
			add	al, 48d
			call printC2
			
		pop	ax
		pop	dx
		pop	si
		ret
	PrintNames endp
;--------------------------------------------------------------------------------------------------------
;	Sprawdza liczbę argumentów i wyciąga ze sparsowanych argumentów nazwy plików.
;	wejście:	ds:parsed_args - sparsowane argumenty
;	wyjście:	ds:input, ds:output 
;--------------------------------------------------------------------------------------------------------

	ExtractArgs proc
		push	dx
		push	di
		push	si
		push	cx
		
		xor	cl, cl
		xor	di, di
		xor	si, si
		mov	si, offset parsed_args
				
		cmp ds:nr_args, 2d
		JE arg2
		cmp ds:nr_args, 3d
		JE arg1
			mov al,6d
			call Error			;błąd nieprawidłowej liczby argumentów
			
	   arg1:
		xor	dl, dl
		mov	di, offset decompression
		inc	si					;zwiększam si bo sparsowane argumenty zaczynaja sie od '$'
		
		mov	al, ds:[si]			;pobieram kolejne znaki z parsed_arguments aż do napotkania dolara aby sprawdzić czy podano opcję dekompresji
		cmp	al, '-'				;jeśli pierwszym znakiem argumentu jest '-' to podano opcję
		je	optiontext_start
			mov	al, 7d
			call Error			;błąd nieprawidłowej opcji
	   optiontext_start:
		inc	cl				;ile podano opcji
	   optiontext:
		inc	dl				;długość podanej opcji (musi być 2)
		inc si
		mov	al, ds:[si]
		cmp	al, '$'			;po napotkaniu '$' przechodzę do następpnego argumentu
		je	arg2
		cmp	al, 'd'
		jne	optiontext_end
			mov byte ptr ds:[di], 1d		;jeśli podano opcję dekompresji zmienna decompression przyjmie wartość 1
	   optiontext_end:
		jmp optiontext
		
	   arg2:
		cmp	cl, 0d
		je	arg2_read			;jeśli w pierwszym argumencie nie podano żadnej opcji to czyta drugi argument
		cmp	dl, 2d
		je	arg2_read
			mov byte ptr ds:[di], 0d
			mov	al, 7d
			call Error			;błąd nieprawidłowej opcji
	   
	   arg2_read:
		xor	di, di
		mov	di, offset input
		inc	si
	   arg2_read_b:
		mov	al, ds:[si]						;pobieram kolejne znaki z parsed_arguments aż do napotkania dolara i zapisuję do zmiennej input
		cmp	al, '$'							;po napotkaniu '$' przechodzę do następpnego argumentu
		je	arg3
		mov ds:[di], al
		inc	di
		inc	si
		jmp arg2_read_b
		
	   arg3:
		inc si
		inc	di
		mov byte ptr ds:[di],0d				;znak końca pliku dla input
		xor	di, di
		mov	di, offset output
	   arg3_read:
		mov	al, ds:[si]						;pobieram kolejne znaki z parsed_arguments aż do napotkania dolara i zapisuję do zmiennej output
		cmp	al, '$'
		je	extract_args_end
		mov ds:[di], al
		inc	di
		inc	si
		jmp arg3_read
		
		
	   extract_args_end:
		inc	di
		mov byte ptr ds:[di],0d						    ; znak końca pliku dla output
		
		pop	cx
		pop	si
		pop	di
		pop	dx
		ret
	ExtractArgs endp
	

;--------------------------------------------------------------------------------------------------------
;	Otwiera pliki.
;	wejście:	ds:input, ds:output - pliki podane jako argumenty
;	wyjście:	ds:input, ds:output 
;--------------------------------------------------------------------------------------------------------
	
	OpenFiles proc
	push ax
	push dx
	push cx
	
		mov	ah, 3Dh					;otwieranie pliku
		mov	al, 00100000b			;0 (brak dziedziczenia) 010 (zabroń innym zapisu) 0 (bit zawsze 0) 000 (możliwy tylko odczyt)
		mov	dx, offset input		;adres nazwy pliku
		int	21h						
		jnc	file1opened					;sprawdzamy, czy nie ma błędu.
			xor	dx, dx
			mov	dx, ax
			add dl, 48d
			mov al,1d
			call Error	
		file1opened:
		mov word ptr [input_handle],ax		;zapisanie uchwytu do pliku
		
		
		mov	ah, 3Ch					;tworzenie pliku
		mov	dx, offset output
		xor	cx, cx					;atrybuty, cx (bity 0-2):	0(tylko do odczytu), 0(plik nieukryty), 0(plik nie systemowy), 
									;atrybuty, cx (bity 3-5):	0(etykieta woluminu), 0(zawsze 0), 0(plik nie archiwalny)
		int	21h
		jnc	outfilecreated
			xor	dx, dx
			mov	dx, ax
			add dl, 48d
			mov	al, 2d
			call Error
		outfilecreated:
		mov	word ptr [output_handle], ax

	pop cx
	pop dx
	pop ax
	ret
	OpenFiles endp
	
	

;--------------------------------------------------------------------------------------------------------
;	Zamyka pliki.
;--------------------------------------------------------------------------------------------------------

	CloseFiles proc 
	push ax
	push bx
	
		mov bx, word ptr [input_handle]
		mov ah,3Eh							;zamknięcie pliku
		int 21h
		jnc closefiles_error
			mov al,3d
			call Error
		closefiles_error:
		
		mov bx, word ptr [output_handle]
		mov ah,3Eh							;zamknięcie pliku
		int 21h
		jnc closefiles_end
			xor	dx, dx
			mov	dx, ax
			add dl, 48d
			mov al,3d
			call Error
		closefiles_end:
		
	
	pop bx
	pop ax
	ret
	CloseFiles endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Kompresuje plik.
;	wejście:	ds:buffer_size - rozmiar bufora
;--------------------------------------------------------------------------------------------------------

	Compress proc
	push si
	push di
	push bx
	push cx
	
		xor	di, di
		xor	cx, cx
		xor	bx, bx			;bx - używane do kontroli czy przeczytano cały bufor i czy plik wejściowy się skończył
		inc	cl				;cl - liczy ilość powtórzonych bajtów, początkowa wartość to 1
		call	GetChar		;odczytany bajt znajduje się w al
		mov	ah, al			;w ah będzie przechowywany ostatni bajt
		compress_loop:
			call	GetChar
			cmp	bx, 0d		;jeśli nie wczytano żadnych danych (EOF) to wyskocz z pętli
			je	compress_end
			dec	bx			
			cmp	ah,	al		;porównanie obecnego bajtu z poprzednim
			jne	not_equal
				call	EqualBytes		;procedura licząca ilość powtórzonych bajtów
				jmp		compress_loop_end
			not_equal:
				cmp	cl, 3d		;czy było więcej niż 3 takich samych bajtów
				ja	more_than_3
					call	LessThan3
					jmp		compress_loop_end
			more_than_3:
				call	MoreThan3
			compress_loop_end:
			mov	ah, al
		jmp	compress_loop
		
		compress_end:		
			cmp	di, 0d				;di - ile bajtów wpisano do bufora zapisu
			je	no_flush			;jeśli bufor jest pusty nie trzeba robić flusha
				call	Flush
			no_flush:
	
	pop	cx
	pop bx
	pop di
	pop si
	ret
	Compress endp
	
	
	EqualBytes proc
		cmp	cl, 255d				;jeśli licznik powtórzeń osiągnął limit zapisuje skompresowany ciąg w buforze zapisu
		jne	equal_bytes_end
			call	MoreThan3
			ret
		equal_bytes_end:
		inc	cl
	ret
	EqualBytes endp
	
	MoreThan3 proc
		push ax
		mov	ah, 0d				;0x00 do pliku wyjściowego
		call	PutChar
		mov	ah, cl				;ile razy wystąpił powtarzający się bajt
		call	PutChar
		pop	ax					;ah - jaki bajt się powtarzał
		call	PutChar
		
		xor	cx, cx
		inc	cl
	ret
	MoreThan3 endp
	
	LessThan3 proc
		cmp	ah, 0d
		jne lessthan3_not_zero
			zero_loop:
				call	PutChar
				call	PutChar
			loop	zero_loop
			jmp	lessthan3_end
		lessthan3_not_zero:
			notzero_loop:
				call	PutChar
			loop	notzero_loop
		lessthan3_end:
			xor	cx, cx
			inc	cl
			
	ret
	LessThan3 endp
	
	
	Flush proc
	
		mov	ax, di
		call	WriteFile
		
	ret
	Flush endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Dekompresuje plik.
;	wejście:	ds:buffer_size - rozmiar bufora
;--------------------------------------------------------------------------------------------------------

	Decompress proc
	push si
	push di
	push cx
	push bx
	
		xor	di, di			;di - liczy ile bajtów zapisano do bufora zapisu
		xor	si, si			;si - liczy ile danych odczytano z obecnego bufora
		xor	bx, bx			;bx - używane do kontroli czy przeczytano cały bufor i czy plik wejściowy się skończył
		decompress_loop:
			call	GetChar		;odczytany bajt znajduje się w al
			cmp	bx, 0d			;jeśli nie wczytano żadnych danych (EOF) to wyskocz z pętli
			je	decompress_end
			dec	bx				;bx wskazuje ile bajtów zostało do odczytania z bufora
			mov	ah, al
			cmp	ah, 0d			;jeśli natrafiono na 0x00 to może to być znak modyfikacji
			jne	not_special
				call	GetChar
				cmp	bx, 0d		;jeśli nie wczytano żadnych danych (EOF) to wyskocz z pętli
				je	decompress_end
				dec	bx
				cmp	al, 0d				;jeśli po 0x00 nie występuje 0x00 to natrafiono na skompresowane dane
				jne	compressed
					call	PutChar		;0x00 0x00 = 0x00
					jmp	decompress_loop_end
				compressed:
					mov	ah, al				;ah - ile razy powtórzył się bajt
					call	GetChar			;al - bajt, który się powtarzał
					cmp	bx, 0d		;jeśli nie wczytano żadnych danych (EOF) to wyskocz z pętli
					je	decompress_end
					dec	bx
					xor	cx, cx
					mov	cl, ah				;cx - ile razy bajt się powtarzał tyle razy trzeba go wpisać do bufora zapisu
					mov	ah, al				
					compressed_loop:
						call	PutChar
					loop	compressed_loop
					jmp	decompress_loop_end
			not_special:
				call PutChar
				
			decompress_loop_end:
		jmp	decompress_loop
		
		decompress_end:
			cmp	di, 0d				;di - ile bajtów wpisano do bufora zapisu
			je	no_flush			;jeśli bufor jest pusty nie trzeba robić flusha
				call	Flush
			no_flush:
	
	pop bx
	pop	cx
	pop	di
	pop si
	ret
	Decompress endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Pobiera bajt z bufora odczytu. W przypadku gdy odczytano cały bufor, ładuje do niego nowe dane.
;	wejście:	si - ilość odczytanych bajtów z obecnego bufora
;	wyjście:	al - odczytany bajt z bufora
;--------------------------------------------------------------------------------------------------------

	GetChar proc
	
		cmp	bx, 0d								;bx - ile bajtów zostało do odczytania z bufora
		jne	get_char_end						;jeśli odczytano już cały bufor to trzeba załadować nowe dane (wywołanie ReadFile) 
			push ax
			xor	si, si
			call	ReadFile
			mov	bx, ax							;bx - ile bajtów załadowano z pliku wejściowego
			pop ax
		
		get_char_end:
		mov	al, byte ptr [input_buffer + si]
		inc	si
	
	ret
	GetChar endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Odczytuje dane z pliku i ładuje je do bufora odczytu.
;	wejście:	ds:input_handle - uchwyt do pliku wejściowego
;				ds:input_buffer - bufor na dane odczytane z pliku wejściowego
;				ds:buffer_size - rozmiar bufora
;	wyjście:	ds:input_buffer - odczytane dane
;				ax - ilość odczytanych danych 
;--------------------------------------------------------------------------------------------------------

	ReadFile proc
	push bx
	push dx
	push cx
		
		mov	bx, word ptr [input_handle]				;uchwyt pliku do odczytu
		mov	cx, word ptr [buffer_size]		;ile bajtów odczytać
		mov	dx, offset input_buffer			;gdzie zapisać odczytane dane
		mov	ah, 3Fh			;odczyt danych z pliku			
		int	21h
		jnc	read_file_end
			xor	dx, dx
			mov	dx, ax
			add dl, 48d
			mov	al, 4d
			call Error
		read_file_end:
		
	pop cx
	pop dx
	pop bx
	ret
	ReadFile endp
	
;--------------------------------------------------------------------------------------------------------
;	Zapisuje znak z ah do output_buffer.
;	wejście:	ah - znak do zapisania w output_buffer
;				ds:output_buffer - bufor na dane dla pliku wyjściowego
;				ds:buffer_size - rozmiar bufora
;				di - ilość bajtów zapisanych do output_buffer
;--------------------------------------------------------------------------------------------------------

	PutChar proc
	push ax
	
		mov	byte ptr [output_buffer + di], ah
		inc	di
		
		cmp	di, word ptr [buffer_size]
		jne	put_char_end
			mov	ax, word ptr [buffer_size]
			call WriteFile
			xor	di, di
		put_char_end:
	
	pop ax
	ret
	PutChar endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Zapisuje output_buffer do pliku.
;	wejście:	ds:output_handle - uchwyt do pliku wyjściowego
;				ds:output_buffer - bufor na dane do zapisu dla pliku wyjściowego
;				ax - ilość bajtów do zapisania
;--------------------------------------------------------------------------------------------------------

	WriteFile proc
	push bx
	push cx
	push dx
	
		mov	bx, word ptr [output_handle]
		mov	cx, ax
		mov	dx, offset output_buffer
		mov	ah, 40h
		int	21h
		jnc	write_file_check
			xor	dx, dx
			mov	dx, ax
			add dl, 48d
			mov	al, 5d
			call	Error
		write_file_check:
		cmp	cx, ax				;sprawdza czy ilość danych rzeczywiście zapisanych do pliku jest równa ilości, która powinna być zapisana
		je	write_file_end
			xor dx, dx
			add dl, 48d
			mov	al, 5d
			call	Error
		write_file_end:
			
	pop dx
	pop cx
	pop bx
	ret
	WriteFile endp
  
;--------------------------------------------------------------------------------------------------------
;	Obsługa błędów
;	wejście:	al-numer błędu
;--------------------------------------------------------------------------------------------------------
	
	Error proc
		
		xor	cx, cx
		mov	cl, dl
		
		cmp	al, 1d
		jne	error2
		mov	dx, offset open_input_err
		jmp	err_end
	  error2:
		cmp	al, 2d
		jne	error3
		mov dx, offset open_output_err
		jmp	err_end
	  error3:
		cmp	al, 3d
		jne error4
		mov dx, offset closing_err
		jmp	err_end
	  error4:
		cmp al, 4d
		jne error5
		mov dx, offset read_file_err
		jmp err_end
	  error5:
		cmp al, 5d
		jne error6
		mov dx, offset write_file_err
		jmp err_end
	  error6:
		cmp al, 6d
		jne error7
		mov dx, offset wrong_args_err
		jmp err_end
	  error7:
		cmp al, 7d
		jne error8
		mov dx, offset wrong_opt_err
		jmp err_end
	  error8:
	  err_end:
		call printNL
		call printNL
		mov	ch, al
		mov	ah,	9h
		int	21h
		
		mov	al, ch
		cmp	al, 5d
		ja	err_end2
		mov	dx, offset err_nr_info
		mov	ah, 9h
		int	21h
		mov	ah, 2h
		mov	dl,	cl
		int	21h
		
	   err_end2:
		mov	ah,4ch
		int	21h
	Error endp
	
	
  
CODE1 ends


STOS1 	segment stack
	dw	200	dup(?)
top1 	dw	?
STOS1	ends


end START	