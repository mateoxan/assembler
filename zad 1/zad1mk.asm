DATA1 segment
  parsed_args	db	128 dup(?)
  key			db	16 dup(?)
  moves 		db 	64 dup(?)
  chessboard	db	153 dup(0d)		;tablica z ilo�ci� odwiedzin
  fingerprint	db	153 dup(0d)		;tablica z ilo�ci� odwiedzin zamienion� na symbole
  version		db	?
  parsed_args_length	db	?
  nr_args		db 	?
  chbl			db	17d		;d�ugo�� szachownicy
  
  new_line		db	10, 13, '$'
  border		db	"+-----------------+",10,13,'$'
  symbols		db	' ','.','o','+','=','*','B','O','X','@','%','&','#','/','^'
  
  args_info		db	"Liczba argumentow: $"
  
  wrong_quantity_err	db	"ERROR: Nieprawidlowa liczba argumentow.", 10, 13, "Nalezy podac:", 10, 13, " wersje programu", 10, 13, 9, "0 - bez modyfikacji", 10,13,9, "1 - dwoch goncow", 10, 13, 9, "2 - przeciwny kierunek analizy par bitow", 10, 13, " klucz", 10, 13, 9, "16 dwuznakowych liczb hex oddzielonych ':'$"
  wrong_version_err	db	"ERROR: Nieprawidlowa wersja.", 10, 13, "Dostepne wersje:", 10, 13, 9, "0 - bez modyfikacji", 10,13,9, "1 - dwoch goncow", 10, 13, 9, "2 - przeciwny kierunek analizy par bitow$"
  wrong_key_err	db	"ERROR: Nieprawidlowy klucz.", 10, 13, "Nalezy podac klucz w postaci 16 dwuznakowych liczb hex oddzielonych ':'$"
  wrong_length_err	db	"ERROR: Nieprawidlowa dlugosc argumentow.", 10, 13, "Nalezy podac:", 10, 13, " wersje programu", 10, 13, 9, "0 - bez modyfikacji", 10,13,9, "1 - dwoch goncow", 10, 13, 9, "2 - przeciwny kierunek analizy par bitow", 10, 13, " klucz", 10, 13, 9, "16 dwuznakowych liczb hex oddzielonych ':'$"
  wrong_hex_err	db	"ERROR: Podano argument w nieprawidlowej postaci.", 10, 13, "Dozwolone znaki: 0-9, a-f, A-F.$"
  conversion_err	db	"ERROR: Blad przy konwersji liczby do postaci binarnej.$"
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
	call PrintArgs
	call CheckArgs
	call ExtractVersion
	call ExtractKey
	call CreateMoves
	call Move
	call ToSymbols
	call PrintChessboard
	
	mov	ax,04c00h
	int	21h
;--------------------------------------------------------------------------------------------------------
;	Procedura scalaj�ca argumenty z linii polece� (zapisuje je jako jeden ci�g znak�w, 
;	poszczeg�lne argumenty rozdzielone znakami '$') 
;	wyj�cie: 	al-d�ugo�� scalonych argument�w
;				ah-liczba argument�w 
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
	
	mov	di, offset parsed_args			;robi miejsce na tablic� znak�w z linii polece�
	
	mov	ah, 62h			;pobiera adres Program Segment Prefix (PSP)
	int	21h				;zwraca adres PSP w BX
	mov	es, bx			;przenosi adres PSP do es  

	mov	si, 81h			;adres pierwszego znaku z linii polece�
   parse:  
	call 	SkipWhiteSpaces	;zjada bia�e znaki
	cmp		ah, 0d			;gdy nie by�o bia�ych znak�w to znajdujemy si� w tym samym argumencie
	je		wyraz			;skocz do 'wyraz'
	
	cmp		al, 13d 		;czy znak ko�ca linii (enter)
	je		koniec_linii	;je�li tak skocz do 'koniec_linii' (ko�czy procedur�)
	
	inc 	ch						;je�li by�y bia�e znaki i nie jest to koniec linii to znaczy �e napotkali�my nowy argument
	mov 	byte ptr ds:[di], '$'	;wstawia znak dolara, kt�ry oddziela od siebie argumenty
	inc 	di
	inc		dl					;zwi�ksza d�ugo�� scalonych argument�w
	
	wyraz:
		cmp	al, 13d 		;czy znak ko�ca linii (enter)
		je	koniec_linii	;je�li tak skocz do 'koniec_linii' (ko�czy procedur�)
		mov	ds:[di], al
		inc	di
		inc	dl				;zwi�ksza d�ugo�� scalonych argument�w
		inc 	si				;bierze kolejny znak z linii polece� i skacze do 'parse' aby go sprawdzi�
		jmp 	parse	
	
	koniec_linii:
		mov	al, dl				;po wykonaniu w al znajduje si� d�ugo�� scalonych argument�w
		mov	ah, ch				;w ah znajduje si� liczba argument�w
		mov	byte ptr parsed_args_length, al
		mov	byte ptr nr_args, ch
	pop		cx
	pop		si
	pop		es
	pop		di
	ret
  ParseArgs endp
	
;--------------------------------------------------------------------------------------------------------
;	Procedura 'zjadaj�ca' bia�e znaki
;	wyj�cie:	al-niebia�y znak
;				ah-liczba pomini�tych bia�ych znak�w
;--------------------------------------------------------------------------------------------------------

  SkipWhiteSpaces proc
	xor	al, al
	xor	ah, ah
   sws_poczatek:
	mov	al, es:[si]
   spacje:
	cmp	al, ' '			;czy znak jest spacj�
	je	bialy_znak
   tabulacje:
	cmp	al, 9d			;czy znak jest tabulacj�
	je	bialy_znak
	ret
   bialy_znak:
	inc	ah				;ile bia�ych znak�w zosta�o pomini�tych
	inc	si
	
	jmp	sws_poczatek
  SkipWhiteSpaces endp	
	
	
;--------------------------------------------------------------------------------------------------------
;	Procedura wypisuj�ca argumenty i ich liczb� - 
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
	mov	cl, al			;cl - d�ugo�� scalonych argument�w (zawiera $)	
  	mov	ch, ah			;ch - liczba argument�w
  	
  	mov	ah, 9h
	mov	dx, offset args_info
	int	21h
   
	mov	dl, ch
	add	dl, 48 		;dodaje kod ascii '0' do liczby argument�w
	call	printC		;wypisuje liczb� argument�w
	
	xor	ch, ch			;zeruje ch, b�dzie liczy�o ile znak�w ju� wypisano
	dec	si
   new_l:
    inc	ch				
    inc	si
    call	printNL
   printargs_begin:		
   	mov	dl, [si]
	cmp	dl, '$'		;je�li napotka znak '$' sko�czy� si� argument i przechodzi do nowego wiersza
	je	new_l
	call	printC
	inc	si
	inc 	ch
	cmp	ch, cl
	jna	printargs_begin	;je�li nie wypisano wszystkich znak�w to skacze do printargs_begin
	 
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
;	Wst�pnie sprawdza poprawno�� wprowadzonych argument�w (ich liczb� i d�ugo��).
;	wej�cie:	al-d�ugo�� scalonych argument�w
;				ah-liczba argument�w 
;--------------------------------------------------------------------------------------------------------

  CheckArgs proc
	
	cmp	ah, 2d				;sprawdza czy podano 2 argumenty (wersj� i klucz)
	je 	ilosc_sprawdzono
		mov	al, 3d
		call	Error
   
   ilosc_sprawdzono:			;sprawdza czy d�ugo�� argument�w jest poprawna
	cmp	al, 50d					;1 znak na oznaczenie wersji + 47 znak�w klucza + 2 znaki '$'
	je	dlugosc_sprawdzono
		mov	al, 2d
		call	Error
   
   dlugosc_sprawdzono:
	ret
  CheckArgs endp
  

;--------------------------------------------------------------------------------------------------------
;	Wyodr�bnia klucz i sprawdza jego poprawno��.
;	wej�cie:	ds:parsed_args - sparsowane argumenty
;				si - pozycja przed kluczem (po wywo�aniu 'ExtractVersion')
;	wyj�cie:	ds:key - tablica przechowuj�ca klucz
;--------------------------------------------------------------------------------------------------------

	ExtractKey proc
		push	dx
		push	cx
		push	si
		push	di
		push	bx
		
		
		xor	di, di
		mov	di, offset key	;pocz�tek tablicy przechowujacej klucz
		
							;jeste�my na znaku '$', tu� przed kluczem
		inc	si				;pierwszy znak klucza
		xor	ch, ch			;b�dzie liczy�o ile par hex ju� zosta�o przerobionych
						
	   nextpair:
		xor	bx, bx
		xor	ax, ax
		mov	al, ds:[si]				;pierwsza cyfra z pary (starsze 4 bity) do al
		cmp	al, ':'					;sprawdza czy jest to ':'
		je	ek_wrongkey_error		;jesli tak to b��d(powinny by� dwa znaki pomi�dzy ':')
		call	CheckHexC				;sprawdza czy cyfra hex i konwertuje j� do bin
		mov	bh, al
		shl	bh, 4d					;przesuwa al o 4 bity w lewo	
		
		inc	si
		mov	al, ds:[si]				;bierze nast�pn� cyfr�
		cmp	al, ':'					;sprawdza czy jest to ':'
		je	ek_wrongkey_error		;jesli tak to blad(powinny by� dwa znaki pomi�dzy ':')
		call	CheckHexC				;po tej operacji w al s� 4 m�odsze bity z pary
		add	bh, al					;w bh znajduje si� para hex przerobiona do bin
		
		
		mov	byte ptr ds:[di], bh		;zapisuje par� hex w postaci binarnej
		inc	di
		inc	ch						
		cmp	ch, 16d					;je�li wszystkie pary zosta�y przerobione to koniec
		jae	extract_key_end 
		inc	si
		mov	al, ds:[si]				;nastepny znak powinien byc ':'
		cmp	al, ':'
		jne	ek_wrongkey_error
		inc	si
		jmp	nextpair		
		
			   
	   ek_wrongkey_error:			
		mov	al, 1d			;je�li pomi�dzy ':' jest wi�cej ni� 2 cyfry wy�wietl blad
		call 	Error
	   
	   extract_key_end:
		pop	bx
		pop	di
		pop	si
		pop	cx
		pop	dx
		ret
	ExtractKey endp
	
	
;--------------------------------------------------------------------------------------------------------
;	Sprawdza poprawno�� znaku (czy jest cyfr� w systemie szesnastkowym)
;	i przekszta�ca go do liczby w systemie binarnym.
;	wej�cie:	al - znak do sprawdzenia
;	wyj�cie:	al - przekonwertowany znak
;--------------------------------------------------------------------------------------------------------	
	
	CheckHexC proc
		cmp	al, '0'				;sprawdza czy znak jest wi�kszy ni� '0'
		jb	check_hex_error
		cmp	al, '9'
		ja	check_AF
			sub	al, 48d
			ret	
	   check_AF:
		cmp	al, 'A'
		jb	check_hex_error
		cmp	al, 'F'
		ja	check_laf
			sub	al, 55d
			ret	
	   check_laf:
		cmp	al, 'a'
		jb	check_hex_error
		cmp	al, 'f'
		ja	check_hex_error
			sub	al, 87d
			ret	
	   check_hex_error:
		mov		al, 4d
		call	Error
	CheckHexC endp

;--------------------------------------------------------------------------------------------------------
;	Wyodr�bnia wersj� programu i sprawdza jej poprawno��.
;	wej�cie:	ds:parsed_args - sparsowane argumenty
;	wyj�cie:	version - zmienna przechowuj�ca wersj� programu 
;				cx - po�o�enie znaku $, kt�ry poprzedza klucz
;--------------------------------------------------------------------------------------------------------	
	
	ExtractVersion proc
		push	dx
		push	bx
		
		xor	si, si
		mov	si, offset parsed_args
		xor	bx, bx
	   ev_check_l:					;sprawdza czy wprowadzono odpowiedni� wersj� programu
		inc	si
		inc	bx
		mov	al, ds:[si]
		cmp	al, '$'
		jne	ev_check_l
		
		mov	cx, bx
		dec	bx					;zmniejszam bo policzyli�my znak '$'
		cmp	bx, 1d				;je�li jako wersje wprowadzono wi�cej ni� jedn� cyfr� wy�wietla komunikat o b��dzie
		ja	ver_error
		
		xor	si, si
		mov	si, [offset parsed_args+1]		;tutaj jest wersja programu (pierwszy znak w sparsowanych argumentach to '$')
		
		mov	al, ds:[si]
		call	CheckHexC
		cmp	al, 0d
		jne	ver1
			mov byte ptr version, al		;zapisuje wprowadzon� wersj� programu w zmiennej version
			jmp	exver_end
	   ver1:
		cmp	al, 1d
		jne	ver2
			mov byte ptr version, al
			jmp	exver_end
		ver2:
		cmp	al, 2d
		jne	ver_error
			mov byte ptr version, al
			jmp	exver_end
	   ver_error:
		mov		al, 0d					;w przypadku gdy podano wersj� programu r�n� od 0, 1 lub 2 wypisuje komunikat o b��dzie
		call	Error
	   exver_end:
		mov	si, cx
		pop	bx
		pop	dx
		ret
	ExtractVersion endp
	
;--------------------------------------------------------------------------------------------------------
;	Zamienia klucz na ruchy.
;	wej�cie:	ds:version - wersja programu
;				ds:key - klucz
;	wyj�cie:	ds:moves - tablica z ruchami do wykonania (64 pary bit�w)
;--------------------------------------------------------------------------------------------------------	

	CreateMoves proc
		push	cx
		push	di
		push	si
		push	dx
		
		mov		si, offset key		;tablica z kluczem		
		mov		di, offset moves	;to b�dzie tablica z parami bit�w okre�laj�cymi ruchy
		mov 	cx, 16d				;liczba bajt�w w kluczu
		
	   klucz:
		xor 	dx, dx
		xor		ax, ax
		mov 	dl, ds:[si]  		;bierzemy bajt z tablicy key
		inc		si
		
		cmp	byte ptr ds:[version], 2d
		je	modification
			
			push 	cx
			mov 	cx, 4d				;liczba par bit�w w bajcie	
			pary:
				mov 	ax, dx
				
				push	bx
				mov		bl, 4d
				div		bl				;ah - reszta z dzielenia (szukana para)
				pop		bx
				
				mov 	byte ptr ds:[di], ah 
				inc 	di
				shr		dl, 2d
			loop pary
			pop cx
		loop klucz
				
			jmp	after_mod
			
			modification:
			push 	cx
			mov 	cx, 4d				;liczba par bit�w w bajcie	
				mod_pary:
					;xor		ax, ax
					mov		ax, dx
					
					push	bx
					mov		bl, 64d
					div		bl				;dziel� al przez 64, al - szukana para
					pop		bx
					
					mov	byte ptr ds:[di], al		
					inc	di
					shl	dl, 2d			;zape�niam wolne 2 bity po wyci�gni�tej parze
				loop mod_pary
			pop cx
		loop klucz
	   
		after_mod:
				
		pop	dx
		pop	si
		pop di
		pop	cx
		ret		
	CreateMoves endp


;----------------------------------------------------------------------------------------------
; 	Przej�cie po szachownicy.
;	wej�cie:	ds:key - tablica z kluczem
;				ds:chessboard - tablica 153 bajty, szachownica
;	wyj�cie:	ds:chessboard - szachownica wype�niona ilo�ciami odwiedzin p�l
;				di - ostatnia pozycja go�ca na szachownicy
;----------------------------------------------------------------------------------------------
	Move proc
		push	cx
		push	si
		push	dx
		
		xor	si, si
		mov	si, offset moves
		
		cmp	byte ptr ds:[version], 1d
		je	mov_modification
		
		xor	di, di
		mov	di, 76d						;ustawienie go�ca na �rodku
		inc	byte ptr ds:[di+chessboard]			;zaznaczanie wizyty w tablicy
		
		mov	cx, 64d
		next_move:
			xor ax, ax
			mov	al, ds:[si]		;pobieram do al analizowan� par� bit�w z tablicy moves			
			cmp	al, 00b			;porownuje dwa bity z 00b
			jne	next1
				call UpLeft
				jmp visit
			next1:
			cmp	al, 01b
			jne	next2
				call UpRight
				jmp visit
			next2:
			cmp	al, 10b
			jne	next3
				call DownLeft
				jmp visit
			next3:
			cmp al, 11b
			jne conv_error
				call DownRight
				jmp visit
			conv_error:
			mov		al, 5d
			call	Error
			
			visit:
			add	ds:[di+chessboard], 1d					;zaznaczenie wizyty w danym polu szachownicy
			inc	si			;nastepny bajt z wejscia
			
			;push	dx
			;push	ax
			;push	di
			;push	si
			;push	cx
			;call	ToSymbols
			;call 	PrintChessboard
			;mov		ah, 0h
			;int		16h
			;pop		cx
			;pop		si
			;pop		di
			;pop		ax
			;pop		dx
			
		loop	next_move
		jmp move_end
		
		mov_modification:
			xor	di, di
			xor	bx, bx
			xor dl, dl						;przechowuje informacj� kt�ry goniec rusza� si� ostanio (2 - drugi, 1 - pierwszy)
			mov	di, 16d						;ustawienie pierwszego go�ca w prawym g�rnym rogu
			inc	byte ptr ds:[di+chessboard]			;zaznaczanie wizyty w tablicy
			mov bx, 136d					;ustawienie drugiego go�ca w lewym dolnym rogu
			inc	byte ptr ds:[bx+chessboard]
			
			mov	cx, 64d
			mod_next_move:
				cmp dl, 1d
				jne	bishop_one				;je�li teraz kolej ruchu pierwszego go�ca to skocz do bishop_one
					push	di				;je�li teraz kolej drugiego go�ca to zachowuje na stosie pozycj� pierwszego go�ca
					mov		di, bx			;(indeks di jest u�ywany do ruch�w)
				bishop_one:
				xor ax, ax
				mov	al, ds:[si]		;pobieram do al analizowan� par� bit�w z tablicy moves			
				cmp	al, 00b			;porownuje dwa bity z 00b
				jne	mod_next1
					call UpLeft
					jmp mod_visit
				mod_next1:
				cmp	al, 01b
				jne	mod_next2
					call UpRight
					jmp mod_visit
				mod_next2:
				cmp	al, 10b
				jne	mod_next3
					call DownLeft
					jmp mod_visit
				mod_next3:
				cmp al, 11b
				jne mod_conv_error
					call DownRight
					jmp mod_visit
				mod_conv_error:
				mov		al, 5d
				call	Error
			
				mod_visit:
				add	ds:[di+chessboard], 1d					;zaznaczenie wizyty w danym polu szachownicy
				cmp	dl, 1d
				jne goniec1_end								;je�li teraz rusza� si� drugi goniec
					xor	bx, bx
					mov	bx, di
					xor	dl, dl
					mov	dl, 2d								;zapisujemy informacj� �e ostatni ruch by� drugiego go�ca
					pop	di									;odzyskujemy ze stosu ostatnie miejsce w kt�rym by� pierwszy goniec
					jmp	goniec2_end
				goniec1_end:
				mov	dl, 1d									;je�li teraz rusza� si� pierwszy goniec to zapisujemy t� informacj�
				
				goniec2_end:
				inc	si			;nastepny bajt z wejscia
							
			loop	mod_next_move
			
		move_end:
		pop	dx
		pop	si
		pop	cx
		ret
	Move endp


;----------------------------------------------------------------------------------------------
; 	Procedury ruch�w go�ca.
;	wej�cie: 	di - pozycja go�ca na planszy
;	wyj�cie:	di - pozycja po wykonaniu ruchu
;----------------------------------------------------------------------------------------------
	UpRight proc			;do g�ry w prawo
		push	ax
		cmp		di, 16d
		je end_UR			;jeste�my w prawym g�rnym rogu planszy
			mov	ax, di		
			div	chbl		;ax div 17
			cmp	ah, 16d		;ax mod 17 = 16 -> jeste�my przy prawej �cianie
			jne	wall_UR
				sub	di, 17d	;�lizgaj si� do g�ry
				jmp	end_UR	
			wall_UR:
			cmp	al, 0d		;ax div 17 = 0 -> jeste�my przy suficie
			jne	ceiling_UR
				inc	di		;�lizgaj si� w prawo
				jmp	end_UR
			ceiling_UR:
			sub	di,16d		;zwyk�y przypadek, upright
		end_UR:
		pop	ax
		ret	
	UpRight endp
		
		
	UpLeft proc
		push ax
		cmp	di, 0d
		je	end_UL			;jeste�my w lewym g�rnym rogu planszy
			mov	ax, di		;ax <- di
			div	chbl		;ax div 17
			cmp	ah, 0d		;ax mod 17 = 0 -> jeste�my przy �cianie
			jne	wall_UL
				sub	di, 17d	;�lizgaj si� do g�ry
				jmp	end_UL	
			wall_UL:
			cmp	al, 0d		;ax div 17 = 0 -> jeste�my przy suficie
			jne	ceiling_UL
				dec	di		;�lizgaj si� w lewo
				jmp	end_UL
			ceiling_UL:
			sub	di, 18d		; zwyk�y przypadek, upleft
		end_UL:
		pop ax
		ret	
	upleft endp
		
		
	DownRight proc
		push	ax
		cmp	di, 152d
		je	end_DR			;jeste�my w prawym dolnym rogu planszy
			mov	ax, di		
			div	chbl		;ax div 17
			cmp	ah, 16d		;ax mod 17 = 16 -> jeste�my przy �cianie
			jne	wall_DR
				add	di, 17d	;�lizgaj si� w d�
				jmp	end_DR	
			wall_DR:
			cmp	al, 8d		;ax div 17 = 8 -> jeste�my przy pod�odze
			jne	floor_DR
				inc	di		;�lizgaj si� w prawo
				jmp	end_DR
			floor_DR:
			add	di, 18d		;zwyk�y przypadek, downright
		end_DR:
		pop ax
		ret	
	DownRight endp
		
		
	DownLeft proc
		push ax
		cmp	di, 136d
		je end_DL			;jeste�my w lewym dolnym rogu planszy
			mov	ax, di		
			div	chbl		;ax div 17
			cmp	ah, 0d		;ax mod 17 = 0 -> jeste�my przy lewej �cianie
			jne	wall_DL
				add	di, 17d	;�lizgaj si� w d�
				jmp	end_DL	
			wall_DL:
			cmp	al, 8d		;ax div 17 = 8 -> jeste�my przy pod�odze
			jne	floor_DL
				dec	di		;�lizgaj si� w lewo
				jmp	end_DL
			floor_DL:
			add	di, 16d		;zwyk�y przypadek, downleft
		end_DL:
		pop ax
		ret
	DownLeft endp	
	
	
;----------------------------------------------------------------------------------------------
; 	Zamiana liczby odwiedzin na symbole.
;	wej�cie:	di - miejsce gdzie goniec sko�czy� ruch
;				ds:chessboard - szachownica
;				ds:symbols - tablica symboli
;----------------------------------------------------------------------------------------------	
	
	ToSymbols proc
		push	si
		push	cx
		push	ax
		
		push	bx
		push	di
		
		xor	di, di
		mov	di, offset chessboard
		mov	cx, 153d
		xor	bx, bx
		mov	bx, offset fingerprint
		change:
			xor	ax, ax					; bo si <- [di] pobierze dwa bajty, usuwam �mieci z ah
			mov	al, [di]
			mov	si, ax
			cmp	si, 14d
			jbe	below14					; jesli SI <=14 przeskocz
				mov	ax, 14d				; je�li SI > 14 to SI = 14'
				mov	si, ax
			below14:
			mov	al, [si+symbols]		; wybieram znak z tabeli znakow
			mov	byte ptr [bx], al		; zapisuje
			inc	di
			inc	bx
			xor	si, si
		loop	change
		
		cmp	byte ptr ds:[version], 1d
		je	symbols_modification
		
			pop	di
			pop	bx
		
			mov	al,	'E'
			mov	byte ptr ds:[di+fingerprint], al			;wpisuje 'E' w miejscu gdzie sko�czy� goniec
			mov	al, 'S'									
			xor	bx, bx
			mov	bx, 76d
			mov	byte ptr ds:[bx+fingerprint], al			;wpisuje 'S' na �rodku szachownicy (start)
			
			jmp symbols_end
		
		symbols_modification:
			pop	di
			pop	bx
		
			mov	al,	'1'
			mov	byte ptr ds:[di+fingerprint], al			;wpisuje 'E1' w miejscu gdzie sko�czy� pierwszy goniec
			mov	al,	'2'
			mov	byte ptr ds:[bx+fingerprint], al			;wpisuje 'E2' w miejscu gdzie sko�czy� drugi goniec
			
			mov	al, 'S'									
			xor	bx, bx
			mov	bx, 16d
			mov	byte ptr ds:[bx+fingerprint], al			;wpisuje 'S1' w prawym g�rnym rogu (start pierwszego go�ca)								
			xor	bx, bx
			mov	bx, 136d
			mov	byte ptr ds:[bx+fingerprint], al			;wpisuje 'S2' w lewym dolnym rogu (start drugiego go�ca)
		
		symbols_end:
		pop	ax
		pop	cx
		pop	si
		ret
	ToSymbols endp

;--------------------------------------------------------------------------------------------------------
;	Drukowanie szachownicy
;	wej�cie:	ds:chessboard - szachownica
;--------------------------------------------------------------------------------------------------------

	PrintChessboard	proc
		push	si
		push	cx
		push	dx
		push	ax
		
		call printNL
		mov	ah, 9h
		mov dx, offset border
		int	21h
		
		xor 	si, si
		mov		si, offset fingerprint
		mov 	cx, 9d						;liczba wierszy szachownicy
		line:
			mov	dl, '|'
			call 	printC
			
			push	cx
			mov	cx, 17d				;liczba kolumn szachownicy
			char:
				mov	dl, ds:[si]
				inc	si
				call 	printC
			loop char
			pop	cx
			
			mov	dl, '|'
			call	printC
			call	printNL
		loop line
		
		mov	ah, 9h
		mov dx, offset border
		int	21h
		
		pop	ax
		pop	dx
		pop	cx
		pop	si
		ret
	PrintChessboard endp

;--------------------------------------------------------------------------------------------------------
;	Obs�uga b��d�w
;	wej�cie:	al-numer b��du
;--------------------------------------------------------------------------------------------------------
	
	Error proc
	   cmp	al, 0d
	   jne 	error1
		mov dx, offset wrong_version_err
		jmp	err_end
	  error1:
	   cmp	al, 1d
	   jne	error2
		mov	dx, offset wrong_key_err
		jmp	err_end
	  error2:
	   cmp	al, 2d
	   jne	error3
		mov dx, offset wrong_length_err
		jmp	err_end
	  error3:
	   cmp	al, 3d
	   jne	error4
		mov dx, offset wrong_quantity_err
		jmp	err_end
	  error4:
		cmp al, 4d
		jne error5
		mov dx, offset wrong_hex_err
		jmp err_end
	  error5:
		cmp	al, 5d
		jne error6
		mov	dx, offset conversion_err
		jmp	err_end
	  error6:
	  err_end:
		call printNL
		call printNL
		mov	ah, 9h
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
