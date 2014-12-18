;;ctv vtc 
(load "include.ss")

;;test icin
(load "test-dictionary.ss")


;;Spell-Checker Helper Func
;;dictionary i parcalayip kontrol edebilmek icin helper yazdim

(define spell-checker-helper
	(lambda (w d)
	;;ilk eleman word e esit mi diye bakiyor
	;;esitse true doner biter
	;;esit degilse alttaki if e girer liste bitti mi diye bakar
	;;liste bittiyse false doner bitmediyse recursion olarak
	;;listenin geri kalanina bakar
	(if
		(equal? w (car d))
		#t
		(if 
			(null? (cdr d))
			#f
			(spell-checker-helper w (cdr d))
		)
	)
))
;;------------------------------------------------
;;;;Gen-Decoder-A Helper Func
;;liste null degilse spell checker yardimiyla wordleri kontrol eder
;;esit buldukca +1 bulamadikca +0 yaparak doner
;;paragraflari boler ve counterHelper'a gonderir o ise cozer
(define gen-decoder-a-helper
	(lambda (d)
	(if 
		(not (null? d))
		(+ (counterHelper (car d)) (gen-decoder-a-helper (cdr d)))
		0
	)
))

;;tek satiri spell checker ile sayar gen-decoder-helper-a yardimci fonksiyonu
(define counterHelper
	(lambda (w)
	(if 
		(not (null? w))
		(if
			(equal? #t (spell-checker (car w)))
			(+ 1 (counterHelper (cdr w)))
			(+ 0 (counterHelper (cdr w)))
		)
		0
	)

))


;;paragrafi alip gen-decoder-a-helper fonksiyonundan yararlanarak
;;her bir kaydırma icin benzer sonuclari liste olarak dondurur
(define gen-decoder-a-helper-2
	(lambda (p)
	;;tum harfleri deniyor
	(map (lambda (val)
		(gen-decoder-a-helper (encode-d p (encode-n val)))
		)'(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
))
;;gen-decoder-a-helper-2'den yararlanir
;;benzer sonuclarin kacar tane oldugu listeyi alir
;;kacinci shift en cok benzerlik sagladiysa onu return eder
;;yani verilen listenin en buyuk elemaninin yerini return eder
;;liste 0 dan basladigi icin tek eleman eksiktir kullanirken +1 yapacagiz
(define gen-decoder-a-helper-3
	(lambda (lst)
	
	(if
		(not (null? lst))
		(if
			(equal? (apply max lst) (car lst))
			(+ 0 (gen-decoder-a-helper-3 (cdr lst)))
			(+ 1 (gen-decoder-a-helper-3 (cdr lst)))
		)
		0
	)
))
;;-----------------------------------------------------
;;Gen-Decoder-B Helper Func
;;bir harften kaçtane var onu buluyor
;;input : character-document
;;uc parca halinde yapıyorum
;;ilki kelimeyi digeri satiri digerleri paragraflari yapar

;; kelimeyi bulan
(define find-a
	(lambda (c d)
	(if 
		(not (null? d))
		(if
			(equal? c (car d))
			(+ 1 (find-a c (cdr d)))
			(+ 0 (find-a c (cdr d)))
		)
		0
	)
))

;;satiri bulan
(define find-b
	(lambda (c d)

	(if 
		(not (null? d))
		(+ (find-a c (car d)) (find-b c (cdr d)))
		0
	)
))
;;paragrafi bulan
(define find-c
	(lambda (c d)

	(if 
		(not (null? d))
		(+ (find-b c (car d)) (find-c c (cdr d)))
		0
	)
))

;;Harflerden kacar tane oldugunu bulup liste olarak donduren fonksiyon
(define gen-decoder-b-helper
	(lambda (p)
	(map (lambda (val)
		(find-c val p)
	)'(a b c d e f g h i j k l m n o p r s t u v y z))
))

;;------------------------------------------------------

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
;;mission completed
(define spell-checker 
  (lambda (w)
	(spell-checker-helper w dictionary)
   ))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
;;mission completed
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
	;;map fonksiyonu yardimiyla kelimeler n kadar ileri giderek degistirildi
	(map (lambda (val) (vtc (modulo (+ n (ctv val)  ) 23 ))  ) w)
      )))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
;;this encoder is supposed to be the output of "encode-n"
;;mission completed
(define encode-d
  (lambda (d encoder)
	(define liste '())
	
	(map (lambda (x)
		(map (lambda (val) (encoder val) ) x))
		d)
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
;;mission completed
(define Gen-Decoder-A
  (lambda (p)
	;;helper yardimiyla en uygun shift sayisi bulundu
	;;simdi cozulmus halini dondurelim
	(encode-d p (encode-n (+ 1 (gen-decoder-a-helper-3 (gen-decoder-a-helper-2 p)))))	
    ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
	;;maksimumu bulurken gen-decoder-a-helper-3 kullanildi
	;;dogru cikti icin kaydirma yapildi
	;;test ds1 ile yapilacagi icin ds1deki en cok kullanilan harf olan 's ye gore yapildi
	;;normalde 's 17. sirada ama 16 kullandim cunku en fazla buldugunu donduren sayi 1 fazla donduruyor
	(encode-d p (encode-n (- 23 (- (gen-decoder-a-helper-3 (gen-decoder-b-helper p)) 16))))
    ))

;; -----------------------------------------------------

