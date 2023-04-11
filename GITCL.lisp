;;; 1.2
(print(- (/ 18 4) (* 3 5) (+ 1 1)))
(print(sqrt(abs(- (/ 18 4)(* 3 5)(+ 1 1)))))
;; the order it is read is forwards like english
(print(/ 18 4)) ;; simplified to 9/2
(print(/ 4 18)) ;; 2/9

;;;1.5
;; a symbol isnt a string or a charater or a array or a list, a interger is a number i.e +4 or 4
;; there are also floats in lisp which are numbers as well (see below)
;;predicates are functions that return true(T) or false(NIL) and have a function naming system withe a NAME and P at the end i.e function NUMBERP checks if something is a number (i.e  a interger 4 or a float 4.56)
;; remember that EVERYTHING is T unless it is explicitly NIL
(print(numberp 3)) ;;returns T
(print(numberp +4)) ;;returns T
(print(numberp -3.19)) ;;returns T

;;printing a LIST of 'nine' and checking if it is a number
(print(numberp (quote(nine)))) ;;returns NIL
;;quote shorthand
(print(numberp '(nine))) ;;returns NIL

;;printing the check of a  list of numbers
(print(numberp '(12345))) ;;returns NIL because it is of type LIST eventhough it is a list of numbers

;;printing the number check of a single character
(print(numberp 'a)) ;;returns NIL

;;Other inbuilt number functions
;;checks if zero
(print(zerop 0)) ;; T
(print(oddp 2)) ;; NIL
(print(evenp 2)) ;; T
;; a prime number is a positive whole number that has only has two prime factors, itself and 1. Only divisible by itself and 1 and no other numbers.
;; checks

;; check if the type is a number, if not return NIL
;; check if it n is O or 1, return nil if true
;; 1 - divided n by 1, return nil if negative
;; 2 - (intergerp n) must return T
;; 3- if it is divisible by 2, 3, 5 or 7 and returns a number that not 1, return NIL
;;if none of these checks fail return T
(print(integerp -1)) ;;negative numbers are an interger
(print(plusp 2)) ;; checks if positive returns true





(defun primep(n)
  (and (integerp n)
       (> n 1)
       (if (or (= n 2)
	      (= n 3)
	      (= n 5)
	      (= n 7))
	   t
	   (and
	       (not(zerop(mod n 2)))
	       (not(zerop(mod n 3)))
	       (not(zerop(mod n 5)))
	       (not(zerop(mod n 7)))))))


;;Sieve of Eratosthenes
(defun sieve (&rest p)
  ;;make a list from >1 to p
  (list ())
  ;;take out all the equal numbers unless it is 2
  ;;take out the numbers divisible by 3,5 and 7
  ;; print the remaining numbers
  )

;; Eucilids sequens for loop
;;;;;;;;;;;;;;;

;; 1.9

(defun add1 (n)
  (+ n 1))
;;this is the same as inbuilt 1+
;; 1.9.2

;; build add2 using add1
(defun add2 (n)
  (add1 (add1 n)))

(defun sub1 (n)
  (- n 1))

(defun sub2(n)
  (sub1 (sub1 n)))

;;Show how to write TWOP in terms of ZEROP and SUB2.
(defun twop (n)
  (zerop (sub2 n)))
;; show how to define half in two different ways

(defun half (n)
  (/ n 2))

;;  it was fun
(defun doubleN (n)
  (* (add2 2) (half n)))

;; does the number have more then one digit
(defun multi-digit-p (n)
  (or (>= n 10)
      (<= n -10)))

;; defining a function of two inputs. which tests whether its first input is exactly one greater than its second input.

(defun onemorep (n k)
  "defining a function of two inputs. which tests whether its first input is exactly one greater than its second input."
  (equalp k (add1 n)))

(defun twomorep (n k)
  (= n (sub2 k)))

(defun average2 (n k)
  (half (+ n k)))

;; one day
(defun average (&rest l)
  (/ (reduce '+ l) (length l)))
;;& rest tells it thT ALL the arguments are going to be a list

;;Write a MORE-THAN-HALF-P predicate that returns T if its first input is more than half of its second input
(defun more-than-half-p (n k)
  (> n (doublen k)))

;;Write XOR, the exclusive-or truth function, which returns T whenone of its inputs is NIL and the other is T, but returns NIL when both are NIL or both are T. 
(defun xor (n k)
  (not (equalp n k)))

(print (- 4))
(print (- 4 2))
(print (/ 4))
(print (/ 4 2))

(defun my-second (l)
  (first (rest l)))

;; (defun my-third (&rest l)
;;   (first (rest (rest l))))


(defun my-third (l)
  (my-second (rest l)))

;; CAR is the pointer to the first element in a list like in 'my-first'
;; CDR is the pointer to the REST of the elements in a list like in 'rest'
;; when you do my-second and my-third to move the car and cdr along to the second or third
;; nothing gets destructed, the pointer in the registry justs moves over to the amount you tell it to and leaved the begginings of the list alone

;;problem 26 rhind papyrus
;;guess and check to solve for x
(defun regula-falsi-p (x)
  (let ((y 15))
    (equalp (+ x (/ x 4)) y)))

(print ((caaar '(((FUN)) (IN THE) (SUN))))) ;;fun
(print ((cadr '(((FUN)) (IN THE) (SUN))))) ;;(IN THE)
(print ((caadr '(((FUN)) (IN THE) (SUN))))) ;;IN
(print ((cadadr '(((FUN)) (IN THE) (SUN))))) ;;THE
(print ((caaddr '(((FUN)) (IN THE) (SUN))))) ;;SUN

(print (caadr '((BLUE CUBE) (RED PYRAMID))));;red
(print (cdr '((BLUE CUBE) (RED PYRAMID)))) ;;((red pyramid))
(print (cdaar '((BLUE CUBE) (RED PYRAMID)))) ;;errors out to get blue
(print (caar '((BLUE CUBE) (RED PYRAMID))));; blue
(print  (cadadr '((BLUE CUBE) (RED PYRAMID)))) ;;pyramid


(let (gratio)
  (/ 2 (+ (sqrt 5) 1)))

(defun increment (n)
  (cons (+ (expt n 2) 1) (cons 0 ()))
  )

(defun seq (l)
  )

(defun wat (a b)
  (cons a (cons b ())))

;; if n is 1 add 1
;; then add n + 1 =

;;exercise 2.21
(defun fun ( a b c d)
  (cons (cons a (cons b nil))
	(cons (cons c (cons d nil)) nil)))
;; above is unreadable

(defun fun1 (a b c d)
  (list (cons a (cons b nil))
	(cons c (cons d nil))))

;;even better
(defun fun2 (a b c d)
  (list (list a b)
	(list c d)))

;;2.22

(defun duo-cons (a b c)
  (cons a (cons b c)))

;;two deeper
(defun two-deeper (l)
  (list (list l)))

;;two deeper
(defun two-deeper1 (l)
  (cons (cons l nil) nil))

(print (atom 18)) ;;T
(print (atom 'l)) ;;T
(print (atom '(hole in one))) ;;NIL

(print (null '())) ;;T
(print (null '(abcds))) ;;NIL
(print (null 'a)) ;;NIL
(print (null 15)) ;;NIL
(print (null ())) ;;T
;;null is to test if a list is empty

(print (length '(usrfoihresfiehrgfegfeirh j))) ;;length of a list


;;advanced section
(defun unary-add1 (l)
  (cons 'x l))

(defun unary-zerop (l)
  (null l))

(defun unary-greaterp (l k)
  (> (length l) (length k)))

(print (cons 1 (cons 2 (cons 3 4))))
(print (list (cons 1 2) (cons 3 4)))

(print (cons 1 (cons 2 (cons 3 #1#))))
;;one day will find the lenght of a circular list

(defun funin (x)
  (funcall x 1 2))

;;write this in repl
(lambda (x)
  (+ x 1))
p
(funcall * 1)

;;;;;;;;;

(funin (lambda (a b)
	 (+ a b)))
;; this gives you 3 as lambda is the function being passed as x in the function funin :^)

;;;;;

(print (funin 'wat))

;; manipulation of lists and functions
(print (funcall
	(eval
	 (cons 'lambda
	       (cons nil
		     (list (cons '+ (cons 1 (cons 2 nil)))))))))
;;https://quotegeek.com/quotes-from-movies/shrek/7316/

(loop (print (eval (read)))) ;;press cntrl c-c

(defun somthing (n k)
  (list (equalp (+ n 1) k)))

;;goldbachs conjecture

;; eval (+ 2 5) but backwards, you have the soup but you must ind the ingredients
;;there must only be two ingredients
;; both ingredients have to be a prime number (use primep and a and macro)
;; so you take a number that a even natural number and INPUT
;; return two prime numbers where the sum of the two EQUALS the INPUT
;; sometimes according to heuristic justification, there can be more then one answer
;; so the added prime numbers must start as large as possible i.e. starting at the prime number closest to the original input and checking if that prime number + the smallest prime number possible will equal to the input UNTIL the SUM is LARGER THAN the input
;; for the first one that returns true, return in eval notation e.g. ( + 2 2)


;;partial solution
(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))



(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun gold (n)
  ;;test lowest prime numbers (lpn) for an input of 8
  ;;take lowest prime number (lpn) from the prime list generated by eucilids sieve

  ;;for example
  ;; lowest prime is 2. 8-2 = equal number
  ;; find the next lpn in eucilids sieve and retest 
  ;; test 3. 8-3=prime number
  ;; primep(n - 3)
  ;;if true, return 3 (lpn) and (eval (- n lpn)) in a list
  (if (primep (- n lpn)) ;;if primep(- 8 5) rurns true
      (list '+ lpn (- n lpn)) ;;then do this
      ;;else increase lpn unless (< 1 (- n lpn)) then return NIL proving goldbach wrong
      (;; find the next lpn in eucilids sieve and retest
       unless (< 1 ( - n lpn)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;back to the book
;;3.5 HALF has been done, cube will be done, onemorep has been done
(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading
			 final-odometer-reading
			 gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


;;make better
(defun gold (n lpn)
  (if (and (evenp n)
	   (primep (- n lpn))
	   (< 1 ( - n lpn)))
      (list '+ lpn (- n lpn))
      ;;else find the lpn where it is larger then the last lpn and try again (loop)
      ))
;;prove that there is a even number where it cannot be added by two prime numbers
;;since the larger numbers 'n' get, the more likely it is that this
;; so is not true for the number 2, because it is even AND prime
;; this is not true for he number 1, because it is a odd number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print pi)
;;symbols  must be quoted to prevent evaluation/eval from treating the symbol as a object that hold a value. if it is not, and the symbol does not hold a value, you will get the unnasigned variable error
;;symbols must be left unquoted when they refer to variables

(defun raven (x y)
  "input x as 'raven and y as 'writing-desk in the repl"
  (list 'why 'is 'a x 'like 'a y))

;;im going to make my favourite recipe, it has 3 ingredients (inputs) and the combination of them in different ratios and cooking METHODS create different dishes(outputs) then return the list of instructions for a person to follow
(defun (egg starch milk)
    "To make vasten vladen; milk custard"
  (let ))
;;this is nonw in file fridge.lisp because i want a cookbook

;;;;;;;;;;;;;;;;


(defun mystery (x)
  (list (second x) (first x)))
;;mystery '(zowie) returns (nil zowie)

(defun myfun (a b)
  (cons (cons a nil) (cons b nil)))

(defun firstp (s l)
  (equalp s (first l))
  )

(defun mid-add1 (l)
  (list (car l) (1+ (second l)) (third l)))

;;fareinheight to celcius

(defun f-to-c (n)
  (/ (* 5 (- n 32)) 9 ))

;;advanced section
(print (symbol-name 'cons))
(print (symbol-function 'cons))
(print (symbol-value 'cons)) ;;error because cons has no value bound to it

;; LAMBDA NOTATION

(lambda (x)
  (+ 3 x))
;;lambda is not a function, it is a marker treated specially by eval
;;defuns job is to associate names with functions so they can be easily called on later
;;lambda does not do this and it skips this step, lambda just returns a lambda function which you can then call by pointer not by name


(set (symbol-function 'test) (lambda (x) x))
;;these two funtions work the same way
(defun test (x)
  x)

;;(lambda (x) x) is  list. when eval does its thing it always goes to the CAR of a list to check if it is a function like (symbol-functionp) (if that is a thing) if it is not a functon it returns a error (unless it is quoted then it is only a symbol)
;; dont forget, numbers are not symbols. numbers evaluate to themselves because they re numbers
;;as explained before in prob chap to there are exceptions

;;SCOPE
;;Global symbols with values can be referenced everywhere and have unbounded scope
;;local variables are bound in a body of wherevre they are
;;think of taking up namespace

;;in general global variables are t be avoided because it could cause errors down the line if you are not clever
;;passing a variable on to another as an argument is simpler
;; this is the fiderence between thai dining ettiquette (global) and giving your bf some food from your plate

(defun parent (n)
  (child (+ n 2)))

(defun child (p)
  (list n p))

;;what is wrong with the above is that the function child cannot see the symbol value for n and will throw an error
;; the bodys between child and parent are completely seperate and in different scopes. there are not joines by parenthesis or n is not passed as a value

;;demonstation of scope

(let ((n 2))
  ;;n is 2
  (defun parent (n)
    ;;n is 3 is you passed the parent function with 3 as the arg
    (print n)
    (child (+ n 2)))

  (defun child (p)
    (print n)
    ;;n is 2 because is is not defined in the body
    (list n p)))

;;solution
(defun parent (n)
  ;;(+ n 2) is p in the funtion child which takes two arguments
  (child (+ n 2) n))

(defun child (p mega)
  ;;when called mega is bond to the second provided argument.
;;in #'parent mega becomes the value of (symbol-value n)
  (list mega p))

(defun alpha (x)
  (bravo (+ x 2) (charlie x 1)))
(defun bravo (y z) (* y z))
(defun charlie (y x) (- y x))

;;apply takes a functions and a list, then applies the function to the elements in the list
;;its solves the problem where you have a list that exists, ut now you have to add all the elements together (for example with the add function)
(print (apply '+ '(3 4)))
;;apply only accepts a function that makes use of any arbitrary number of functions like plus and equal
(print (apply '= '(1 1 1)))

;;apply effectively puts the function at the beggining of the list like car and cons and then it gets evaluated

(print (eval (cons '+ (list 1 1 1 1)))) ;; this is how eval and apply are related to eachother
(print (apply '+ '( 1 1 1 1)))

(print (equalp (+ 1 1 1 1) (apply '+ '( 1 1 1 1)))) ;;they are the same

(defun further (n)
  "Function that returns a number further away from 0 by one"
  (if (plusp n)
      (1+ n)
      (1- n)))
;;my-not

(defun my-not (anything)
  (if anything
      nil
      t))

(defun ordered (n k)
  "Two arguments. Returns a number list in ascending order"
  (if (> n k)
      (list k n)
      (list n k)))

(defun order-list (&rest list)
  "Accepts a list that gets sorted into ascending order"
  (sort list '<))

;;pg 128
;;closed over functions
(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) (lambda () (print x)))))

(defun print-words ()
  'second-is-smaller)

(defun compare2 (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) (print-words))))


(defun compare3 (x y)
  (cond ((equal x y) (lambda () (+ x y)))
	((< x y) 'first-is-smaller)
	((> x y) (lambda () (print x)))))

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
	(t x)))

;;constrain with cond
(defun constrain (x min max)
  (cond ((< x min) min)
	((< max x) max)
	(t x)))

;;constrain wih nested-ifs
(defun constrain2 (x min max)
  (if (< x min)
      min
      (if (< max x)
	  max
	  x)))

(defun firstzero (&rest number-list)
  "takes a list of 3 numbers as input and tells you where there is a zero"
  (cond ((zerop (car number-list)) 'first)
	((zerop (second number-list)) 'second)
	((zerop (third number-list)) 'third)
	(t 'No-zeros)))

(defun cycle (n)
  (cond ((equalp n 99) 1)
	((and (< n 99)
	      (< 0 n))
	 (1+ n))
	(t 'use-proper-number-fool)))

(defun howcompute (a b c)
  (cond ((equalp (+ a b) c) 'sum-of)
	((equalp (* a b) c) 'product-of)
	((equalp (/ a b) c) 'division-of)
	((equalp (expt a b) c) 'exponent-of)
	((equalp (- b a) (- c b)) 'first-difference-sequence)
	(t 'beats-me)))

(defun how-alike (a b)
  (cond ((equal a b) 'the-same)
	((and (oddp a) (oddp b)) 'both-odd)
	((and (not (oddp a)) (not (oddp b))) 'both-even)
	((and (< a 0) (< b 0)) 'both-negative)
	(t 'not-alike)))

;;Write a function that squares a number if it is odd and positive, doubles
;;it if it is odd and negative, and otherwise divides the number by 2

(defun wierdmath (n)
  (cond ((and (oddp n)
	      (plusp n))
	 (expt n 2))
	((and (oddp n)
	      (not(plusp n))
	      (* 2 n)))
	(t (/ 2 n))))

;;if you double a odd number you get a even number
;;if you raise an oddnumber to the power of 2 you get a odd number

;;Write a predicate that returns T if the first input is either BOY or GIRL
;;and the second input is CHILD, or the first input is either MAN or
;;WOMAN and the second input is ADULT.
(defun childp (gender stage)
  (and (or (equalp gender 'girl)
	   (equalp gender 'boy))
       (equalp stage 'child)))

(defun adultp (gender stage)
  (and (or (equalp gender 'woman)
	   (equalp gender 'man))
       (equalp stage 'adult)))

(defun validp (a b)
  (or (childp a b)
      (adultp a b)))

#||
Write a function to act as referee in the Rock-Scissors-Paper game. In
this game, each player picks one of Rock, Scissors, or Paper, and then
both players tell what they picked. Rock ‘‘breaks’’ Scissors, so if the
first player picks Rock and the second picks Scissors, the first player
wins. Scissors ‘‘cuts’’ Paper, and Paper ‘‘covers’’ Rock. If both
players pick the same thing, it’s a tie. The function PLAY should take
two inputs, each of which is either ROCK, SCISSORS, or PAPER, and
return one of the symbols FIRST-WINS, SECOND-WINS, or TIE.
Examples: (PLAY ’ROCK ’SCISSORS) should return FIRST-WINS.
(PLAY ’PAPER ’SCISSORS) should return SECOND-WINS.

||#

;;rock is more then sissors
;;paper is more then rock
;;sissors is more then paper

(defun play (player1 player2)
  (cond ((player1-wins-p player1 player2) 'first-wins)
	((tiep player1 player2) 'tie)
	(t 'second-wins)))

(defun player1-wins-p (player1 player2)
  (or 
  ;;win-condition1
  (and (equalp player1 'rock)
       (equalp player2 'scissors))
  ;;win-condition2
  (and (equalp player1 'scissors)
       (equalp player2 'paper))
  ;;win-condition3
  (and (equalp player1 'paper)
       (equalp player2 'rock))))

(defun tiep (player1 player2)
  (equalp player1 player2))

#|| 
4.19. Show how to write the expression (AND X Y Z W) using COND
instead of AND. Then show how to write it using nested IFs instead of
AND.
||#
;;AND X Y Z W should return true since they are not nil unless they are tied to something
;;if anything in the cond returns nil then return nil
;;store the variables returned in cond in a LIST and iterate over them to check if any are nil
(defun test333 (x y z w)
  (let (()))
  (cond ((numberp x) 't))	       
