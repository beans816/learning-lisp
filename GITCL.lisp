;;https://www.cs.cmu.edu/~dst/LispBook/book.pdf

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

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


(defun cube (n)
  (expt n 3))

;;3.6
(defun pythag (x y)
  "Pythagoras formula to return the hypotenuse of a right angled triangle given a and b; c = square root of a^2 + b^2"
  (sqrt (+ (expt x 2) (expt y 2))))

;;3.7
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ gallons-consumed (- final-odometer-reading initial-odometer-reading)))


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


(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) 'second-is-smaller)))

(defun if-compare (x y)
  (if (equal x y)
      (print 'numbers-are-the-same)
      (if (< x y)
	  (print 'first-is-smaller)
	  (if (> x y)
	      (print 'second-is-smaller)))))

(defun and-compare (x y)
  (or (and (equal x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      (and (> x y) 'second-is-smaller)
      'no))

#||
Use COND to write a predicate BOILINGP that takes two inputs,
TEMP and SCALE, and returns T if the temperature is above the
boiling point of water on the specified scale. If the scale is
FAHRENHEIT, the boiling point is 212 degrees; if CELSIUS, the
boiling point is 100 degrees. Also write versions using IF and
AND/OR instead of COND
||#

(defun boilingp (temp scale)
  (let ((bp-celcius 100)
	(bp-farenheit 212))
    (cond ((equal scale 'fahrenheit)
	   ( >= temp bp-farenheit))
	  ((equal scale 'celcius)
	   ( >= temp bp-celcius)))))




(defun and-boiling-p (temp scale)
  (or (and (equal scale 'fahrenheit)
	   (>= temp 212))
      t
      (and (equal scale 'celcius)
	   (>= temp 100))
      t))

;;pg 144
(defun logical-and-p (x y)
  "if x or y are nil returns nil"
  (and x y t))

(defun logical-cond-p (x y)
  "unless x and y are nil returns true"
  (cond	(x t)
	(y t)))

(defun logical-if-p (x y)
  (if (or x y)
      t
      nil))

;;Write LOGICAL-OR. Make sure it returns only T or NIL for its result.

(defun logical-or (x y)
  (if (or x y)
      t
      nil))

;;demorgans theorum

(defun demorgan-and (x y z)
  (not (or (not x)
	   (not y)
	   (not z))))

(defun demorgan-or (x y z)
  (not (and (not x)
	    (not y)
	    (not z))))

(defun nand (x y)
  (not (and x y)))

(defun nand-and (x y)
  (not(nand x y)))

;;https://www.youtube.com/watch?v=JQBRzsPhw2w
;;0 is nil
;;1 is t
;;binary numbers for transistors
;; nil is 0V and t is usually 5V but can be anything other then 0V
;;circuits 0 is off, 1 is on
;;not it like a light switch

(defun not-logic-gate (voltage)
  (not voltage))

(defun and-logic-gate (a b)
  "The only way for the voltage to run through, is if both inputs a AND b are true"
  ;;anything * 0 is 0, this is why all inputs have to be true to return true in and gates
  (* a b))

(defun or-logic-gate (a b)
  (+ a b))

;;nand gate does the opposite of the input of the and gate

(defun nand-gate (a b)
  (not-logic-gate (and-logic-gate a b)))

;;boolean algebra
;; if the input is (a a)
;; then the and logic gate results in a
;;1 *1 is 1
;;0 * 0 is 0

(defun booleantest (on on)
  (nand-logic-gate on on))
;;now it is off and is theeqivilent of just using not

(defun proper-not (voltage)
  (if (null voltage)
      t
      nil))

;;better nand gate using the proper-not
(defun proper-not-and (a b)
  (proper-not (and-logic-gate a b)))

;;nor gate

(defun not-or (a b)
  (proper-not (or a b)))

;;answering the questions in the book

(defun not-not-and-is-and-really (a b)
  (proper-not-and (proper-not a) (proper-not b)))

;; you cant contruct using only nand because it requires two inputs unless you use cons?

;;this should be the answer because of boolean algebra
(defun tryingagain (a b)
  (proper-not-and (proper-not-and a a) (proper-not-and b b)))

(defun not-not-or (a b)
  (not-or (not-or a a) (not-or b b)))

;;the video at 22:59
;;f = xy+ xy'
;;or is addinf and and is *

(defun functionyay (x y)
  (or-logic-gate (and-logic-gate x y)
		 (and-logic-gate x (proper-not-and y y))))

;;to summarise you can make anything with just nand and nor
;;any equation, any logical function and any circuit since circuits are reliant on booelan arithmatic
;;"everything is true if it is not nil"
;;you can create something very complex and not get confused if you use logic gates
;; f = mx + c
;;you can even redo functionyay to only take one function

(defun functionyay (x y)
  (not-not-or (proper-not-and (proper-not x) (proper-not y))
	      (and-logic-gate x (not (proper-not-and (not y) (not y))))))

;;but doing this will confuse someone very quickly :) unless that is what you want, but it can confuse you in the future if you need to fix it!
;;what is in the function does not matter so much, as long as it returns the correct output

;;and an or is not as logically complete as nand and nor because it does not include not

;;Variables and side-affects

;;The SETF macro function assigns a value to a variable. If the variable already has a value, the new value replaces the old one.

;;random shows you the difference of a int and a float
;;if you give it an int with no decimal values
(random 5)
;;got get reterned an int(whole number) with not decimal values

;;ig you give it an input that has a decimal/float you get returned a decimal/float
;;you should probably be careful with this if you are working with money!!!!!
;;most money only has 2 decimal values!!! not 5
(random 5.0)

;;dissasembly and de-coding
;;bottom-up programming and then reassemblly
;;you are given a piece of assembly and you are told that it has a function that can be malware or software, the idea of malware is that its software that does something bad/ mal-icious

;;take an elevator for example, how do you use it?
;;you press the button (input level5) or just input 5 and then the elevator goes to level 5 and opens the doors
;;but the elevator doesnt simply go to level 5 if it is already on level 4 and is moving down so if you want to diasemble this logic:
;; if input > whereitis while state=movingup then return true
;; if input = whereitis then it is state=stationary
;;if input < whereitis while state=movingdown then return false

(defparameter *im-a-global-variable* 'adminpassword)

;;evaltrace basically gives you the logic of a program
;;to build a project in binary you just need logic of a program
;;so make a language that explains logic to the compiler and you have a programming language

;;5.1. Rewrite function POOR-STYLE to create a new local variable Q using LET, instead of using SETF to change P. Call your new function GOOD-STYLE

(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

;;In this book we will use SETF only on global variables, because it is goodprogramming style to avoid changing the values of local variables.
;;so basically from my understanding, its better to use setf for global variables then local variables because it is destructive
;;on the other hand, let works within a scope so its perfect for local variables

(defun good-style (p)
  (let ((a (+ p 5)))
    ;;instead of destructivly re-assigning p, you assign the sum to a which is stored localy
    (list 'result 'is a)))

(defun lambda-style (p)
  ;;closing over the value of p
  (lambda () (list 'result 'is (+ p 5))))

;;side effects of variables can cause bugs where the program compiles but it doesnt work as intended
;;this is why you need unit testing or why you need to continiusly test while you are coding

;;pg 163 keyboard exercise
#|| 
Write a function THROW-DIE that returns a random number from 1
to 6, inclusive. Remember that (RANDOM 6) will pick numbers
from 0 to 5. THROW-DIE doesn’t need any inputs, so its argument
list should be NIL
||#

(defun throw-die()
  "Returns a random number that is 1 >= x >= 7"
  (let ((value (random 7)))
    (if (zerop value)
	(throw-die)
	value)))

(defun throw-dice ()
  "Returns two random numbers in a list"
  (let ((throw (list (throw-die) (throw-die))))
    throw))

(defun snake-eyes-p (throw)
  "Returns T or Nil if the input is '(1 1)"
  (let ((snake-eyes '(1 1)))
    (equalp throw snake-eyes)))

(defun boxcars-p (throw)
  "Returns T or nil if the input it '(6 6)"
  (let ((boxcars '(6 6)))
    (equalp throw boxcars)))

(defun instant-win-p (throw)
  "Returns T if the sum of your throw equals to 7 or 11, you win!"
  (let ((sum (apply '+ throw)))
    (or (equalp 7 sum)
	(equalp 11 sum))))

(defun instant-loss-p (throw)
  "Returns T if the sum of your throw equals to 2, 3 or 12, you lose!"
  (let ((sum (apply '+ throw)))
    (or (equalp 2 sum)
	(equalp 3 sum)
	(equalp 12 sum))))

(defun say-snake-eyes (throw)
  "Says snake-eyes when true, when not true returns nil"
  (when (snake-eyes-p throw)
    'SNAKE-EYES))

(defun say-boxcars (throw)
  "Says boxcars when true, when not true returns nil"
  (when (boxcars-p throw)
    'BOX-CARS))

(defun sum-dice (throw)
  (apply '+ throw))

(defun say-points (throw)
  (list 'your 'point 'is (sum-dice throw)))

(defun say-instant-win (throw)
  "Says snake-eyes when true, when not true returns nil"
  (when (instant-win-p throw)
    'you-win))

(defun say-instant-loss (throw)
  "Says boxcars when true, when not true returns nil"
  (when (instant-loss-p throw)
    'you-lose))

(defun say-throw (throw)
  "Says boxcars or snake-eyes if they are true"
  (or (say-boxcars throw)
      (say-snake-eyes throw)
      (sum-dice throw)))



#||
If you don’t win or lose on the first throw of the dice, the value you
threw becomes your ‘‘point,’’ which will be explained shortly.
Write a function (CRAPS) that produces the following sort of
behavior. Your solution should make use of the functions you wrote
in previous steps.
> (craps)
(THROW 1 AND 1 -- SNAKEYES -- YOU LOSE)
> (craps)
(THROW 3 AND 4 -- 7 -- YOU WIN)
> (craps)
(THROW 2 AND 4 -- YOUR POINT IS 6)
||#
;;

;;if yo get a win or loss condition you put in say-throw, if you dont then you say the point

(defun craps ()
  (cond ((instant-win-p throw)
	 (list 'throw (car throw) 'and (cadr throw) '--
	       (say-throw throw) '-- 'You 'win))
	((instant-loss-p throw)
	 (list 'throw (car throw) 'and (cadr throw) '--
	       (say-throw throw) '-- 'You 'lose))
	(let ((throw (throw-dice)))
	  (t (list  'throw (car throw) 'and (cadr throw) '--
		    'your 'point 'is (sum-dice throw))
	     ;;store the point and recall the function
	     ;;if the point is the same as the previous point the you win
	     ;;if the point is not the same as the previous point then try again
	     ;;but if you throw snake-eyes you lose.
	     ))))


(defun try-for-point (previous-point)
  (let* ((throw (throw-dice))
	 (new-point (sum-dice throw)))
    (if (equalp 7 new-point)
	(list 'throw (car throw) 'and (cadr throw) '--
	      (say-throw throw) '-- 'You 'lose)
	(if (equalp new-point previous-point)
	    (list 'throw (car throw) 'and (cadr throw) '--
		  (say-throw throw) '-- 'you 'win)
	    (list 'throw (car throw) 'and (cadr throw) '--
		  (say-throw throw) '-- 'try 'again)))))

;;pg 181
(defparameter *line*
  '(roses are red))

(reverse *line*)
;;(red are roses)

(first (last *line*))
;;red

(nth 1 line)
;;are

(reverse (reverse *line*))
;;(roses are red)

(append *line* (list (first *line*)))
;;(roses are red roses)

(append (last *line*) *line*)
;;(red roses are red)

(list (first *line*) (last *line*))
;;(roses (red))

(cons (last *line*) *line*)
;;((red) roses are red)

(remove 'are *line*)
;;(roses red)

(append *line* '(violets are blue))
;;(roses are red violets are blue)

;;Use the LAST function to write a function called LAST-ELEMENT
;;that returns the last element of a list instead of the last cons cell.
(defun last-element (list)
  (first (last list)))

;;another version of LAST-ELEMENT using REVERSE instead of last

(defun last-element2 (list)
  (first (reverse list)))
;;the first one is faster because reverse is like dolist

;;Write another version using NTH and LENGTH.
(defun last-element3 (list)
  (nth (1- (length list)) list))
;;the first one is still faster then this one

;;Use REVERSE to write a NEXT-TO-LAST function that returns the
;;next-to-last element of a list. Write another version using NTH.
(defun next-to-last (list)
  (second (reverse list)))

(defun next-to-last2 (list)
  (nth (- (length list) 2) list))

;;Write a function MY-BUTLAST that returns a list with the last element
;;removed. (MY-BUTLAST ’(ROSES ARE RED)) should return the list
;;(ROSES ARE). (MY-BUTLAST ’(G A G A)) should return (G A G)

(defun my-butlast (list)
  (reverse (rest (reverse list))))

;;(defun mystery (x) (first (last (reverse x))))

(defun mystery (x)
  (first x))

#|| 
6.10. A palindrome is a sequence that reads the same forwards and
backwards. The list (A B C D C B A) is a palindrome; (A B C A B C)
is not. Write a function PALINDROMEP that returns T if its input is a
palindrome
||#

;;chech to see if the list is symmetrical
;;is the reverse of the list the same as te normal order of the list?

(defun palindromep (list)
  (equalp list (reverse list)))

#||
Write a function MAKE-PALINDROME that makes a palindrome out
of a list, for example, given (YOU AND ME) as input it should return
(YOU AND ME ME AND YOU).
||#

(defun make-palindrome (list)
  (append list (reverse list)))
;;CL-USER> (make-palindrome '(1 2 3 4 5 6 7))
;;(1 2 3 4 5 6 7 7 6 5 4 3 2 1)

(defun make-palindrome2 (list)
  (append list (rest (reverse list))))
;;CL-USER> (make-palindrome2 '(1 2 3 4 5 6 7))
;;(1 2 3 4 5 6 7 6 5 4 3 2 1)

;;6.12 no it just returns it doesnt cycle through and make a new list

;;intersection takes the common elements in two lists

;;6.13 you get nil because () is the only common element
;;6.14 you should get all the common elements
;;6.15
(defun contains-article-p (set)
  (intersection '(the a an) set))

(defun contains-article-p2 (set)
  (or (member 'the set)
      (member 'a set)
      (member 'an set)))

;;and and or are two completely different functions and i spent 30 minutes trying to make one the other

(defun add-vowels (set)
  (union set '(A E I O U)))

;;the first value of set-difference needs to be copied to return anything
;;if the first value is nil the return value is nil
;;but if the first value is full of symbols and the last value is nil it just returns the first value

(defun my-subsetp (setx sety)
  (if (set-difference setx sety)
      'nil
      't))

;;6.22. Suppose the global variable A is bound to the list (SOAP WATER).
;;What will be the result of each of the following expressions?

(defparameter *a* (list 'SOAP 'WATER))

;;(union *a* '(no soap radio))
;;(radio no soap water)

;;(intersection *a* (reverse *a*))
;;(reverse *a*) = (water soap)
;;all the common elements (water soap)

;;(set-difference a '(stop for water))
;;(soap)

;;(set-difference a a)
;; there is no difference between the two lists so it doesnt matter

;;(member 'soap *a*)
;;(soap water)

;;(member 'water *a*)
;;(water)

;;(member 'washcloth *a*)
;;nil

;;The cardinality of a set is the number of elements it contains. What
;;Lisp primitive determines the cardinality of a set? the length

;;check if something is cardinal like ying yang
(defun set-equalp (setx sety)
  (and (equalp (length setx) (length sety))
       (subsetp setx sety)))

;;if X is a subset of Y but not equal
;;to Y then it is the function properset

(defun properset (setx sety)
  (and (subsetp setx sety)
       (not (set-equalp setx sety))))

;;why abc is not a proper subset of bca

;;6.26. We are going to write a program that compares the descriptions of two
;; objects and tells how many features they have in common. The
;; descriptions will be represented as a list of features, with the symbol
;; -VS- separating the first object from the second. Thus, when given a
;; list like
;; (large red shiny cube -vs-
;; small shiny red four-sided pyramid)
;; the program will respond with (2 COMMON FEATURES). We will
;; compose this program from several small functions that you will write
;; and test one at a time.
;; so red and shiny are common things in the list

;;a. Write a function RIGHT-SIDE that returns all the features to the
;; right of the -VS- symbol. RIGHT-SIDE of the list shown above
;; should return (SMALL SHINY RED FOUR-SIDED PYRAMID).
;; Hint: remember that the MEMBER function returns the entire
;; sublist starting with the item for which you are searching. Test your
;; function to make sure it works correctly.

;;this is treating -vs- as the starting point value and then excluding it like list when you look sraight ahead and turn your head to the right and to the left. or you are treating -vs- like a mirror and trying to find the similarities between two things, you you should know tha you can make a lage red shiny cube from a small shiny red foursided pyramid so they are also similiar in that way.

;;(large red shiny cube -vs- small shiny red foursided pyramid)

(defun right-side (list)
  (rest (member '-vs- list)))

;; Write a function LEFT-SIDE that returns all the features to the left
;; of the -VS-. You can’t use the MEMBER trick directly for this one,
;; but you can use it if you do something to the list first.

(defun left-side (list)
  (rest (member '-vs- (reverse list))))

;;Write a function COUNT-COMMON that returns the number of
;;features the left and right sides of the input have in common.

;;treat the two as a list and then compare them using subset

(defun count-common (list)
  (length (intersection (right-side list) (left-side list))))

;;now it is red and shiny but now we must count how many elements are in the list

;;Write the main function, COMPARE, that takes a list of features
;;describing two objects, with a -VS- between them, and reports the
;;number of features they have in common. COMPARE should return
;;a list of form (n COMMON FEATURES).

;;add some words i suppose

(defun compare (list)
  (list (count-common list) 'common 'features))
;;lol

;;this program needs to be more symmetrical because the lists are similar in four ways
;;http://s3.amazonaws.com/illustrativemathematics/images/000/003/233/large/Pyramid_in_Cube_395485726d46ab8fabecc1085c48d4ca.png?1403105489
;;its egypt now
;;(large red shiny cube -vs- four small shiny red foursided pyramids)

(defun set-equal ()
  (setq (equal (right-side list) (left-side list))))

;;the book is a bit outdated
(defparameter *words*
  '((one . un)
    (two . deux)
    (three . trois)
    (four . quatre)
    (five . cinq)))

;;6.27 technically but doing that is a bit inelegant
;;'(assoc . rassoc) you use rassoc to search by using the cdr and assoc to seach by the car

;;(assoc 'three *words*)
;;(rassoc 'trois *words*)

(defparameter *things*
  '((object1 large green shiny cube)
   (object2 small red dull metal cube)
   (object3 red small dull plastic cube)
   (object4 small dull blue metal cube)
   (object5 small shiny red four-sided pyramid)
   (object6 large shiny green sphere)))

;;this is bad use of assoc

(defun description (x)
  (rest (assoc x *things*)))

(defun differences (x y)
  (set-exclusive-or (description x)
		    (description y)))
;;this is like set-difference!

;;6.29 length

(defparameter *books*
    '((Pride-and-Prejudice . Jane-Austen)
      (I-can-count-the-petals-of-flowers . John-Wahl-Stacey-Wahl)
      (Symbols-of-sacred-science . Rene-Guenon)
      (One-is-fun! . Delia-Smith)
      (A-Country-Called-Stratherrick . Alan-B-Lawson)))

(defun who-wrote (name-of-book)
  (cdr (assoc name-of-book *books*)))

;;it shouldnt change at all

;;the table would have to have the author name first and then it would work with assoc

(defparameter *atlas*
  '((pennsylvania (pittsburg
		   johnstown))
    (new-jersey (newark
		 princeton
		 trenton))
    (ohio (columbus))))

;; In this problem we will simulate the behavior of a very simple-minded
;; creature, Nerdus Americanis (also known as Computerus Hackerus ).
;; This creature has only five states: Sleeping, Eating, Waiting-for-a-
;; Computer, Programming, and Debugging. Its behavior is cyclic: After
;; it sleeps it always eats, after it eats it always waits for a computer, and
;; so on, until after debugging it goes back to sleep for a while.
;;very cringe but kinda cool

;; What type of data structure would be useful for representing the
;; connection between a state and its successor? Write such a data
;; structure for the five-state cycle given above, and store it in a global
;; variable called NERD-STATES

(defparameter *nerd-states*
  '(sleeping eating waiting-for-a-computer programming debugging))

(defun nerd-cycle-sucessor (state)
  (second (member state *nerd-states*)))

;;sleeping eating
;;eating waiting-for-a-computer
;;waiting-for-a-computer programming
;;programming debugging
;;debugging sleeping

(defparameter *nerd-states*
  '((sleeping . eating)
    (eating . waiting-for-a-computer)
    (waiting-for-a-computer . programming)
    (programming . debugging)
    (debugging . sleeping)))

(defun nerdus (state)
  (cdr (assoc state *nerd-states*)))

(defun sleepless-nerd (state)
  (if (equalp 'debugging state)
      'eating
      (nerdus state)))

(defun sleepless-days (one-state-cycle-number)
  (let ((new-state 'eating))
    (dotimes (i one-state-cycle-number new-state)
      (setf new-state (sleepless-nerd new-state))
      (print new-state))))

;;nerd on caffiene is xn + 2
(defun nerd-on-caffiene (state)
  (nerdus (nerdus state)))

(defun nerd-cycle-on-caffiene (one-cycle-number)
  (let ((new-state 'programming))
    (dotimes (i one-cycle-number new-state)
      (setf new-state (nerd-on-caffiene new-state))
      (print new-state))))
;;3

;;exercises
;;swap-first-last
;;defun laste element of a list


(defun middle-elements (list)
  (butlast (rest list)))


(defun swap-first-last (list)
  (append (last list) (middle-elements list) (list (first list))))

;;rotate like a cesearean cipher clockwise
;;the first becomes the last
;;the second becomes the first

(defun rotate-left (list)
  (append (rest list) (list (first list))))

(defun rotate-right (list)
  (append (last list) (butlast list)))

;;when you do set-difference '(a b c) '(a b c) the output is the same when ou rotate them. You do the same when you have the input as () ()
;;equalp nil nil returns t

;;cons 

;;Show how to transform the list (A B C D) into a table so that the
;;ASSOC function using the table gives the same result as MEMBER
;;using the list.

(defparameter *test* '((a . nil) (b . nil) (c . nil) (d . nil)))

;;this is very hacky

(defparameter *rooms*
  '((living-room (north front-stairs)
     (south dining-room)
     (east kitchen))
    (upstairs-bedroom (west library)
     (south front-stairs))
    (dining-room (north living-room)
     (east pantry)
     (west downstairs-bedroom))
    (kitchen (west living-room)
     (south pantry))
    (pantry (north kitchen)
     (west dining-room))
    (downstairs-bedroom (north back-stairs)
     (east dining-room))
    (back-stairs (south downstairs-bedroom)
     (north library))
    (front-stairs (north upstairs-bedroom)
     (south living-room))
    (library (east upstairs-bedroom)
     (south back-stairs))))

#||
Write a function CHOICES that takes the name of a room as input
and returns the table of permissible directions Robbie may take from
that room. For example, (CHOICES ’PANTRY) should return the
list ((NORTH KITCHEN) (WEST DINING-ROOM)). Test your
function to make sure it returns the correct result.
||#

(defun choices (name-of-room)
  (rest (assoc name-of-room *rooms*)))

#||
Write a function LOOK that takes two inputs, a direction and a
room, and tells where Robbie would end up if he moved in that
direction from that room. For example, (LOOK ’NORTH
’PANTRY) should return KITCHEN. (LOOK ’WEST ’PANTRY)
should return DINING-ROOM. (LOOK ’SOUTH ’PANTRY)
should return NIL. Hint: The CHOICES function will be a useful
building block.
||#

(defun look (direction room)
  (second (assoc direction (choices room))))

(defparameter *loc* 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to place by setting loc"
  (setf *loc* place))

(defun how-many-choices (loc)
  (length (choices loc)))

(defun upstairsp (loc)
  (or (equalp loc 'library)
      (equalp loc 'upstairs-bedroom)))

(defun onstairsp (loc)
  (or (equalp loc 'front-stairs)
      (equalp loc 'back-stairs)))

(defun where()
  (cond ((upstairsp *loc*) (list 'Robbie 'is 'upstairs 'in 'the *loc*))
	((onstairsp *loc*) (list 'Robbie 'is 'on 'the *loc*))
	(t (list 'Robbie 'is 'downstairs 'in 'the *loc*))))

#|| Write a function MOVE that takes one input, a direction, and moves
Robbie in that direction. MOVE should make use of the LOOK
function you wrote previously, and should call SET-ROBBIE-
LOCATION to move him. If Robbie can’t move in the specified
direction an appropriate message should be returned. For example,
if Robbie is in the pantry, (MOVE ’SOUTH) should return
something like (OUCH! ROBBIE HIT A WALL). (MOVE
’NORTH) should change Robbie’s location and return (ROBBIE IS DOWNSTAIRS IN THE KITCHEN).
||#

(defun move (direction)
  (let ((newloc (look direction *loc*)))
    (if newloc
	(progn (set-robbie-location newloc) (where))
	(list 'Ouch! 'Robbie 'Hit 'a 'wall))))
#||
Starting from the pantry, take Robbie to the library via the back
stairs. Then take him to the kitchen, but do not lead him through the
downstairs bedroom on the way
directions -> west, north, east :D
||#

;;finally finished it!

;;advanced topic pg 204
;;substitution is almost just like substitution in algebra very simple -> where x is 5 find the value of y through substitution x = xy is the same as 5 = 5y because you substitute all the values of x for 5. But it doesn't work on nested lists/trees because it only looks at the top level list. In this case it works for substituting symbols or numbers for symbols or numbers.
;;subst is the same but it works on nested lists/trees so EVERYTHING not just the top level list
;;replace is similar to substitution or substitute
;;sublis works like this (sublis '((dotted . pair) (list . array)) '(List that needs to be subsituted for its dotted pair))
(sublis '((spring . may) (showers . flowers)) '(spring showers))
;;output will be (may flowers) a.k.a spring showers bring may flowers and it makes you smile.

#|| 6.42. Write a function called ROYAL-WE that changes every occurrence of
the symbol I in a list to the symbol WE. Calling this function on the list
(IF I LEARN LISP I WILL BE PLEASED) should return the list (IF
WE LEARN LISP WE WILL BE PLEASED)
||#

(defparameter *connections-to-the-past*
  '((thai-greetings . sawatdee-ka)
    (thai-goodbye . sawatdee-ka)
    (farewell . greetings)
    (good-tidings-upon-you . fare-thee-well)
    (thai-thank-you-kindly . kawpkun-ka)
    (thai-are-you-well . sabaai-dee-mai)
    (thai-I-am-well . sabaai-dee)
    (thai-well-ness-no-worries . sabaai-dee)
    (thai-good-serenity-relaxed . sabaai)
    (thai-yes . chai)
    (thai-no . mai)
    (thai-you . kun)
    (thai-me-I . chan)
    (thai-go . pai)
    (padma . lotus)
    (sanscrit-varthi . spin-cycle)
    (sanscrit-chakra . circle)
    (latin-borea . north)
    (latin-hyber . past)
    (latin-septen . seven)
    (latin-triones . stars)
    (old-latin-septentriones . ursa-minor)
    (latin-ursa-minor . little-bear)
    (celtic-arth . bear)
    (seasons . cycle-ursa-minor)))

(defun translate-meaning (list)
  (sublis *similar-meaning* list))

;;now you can speak thai, sanscrit or latin with me by breaking down the meaning of words-but they are romanised not the correct symbol. there is no punctuation in thai because the language already insinuates it. But it would be better if you actually learnt the language in your head since its faster.

;;(translate-meaning '(greetings are-you-well I-am-well thank-you-kindly goodbye))


;;atom is a predicate - if something is not a bunch of grouped things like a list then it is atomic a.k.a a building block

(defun add-to-end-1 (x y)
  (append x (list y)))

(defun add-to-end-2 (x y)
  (reverse (cons y (reverse x))))

(defparameter *text* '(b a n a n a - p a n d a))
;;remove all a's
(remove 'a *text*)
(remove 'a *text* :count 3)
;;Remove also accepts a :FROM-END keyword. If its value is non-NIL,
;;then REMOVE starts from the end of the list instead of from the beginning.
(remove 'a *text* :count 3 :from-end t)

(defparameter *cards* '((3 clubs) (5 diamonds) (ace spades)))

(member '(5 diamonds) *cards* :test #’equal)
;;((5 DIAMONDS) (ACE SPADES))

;;'time (time is a symbol and a constant)
;;list * 'after *


(defun mydouble (n)
  (* n 2))

(defun mylistdouble (list)
  (mapcar #'mydouble list))

;;(mapcar (lambda (n) (* n 2)) '(2 4 6 5))
;;(mapcar #'first '((1 3 4) (3 4 6) (6 7 9)))
;;mapcar maps every element in a list and 'applies' the function to it
;;(mapcar #'+ '(1 2 3 4) '(1 2 3 4)) using two lists of equal length and adds them and then con's the sum values together to form a list 
;;(mapcar #'first '())
;;(mapcar (lambda (&rest list) (apply #'+ list)) '(1 2 3 4 5) '(1 2 3 4 5))
;;(mapcar (lambda (n y) (+ n y)) '(1 2 3 4 5) '(1 2 3 4 5))
;;(mapcar #'+ '(1 2 3 4) '(1 2 3 4))
;;(mapcar #'first '((1 2 3) (3 2 4) (8 7 6)))

;;Write an ADD1 function that adds one to its input. Then write an
;;expression to add one to each element of the list (1 3 5 7 9).
(defun add1 (n)
  (1+ n))

(defun add1list (list)
  (mapcar #'1+ list))

(defparameter *daily-planet* '((olsen jimmy 123-76-4535 cub-reporter)
			       (kent clark 089-52-6787 reporter)
			       (lane lois 951-26-1438 reporter)
			       (white perry 355-16-7439 editor)))

;;Each table entry consists of a last name, a first name, a social security
;;number, and a job title. Use MAPCAR on this table to extract a list of
;;social security numbers
;;this would be useful to help hospitals

(defun ssn()
  (mapcar #'third *daily-planet*))

;;Write an expression to apply the ZEROP predicate to each element of
;;the list (2 0 3 4 0 -5 -6). The answer you get should be a list of Ts and
;;NILs.

(mapcar #zerop '(2 0 3 4 0 -5 -6))

#||
Suppose we want to solve a problem similar to the preceding one, but
instead of testing whether an element is zero, we want to test whether it
is greater than five. 
We can’t use > directly for this because > is a
function of two inputs; MAPCAR will only give it one input. 
Show
how first writing a one-input function called GREATER-THAN-FIVE-
P would help.
||#

(mapcar (lambda (x) (> x 5)) )

(defun greater-than-five-p (n)
  (> n 5))

(mapcar #'greater-than-five-p '(1 2 34 2 3))


;;7.5 LAMBDA EXPRESSIONS
;;this can be very useful
;;(mapcar #’(lambda (x) (list ’hi ’there x))
;;’(joe fred wanda))
;;((HI THERE JOE) (HI THERE FRED) (HI THERE WANDA))
;;(lambda (n) (- 7 n))

;;Write a lambda expression that returns T if its input is T or NIL, but
;;NIL for any other input.

;;(lambda (p) (or (equal p t) (null p))

;;Write a function that takes a list such as (UP DOWN UP UP) and
;;"flips" each element, returning (DOWN UP DOWN DOWN). Your
;;function should include a lambda expression that knows how to flip an
;;individual element, plus an applicative operator to do this to every
;;element of the list.

;;(lambda (p) (cond (equal p t) nil) ((equal p nil) t))

(defun gravity-flip (list)
  (mapcar (lambda (gravity)
            (cond ((equal gravity 'up) 'down)
				  ((equal gravity 'down) 'up)
				  (t gravity)))
	  list))

;;CL-USER> (gravity-flip '(I am going up))
;;(I AM GOING DOWN)

;;CL-USER> (gravity-flip '(I am going up))
;;(I AM GOING DOWN)

;;(find-if #’oddp ’(2 4 6 7 8 9))
;;7
;;find-if returns the first element where the function returns true

(defun sieve-all-odd (list)
  (find-if (lambda (n) (if (oddp n) n)) list))
;;doesnt work T^T


;;writing assoc with if
;;table of numbers to thai

(defparameter *eng-num-to-thai-numb*
  '((zero . sun)
    (one . nung)
    (two . song)
    (three . sam)
    (four . si)
    (five . ha)
    (six . hok)
    (seven . chet)
    (eight . paet)
    (nine . kao)
    (ten . sip)))

(defun my-assoc-encode (key table)
  (find-if #'(lambda (entry)
	       (equal key (first entry)))
	   table))
#||
Write a function that takes two inputs, X and K, and returns the first
number in the list X that is roughly equal to K. Let’s say that ‘‘roughly
equal’’ means no less than K − 10 and no more than K + 10.
||#

(defun thing (x k)
  (find-if #'(lambda (n)
	       (> (+ n 10) k (- n 10)))
	   x))

#|| Write a function FIND-NESTED that returns the first element of a list
that is itself a non-NIL list.
||#
;;this is useful for encoding/translating

(defun find-nested (list)
  (first (find-if (lambda (element)
		    (and (listp element)
			 (not (null element))))
		  list)))

;;MINI KEYBOARD EXERCISE

;;create a global variable called note-table
;;piano!!!

(defparameter *note-table*
  '((1 . c)
    (2 . c#)
    (3 . d)
    (4 . d#)
    (5 . e)
    (6 . f)
    (7 . f#)
    (8 . g)
    (9 . g#)
    (10 . a)
    (11 . a#)
    (12 . b)))

;; Write a function called NUMBERS that takes a list of notes as input
;; and returns the corresponding list of numbers. (NUMBERS ’(E D C
;; D E E E)) should return (5 3 1 3 5 5 5). This list represents the first
;; seven notes of ‘‘Mary Had a Little Lamb.’’
;;this is just encoding again 

(defun numbers (list-notes)
  (mapcar #'(lambda (note)
	      (car (rassoc note *note-table*)))
	  list-notes))

#||
Write a function called NOTES that takes a list of numbers as input
and returns the corresponding list of notes. (NOTES ’(5 3 1 3 5 5
5)) should return (E D C D E E E). Hint: Since NOTE-TABLE is
keyed by note, ASSOC can’t look up numbers in it; neither can
RASSOC, since the elements are lists, not dotted pairs. Write your
own table-searching function to search NOTE-TABLE by number
instead of by note.
||#

(defun notes (list-numbers)
  (mapcar #'(lambda (number)
	      (cdr (assoc number *note-table*)))
	  list-numbers))

;;What can be said about (NOTES (NOTES X)) and (NUMBERS
;;(NUMBERS X))
;;For X a list of notes:
;;X = (NOTES (NUMBERS X))
;;For X a list of numbers:
;;X = (NUMBERS (NOTES X))

;;it will return nil or the original input

#||
o transpose a piece of music up by n half steps, we begin by adding
the value n to each note in the piece. Write a function called RAISE
that takes a number n and a list of numbers as input and raises each
number in the list by the value n. (RAISE 5 ’(5 3 1 3 5 5 5)) should
return (10 8 6 8 10 10 10), which is ‘‘Mary Had a Little Lamb’’
transposed five half steps from the key of C to the key of F.
||#

(defun raise (n number-list)
  (mapcar (lambda (number) (+ number n)) number-list))

#||
Sometimes when we raise the value of a note, we may raise it right
into the next octave. For instance, if we raise the triad C-E-G
represented by the list (1 5 8) into the key of F by adding five to
each note, we get (6 10 13), or F-A-C. Here the C note, represented
by the number 13, is an octave above the regular C, represented by
1. Write a function called NORMALIZE that takes a list of numbers
as input and ‘‘normalizes’’ them to make them be between 1 and 12.
A number greater than 12 should have 12 subtracted from it; a
number less than 1 should have 12 added to it. (NORMALIZE ’(6
10 13)) should return (6 10 1).
||#

(defun normalize (list-numbers)
  (mapcar (lambda (n)
            (cond ((> n 12) (- n 12))
                  ((> 1 n) (+ n 12))
                  (t n)))
	  list-numbers))

#||
Write a function TRANSPOSE that takes a number n and a song as
input, and returns the song transposed by n half steps.
(TRANSPOSE 5 ’(E D C D E E E)) should return (A G F G A A A).
Your solution should assume the availability of the NUMBERS,
NOTES, RAISE, and NORMALIZE functions. Try transposing
‘‘Mary Had a Little Lamb’’ up by 11 half steps. What happens if
you transpose it by 12 half steps? How about − 1 half steps
||#

;;take the notes
;;covert it to numbers
;;raise by n
;;normalise
;;convert to notes

(defun transpose (n song)
  (let ((numbers (numbers song)))
    (notes (normalize (raise n numbers)))))

;;))

(remove-if #'numberp '(2 for 1 sale))

(remove-if #'oddp '(1 2 3 4 5 6 7))

(remove-if #'(lambda (x) (not (plusp x))) '(2 0 -4 6 -8 10))

(remove-if-not #'plusp '(2 0 -4 6 -8 10))

(remove-if-not #'oddp '(2 0 -4 6 -8 10))

(remove-if-not #'(lambda (x) (> x 3))
               '(2 4 6 8 4 2 1))

(remove-if-not #'numberp
	       '(3 apples 4 pears and 2 little plums))

(remove-if-not #'symbolp
	       '(3 apples 4 pears and 2 little plums))

(defun count-zeros (x)
  (length (remove-if-not #’zerop x)))

(count-if #'zerop x)


;;.11. Write a function to pick out those numbers in a list that are greater than one and less than five.

(defun pick-out (list)
  (remove-if-not (lambda (x) (> 5 x 1)) list))

;;.12. Write a function that counts how many times the word ‘‘the’’ appears
;;in a sentence.

(defun the-count (list)
  (count 'the list))

;;Write a function that picks from a list of lists those of exactly length two.
;; ((1 2) (1 2 3) (1 2 3) (1 2))

(defun nested-two (list)
  (remove-if-not #'(lambda (x) (equal 2 (length x))) list))

;;Here is a version of SET-DIFFERENCE written with REMOVE-IF:

(defun my-setdiff (x y)
  (remove-if #'(lambda (e) (member e y))
	     x))

(defun my-intersect (list1 list2)
  (remove-if-not (lambda (x) (member x list1))
		 list2))

;;union joins 2 lists together and removes doubles
(defun my-union (list1 list2)
  (append list1 (remove-if #'(lambda (x)
			       (member x list1))
			   list2)))

#||
In this keyboard exercise we will manipulate playing cards with
applicative operators. A card will be represented by a list of form (rank
suit), for example, (ACE SPADES) or (2 CLUBS). A hand will be
represented by a list of cards.
||#

#||
Write the functions RANK and SUIT that return the rank and suit of
a card, respectively. (RANK ’(2 CLUBS)) should return 2, and
(SUIT ’(2 CLUBS)) should return CLUBS.
||#

;;naming convention list-rs stands for a list that contains the rank and suit
;;list-rs -> '(2 CLUBS)
;;________________________________________________________________________
;;________________________________________________________________________


(defun rank (list-rs)
  (first list-rs))

(defun suit (list-rs)
  (second list-rs))

(defparameter *my-hand*
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun count-suit (suit hand-of-cards)
  (length (remove-if-not #'(lambda (x)
                             (equal suit (suit x)))
                         hand-of-cards)))

(defun count-suit1 (suit hand-of-cards)
  (count suit hand-of-cards :key #'suit))

(defparameter *colours*
  '((clubs black)
     (diamonds red)
     (hearts red)
    (spades black)))

(defun retrieve-colour (suit)
  "Retrieves the colour of a card from the table colours"
  (second (assoc suit *colours*)))

(defun colour-of (card)
  (retrieve-colour (suit card)))

;;Write a function FIRST-RED that returns the first card of a hand
;;that is of a red suit, or NIL if none are.
;;check if red is in the table and if its not

(defun first-red (hand-of-cards)
  (find-if #'(lambda (card)
              (equalp (colour-of card) 'red))
           hand-of-cards))

;;Write a function BLACK-CARDS that returns a list of all the black
;;cards in a hand.

(defun black-cards (hand-of-cards)
  (mapcar #'(lambda (card)
              (equalp (colour-of card) 'black))
          hand-of-cards))

#||
list (ACE). Hint: First extract all the cards of the specified suit,
then use another operator to get the ranks of those cards.
||#

;;write a function that takes two inputs
;;a suit and a hand-of-cards
;;returns the ranks of all cards
;;what-ranks
;;(WHAT-RANKS ’DIAMONDS MY-HAND) should return the list (2 4)
;;make a function that returns the list of all the diamonds in my hand

(defun list-suits (suit hand-of-cards)
  "Input (list-suits 'diamonds *my-hand*) Output ((2 Diamonds) (4 Diamonds))"
  (remove-if-not #'(lambda (x)
                     (equal suit (suit x)))
                 hand-of-cards))

;; break it down more to find the first rank of the first list

;; (defun first-rank (suit hand-of-cards)
;;   "Input (first-rank 'diamonds *my-hand*) Output 2"
;;   (list (caar (list-suits suit hand-of-cards))))

;;how to get the first elements of two nested lists
;;for every nested list in the list, return the first-rank
;;so basically just use mapcar...map every car value into a list :)

;; (defun all-ranks (suit hand-of-cards)
;;   "Input (all-ranks 'diamonds *my-hand*) Output (2 4)"
;;   (mapcar #'(lambda (x)
;;                     (first x))
;;                 (list-suits suit hand-of-cards))))

(defun what-ranks (suit hand-of-cards)
  "Input (all-ranks 'diamonds *my-hand*) Output (2 4)"
  (mapcar #'first
          (list-suits suit hand-of-cards)))

;; Set the global variable ALL-RANKS to the list
;; (2 3 4 5 6 7 8 9 10 jack queen king ace)
;; Then write a predicate HIGHER-RANK-P that takes two cards as
;; input and returns true if the first card has a higher rank than the
;; second. Hint: look at the BEFOREP predicate on page 171 of
;; Chapter 6.

(defparameter *all-ranks*
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))

;; (defun higher-rank-p-test1 (card1 card2)
;;   (> (rank card1) (rank card2)))

;;now it has to accept symbols as numbers
;;jack is 11, queen is 12, king is 13 and ace is 14 or 1
(defun beforep (x y)
  "Returns true if X appears before Y in List"
  (member y (member x *all-ranks*)))

(defun higher-rank-p (card1 card2)
  "Returns true if the first card has a higher rank than the second"
  (beforep (rank card2) (rank card1)))

;; Write a function HIGH-CARD that returns the highest ranked card
;; in a hand. Hint: One way to solve this is to use FIND-IF to search a
;; list of ranks (ordered from high to low) to find the highest rank that
;; appears in the hand. Then use ASSOC on the hand to pick the card
;; with that rank. Another solution would be to use REDUCE (defined
;; in the next section) to repeatedly pick the highest card of each pair.

;; make a list of all the ranking in a hand of cards
;; compare all of them to find the highest rank with find-if


;; (defun list-of-ranks-in-hand (hand-of-cards)
;;   (mapcar #'first hand-of-cards))
;;my-hand is
;; (defparameter *my-hand*
;;   '((3 hearts)
;;     (5 clubs)
;;     (2 diamonds)
;;     (4 diamonds)
;;     (ace spades)))

;;ranks-of-cards is
;; (list-of-ranks-in-hand *my-hand*)
;; (3 5 2 4 ACE)

;; (defun rank-card-compare (ranks-of-cards)
;;   (let ((all-ranks (reverse *all-ranks*)))
;;     (find-if #'(lambda (x)
;;                  (find x ranks-of-cards))
;;              all-ranks)))

(defun high-card (hand-of-cards)
  "Returns the highest ranked card in a hand"
  (assoc (find-if #'(lambda (x)
                      (assoc x hand-of-cards))
                  *all-ranks* :from-end t)
         hand-of-cards))

#||
.16. Suppose we had a list of sets ((A B C) (C D A) (F B D) (G)) that we
wanted to collapse into one big set. If we use APPEND for our
reducing function, the result won’t be a true set, because some elements
will appear more than once. What reducing function should be used
instead?
||#

;; CL-USER> (reduce #'union
;;                      '((one un) (two deux) (three trois)))
;; (DEUX TWO ONE UN THREE TROIS)
;; CL-USER

;; Write a function that, given a list of lists, returns the total length of all
;; the lists. This problem can be solved two different ways.

(defun length-of-nested (list-of-lists)
  (reduce #'+ (mapcar #'(lambda (nested-list)
                      (length nested-list))
                  list-of-lists)))

;;(REDUCE #’+ NIL) returns 0, but (REDUCE #’* NIL) returns 1.
;;Why do you think this is?

;;when you times nil to nil it becomes true like a nand gate
;;two negatives make a truth
;;two black holes cancel eachother out


;;  Write a function ALL-ODD that returns T if every element of a list of
;; numbers is odd.

(defun all-odd (list)
  (every #'oddp list))

;; Write a function NONE-ODD that returns T if every element of a list of
;; numbers is not odd.

(defun none-odd (list)
  (every #'even list))

;; Write a function NOT-ALL-ODD that returns T if not every element of
;; a list of numbers is odd.

(defun not-all-odd (list)
  (some #'(lambda (x)
              (evenp x))
        list))

;; Write a function NOT-NONE-ODD that returns T if it is not the case
;; that a list of numbers contains no odd elements
;; this is ridiculous

(defun not-none-odd (list)
  (every #'oddp list))

;; Are all four of the above functions distinct from one another, or are
;; some of them the same? Can you think of better names for the last
;; two? some-odd and all-odd

;;What is an applicative operator?
;;something that is very useful

;;Why are lambda expressions useful? Is it possible to do without them?
;;they are useful because you can break large lists into thier eserate elements and check them. It's possible to do without them but it would be a huge confusing mess full of mistakes

;;Show how to write FIND-IF given REMOVE-IF-NOT.
;;find something if it is like this
;;remove something if it isnt like this
;;dont remove something is it isnt like this, put it into a new list
;;put this thing into a list

(defun my-find-if (predicate list)
  (car (remove-if-not #'(lambda (x)
                           (funcall predicate x))
                       list)))

;;Show how to write EVERY given REMOVE-IF.
;;for every predicate true in the list return x
;;if predicate true is in the list remove it is if is not true

(defun my-every (predicate list)
  (remove-if #'(lambda (x)
                   (not (funcall predicate x)))
             (remove-if #'(lambda (x) (funcall predicate x)) list)))

(defun second-try (predicate list)
  (remove-if #'(lambda (x)
                 (not (funcall predicate x)))
             list))

(defun thrid-try (predicate list)
  (remove-if #'(lambda (x)
                 (unless (funcall predicate x)
                   nil))
             list))

;;if all of the elements are even the list length should stay the same!

(defun my-every (predicate list)
  (equalp (length list)
          (length (remove-if #'(lambda (x)
                                 (not (funcall predicate x)))
                             list))))

;;learning trace

(defun half (n) (* n 0.5))

(defun average (x y)
  (+ (half x) (half y)))

;;Avoid tracing the most fundamental
;;built-in functions such as EVAL, CONS, and +. Otherwise your Lisp might
;;end up in an infinite loop, and you will have to abandon it and start over

;;keyboard exercise

(defparameter *database*
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

#||
a. Write a function MATCH-ELEMENT that takes two symbols as
inputs. If the two are equal, or if the second is a question mark,
MATCH-ELEMENT should return T. Otherwise it should return
NIL. Thus (MATCH-ELEMENT ’RED ’RED) and (MATCH-
ELEMENT ’RED ’?) should return T, but (MATCH-ELEMENT
’RED ’BLUE) should return NIL. Make sure your function works
correctly before proceeding further.
||#

(defun match-element (one two)
  (or (equalp one two)
      (equalp '? two)))

#||
Write a function MATCH-TRIPLE that takes an assertion and a
pattern as input, and returns T if the assertion matches the pattern.
Both inputs will be three-element lists. (MATCH-TRIPLE ’(B2
COLOR RED) ’(B2 COLOR ?)) should return
T. (MATCH-TRIPLE ’(B2 COLOR RED) ’(B1 COLOR GREEN))
should return NIL. Hint: Use MATCH-ELEMENT as a building
block.
||#

(defun match-triple (assertion pattern)
  (and (match-element (first assertion) (first pattern))
       (match-element (second assertion) (second pattern))
       (match-element (third assertion) (third pattern))))
              

#||
. Write the function FETCH that takes a pattern as input and returns
all assertions in the database that match the pattern. Remember that DATABASE is a global variable. (FETCH ’(B2 COLOR ?))
should return ((B2 COLOR RED)), and (FETCH ’(? SUPPORTS
B1)) should return ((B2 SUPPORTS B1) (B3 SUPPORTS B1)).
||#
;;this is similar to sql

(defun fetch (pattern)
  (remove-if-not #'(lambda (x)
                     (match-triple x pattern))
                 *database*))


#||
Write a function that takes a block name as input and returns a
pattern asking the color of the block. For example, given the input
B3, your function should return the list (B3 COLOR ?)
||#

(defun query-color (name)
  (list name 'color '?))

#||
Write a function SUPPORTERS that takes one input, a block, and
returns a list of the blocks that support it. (SUPPORTERS ’B1)
should return the list (B2 B3). Your function should work by
constructing a pattern containing the block’s name, using that
pattern as input to FETCH, and then extracting the block names
from the resulting list of assertions.
||#
;;oh we're making egypt again
;;(b1 supported-by ?)

(defun query-supporter (block)
  (list block 'supported-by '?))

(defun supporters (block)
  (mapcar #'(lambda (x)
              (third x))
          (fetch (query-supporter block))))

#||
Write a predicate SUPP-CUBE that takes a block as input and
returns true if that block is supported by a cube. (SUPP-CUBE ’B4)
should return a true value; (SUPP-CUBE ’B1) should not because
B1 is supported by bricks but not cubes. Hint: Use the result of the
SUPPORTERS function as a starting point.
||#

(defun supp-cube (block)
  (if (equalp (last (assoc block *database*)) 'cube)
      (supporters block)
      nil))
#||
We are going to write a DESCRIPTION function that returns the
description of a block. (DESCRIPTION ’B2) will return (SHAPE
BRICK COLOR RED SIZE SMALL SUPPORTS B1 LEFT-OF
B3). 

We will do this in steps. First, write a function DESC1 that
takes a block as input and returns all assertions dealing with that
block. (DESC1 ’B6) should return ((B6 SHAPE BRICK) (B6
COLOR PURPLE) (B6 SIZE LARGE))
||#

(defun desc1 (block)
  (remove-if-not #'(lambda (x)
              (equalp (car x) block))
                 *database*))

;;first remove the first element in the nested list
;;then join all of them together with reduce

(defun description (block)
  (reduce #'append (mapcar #'rest (desc1 block))))
;;woops accidentaly solved now cant do the next questions t.t

;;k. What is the description of block B1? Of block B4?
;;CL-USER> (description 'b1)
;;(SHAPE BRICK COLOR GREEN SIZE SMALL SUPPORTED-BY B2 SUPPORTED-BY B3)

;;l. Block B1 is made of wood, but block B2 is made of plastic. How
;;would you add this information to the database?

;;type it /append it to the list

;;this is how you can assign everyone a job in a nice orderly fashion
#||
CL-USER> (mapcar #'(lambda (x y) (list x 'gets y))
'(fred wilma george diane)
'(job1 job2 job3 job4))
((FRED GETS JOB1) (WILMA GETS JOB2) (GEORGE GETS JOB3) (DIANE GETS JOB4))
||#

;;this is how you can add a number to a number in a orderly fashion
#||
CL-USER> (mapcar #'+ '(1 2 3 4 5) '(60 70 80 90 100))
(61 72 83 94 105)
||#

   
#||
Recall the English–French dictionary we stored in the global variable
WORDS earlier in the chapter. Given this dictionary plus the list or
corresponding Spanish words (UNO DOS TRES QUATRO CINCO), write an expression to return a trilingual dictionary. The first entry of the dictionary should be (ONE UN UNO).
||#

(defparameter *words*
  '((one un)
    (two deux)
    (three trois)
    (four quatre)
    (five cinq)))

(defun add-language (lang)
  (mapcar #'(lambda (x y)
              (append x (cons y ())))
          *words* lang))
