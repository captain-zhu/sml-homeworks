(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun only_capitals xs = List.filter (fn (x) => Char.isUpper (String.sub(x,0))) xs

fun longest_string1 xs = List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs = List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f =
  List.foldl f "" 

val longest_string3 = longest_string_helper (fn (x, y) => if String.size x > String.size y then x else y)

val longest_string4 = longest_string_helper (fn (x, y) => if String.size x >= String.size y then x else y)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case (f x) of
		    NONE => first_answer f xs'
		  | SOME v => v

fun all_answers f xs =
  let
      fun aux (ys, acc) =
	case ys of
	    [] => SOME acc
	  | y::ys' => case f(y) of
			 NONE => NONE
		       | SOME v => aux(ys', acc@v)
  in
      aux(xs, [])
  end
 
val count_wildcards = g (fn () => 1) (fn x => 0)
  

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x) 
		  
fun count_some_var (x, y) =
  g (fn () => 0) (fn a => if a = x then 1 else 0) y

val check_pat =
    let
	fun check_pat_helper1 x =
	  case x of
	      Variable x => [x]
	    | TupleP ps => List.foldl (fn (p, i) => i @ (check_pat_helper1 p)) [] ps
	    | ConstructorP(_, p) => check_pat_helper1 p
	    |  _  => []
	fun check_pat_helper2 xs =
	  case xs of
	      [] => true
	    | x::xs' => if List.exists (fn y => x = y) xs'
			then
			    false
			else
			    check_pat_helper2 xs'
    in
	check_pat_helper2 o check_pat_helper1
    end

fun match (va, pat) =
    case (va, pat) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                     then match(v, p)
                                                     else NONE
      | (_, _) => NONE

fun first_match va plst =
    SOME (first_answer (fn x => match(va, x)) plst) handle NoAnswer => NONE
	
      
  
	 
					    
(**** you can put all your code here ****)
