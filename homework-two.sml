(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x, y) =
  case y of
      [] => NONE
    | y'::ys' => if same_string(x, y')
		 then
		     SOME ys'
		 else
		     case all_except_option(x,ys')of
			 NONE => NONE
		       | SOME somey => SOME (y'::somey)

fun get_substitutions1(x, y) =
  case x of
      [] => []
    | x'::xs' => case all_except_option(y,x') of
		     NONE => []@get_substitutions1(xs', y)
		   | SOME v => v@get_substitutions1(xs', y)

fun get_substitutions2(x,y) =
  let
      fun aux(a, b, acc) =
	case a of
	    [] => acc
	  | a'::as' => case all_except_option(b,a') of
			   NONE => aux(as', b, acc)
			 | SOME v => aux(as', b, acc@v)
  in
      aux(x, y, [])
  end

fun similar_names (x, {first=y, middle=z, last=t}) =
  let
      fun get_name(a) =
	{first=a, last=z, middle =t}
      fun aux (d, acc) =
	case d of
	    [] => acc
	  | d'::ds' => aux(ds', acc@[get_name(d')])
  in
      aux(get_substitutions2(x, y), [get_name(y)])
  end



	
  
  

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, rank) =
  case suit of
      Spades => Black
    | Clubs => Black
    |  _  => Red

fun card_value (suit, rank) =
  case rank of
      Num n => n
    | Ace  => 11
    | _  => 10

fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | c'::cs' => if c = c'
		 then
		     cs'
		 else
		     c'::remove_card(cs', c, e)

fun all_same_color cs =
  case cs of
      [] => true
    | c::[] => true
    | c::(c'::cs') => (card_color c = card_color c' andalso all_same_color (c'::cs'))

fun sum_cards cs =
  let
      fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', acc + (card_value x))
  in
      aux(cs, 0)
  end

fun score (cs, goal) =
  let
      val sum = sum_cards cs
      val all_same = all_same_color cs
      val diff = sum - goal
      val abs_diff = abs diff
      val compute_divide_num = fn x =>
			  if x
			  then
			      2
			  else
			      1
      val divide_num = compute_divide_num all_same
  in
      if diff > 0
      then
	  (3 * diff) div divide_num
      else
	  abs_diff div divide_num
  end

fun officiate (cs, ms, goal) =
  
