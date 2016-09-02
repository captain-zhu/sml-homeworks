fun repeat (fl : int list, sl : int list) =
  if null fl
  then
      []
  else
      if (hd sl) = 0
      then
	  repeat(tl fl, tl sl)
      else
	  (hd fl)::repeat(fl, ((hd sl) - 1)::(tl sl))

fun addAllOpt (options : int option list) =
  if null options
  then
      NONE
  else
      if isSome (hd options)
      then
	  if isSome (addAllOpt (tl options))
	  then
	      SOME ((valOf (hd options)) + (valOf (addAllOpt (tl options))))
	  else
	      SOME (valOf (hd options))
      else
	  addAllOpt(tl options)

fun any (bools : bool list) =
  if null bools
  then
      false
  else
      if hd bools
      then
	  true
      else
	  any (tl bools)

fun all (bools : bool list) =
  if null bools
  then
      true
  else
      if hd bools
      then
	  all (tl bools)
      else
	  false

fun zip (fl : int list, sl : int list) =
  if null fl
  then
      []
  else
      if null sl
      then
	  []
      else
	  (hd fl, hd sl)::zip(tl fl, tl sl)


fun cycle (xl : int list, le : int) =
  let
      val xle = length xl
  in
      if (length xl) >= le
      then
	  xl@List.take(xl, le)
      else
	  cycle(xl@xl, le - (length xl))
  end

fun zipRecycle (fl : int list, sl : int list) =
  let
      val length_fl = length fl
      val length_sl = length sl
  in
      if length_fl >= length_sl
      then
	  zip(fl, cycle(sl, length_fl - length_sl))
      else
	  zip(cycle(fl, length_sl - length_fl), sl)
  end

fun lookup (pl : (string * int) list, to_look_up : string) =
  if null pl
  then
      NONE
  else
      if (#1 (hd pl)) = to_look_up
      then
	  SOME (#2 (hd pl))
      else
	  lookup(tl pl, to_look_up)

fun splitup (nums : int list) =
  if null nums
  then
      ([], [])
  else
      let
	  val hd_num = hd nums
	  val tl_nums = splitup(tl nums)
      in
	  if hd_num <= 0
	  then
	      (hd_num::(#1 tl_nums), #2 tl_nums)
	  else
	      (#1 tl_nums, hd_num::(#2 tl_nums))
      end

fun splitAt (nums : int list, position : int) =
  if null nums
  then
      ([], [])
  else
      let
	  val hd_num = hd nums
	  val tl_nums = splitAt(tl nums, position)
      in
	  if hd_num <= position
	  then
	      (hd_num::(#1 tl_nums), #2 tl_nums)
	  else
	      (#1 tl_nums, hd_num::(#2 tl_nums))
      end
	  
fun isSorted (nums : int list) =
  if length nums = 1
  then
      true
  else
      if hd nums >= (hd (tl nums))
      then
	  false
      else
	  isSorted(tl nums)

fun isAnySorted( nums : int list) =
  isSorted(nums) orelse isSorted(rev nums)
		       
fun sortedMerge (nums: (int list) * (int list)) =
  let
      val fl = #1 nums
      val sl = #2 nums
  in
      if null fl
      then
	  sl
      else
	  if null sl
	  then
	      fl
	  else
	      let
		  val fl_item = hd fl
		  val sl_item = hd sl
	      in
		  if fl_item <= sl_item
		  then
		      fl_item::sortedMerge(tl fl, sl)
		  else
		      sl_item::sortedMerge(fl, tl sl)
	      end
  end

fun qSort(nums : int list) =
  if null nums
  then
      []
  else
      let
	  val threshold = hd nums
	  val tl_nums = tl nums
      in
	  if null tl_nums
	  then
	      [threshold]
	  else
	      if length tl_nums = 1
	      then
		  if (threshold < hd tl_nums)
		  then
		      [threshold, hd tl_nums]
		  else
		      [hd tl_nums, threshold]
	      else
		  let
		      val l_pairs = splitAt(tl_nums, threshold)
		      val fl = (#1 l_pairs)@[threshold]
		      val sl = #2 l_pairs
		  in
		      sortedMerge((qSort fl,qSort sl))
		  end
      end


fun generate(nums : int list) =
  if null nums
  then
      []
  else
      let
	  val head = hd nums
	  val rest = tl nums
      in
	  if null rest
	  then
	      [head]
	  else
	      head::generate(tl rest)
      end
				 

fun divide (nums : int list) =
  if null nums
  then
      ([],[])
  else
      (generate(nums), generate(tl nums))

fun not_so_quick_sort(nums : int list) =
  if null nums
  then
      []
  else
      if null (tl nums)
      then
	  [hd nums]
      else
	  let
	      val rest = tl nums
	  in
	      if null (tl rest)
	      then
		  if hd nums <=  hd rest
		  then
		      [hd nums, hd rest]
		  else
		      [hd rest, hd nums]
	      else
		  let
		      val divide_pairs = divide(nums)
		      val fl = #1 divide_pairs
		      val sl = #2 divide_pairs
		  in
		      sortedMerge(not_so_quick_sort(fl),not_so_quick_sort(sl))
		  end
	  end

fun fullDivide(fp : int * int) =
  let
      val fi = #1 fp
      val si = #2 fp
  in
      if (si mod fi) <> 0
      then
	  (0, si)
      else
	  let
	      val result = fullDivide (fi, si div fi)
	  in
	      (1 + (#1 result), #2 result)
	  end
  end


  
	  
