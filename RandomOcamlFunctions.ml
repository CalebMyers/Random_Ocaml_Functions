(*----------------------------------------------------------------
 |
 |  Program: Homework 6 -- Ocaml Functions
 |
 |  Class:   CS 3200 -- Professor Abukamail
 |
 |  Author:  Caleb Myers
 |  Email:   cm346613@ohio.edu
 |
 |  Description: This program contains several Ocaml functions and
 |               helper functions as asked on the homework 6
 |               document.
 |
 |  Date:        April 17, 2017
 |
 ---------------------------------------------------------------*)


(************ Question 1 -- Remove Last ************)

(*--------------------------------------------------
 |  Function: remove_last
 |
 |  Parameter(s): lst - a list
 |
 |  Purpose: Returns the list with the last element 
 |           removed. If the list is empty an empty 
 |           list is returned.
 |
 |  Calls: remove_last
 |
 ---------------------------------------------------*)
let rec remove_last lst =
	match lst with
	  [] -> []
	  | [x] -> []
	  | h::t -> [h]@remove_last t;;


(************ Question 2 -- Unique Element ************)

(*--------------------------------------------------
 |  Function: remove_duplicaes_of_el
 |
 |  Parameter(s): el  - an element
 |                lst - a list
 |                inList - a bool, if true then el is
 |                         in the new list, if false
 |                         it isn't.
 |
 |  Purpose: Returns the list with no duplicates of el.
 |
 |  Calls: remove_duplicaes_of_el
 |
 ---------------------------------------------------*)
let rec remove_duplicaes_of_el el lst inList =
	    match lst with
	       [] -> []
	       | h::t ->
	       if h = el && inList = true then
	         remove_duplicaes_of_el el t true
	       else if h = el && inList = false then
	         [h]@remove_duplicaes_of_el el t true
	       else 
	         [h]@remove_duplicaes_of_el el t false;;

(*--------------------------------------------------
 |  Function: unique_el
 |
 |  Parameter(s): el  - an element
 |                lst - a list
 |
 |  Purpose: Returns the list with no duplicates of el.
 |
 |  Calls: remove_duplicaes_of_el
 |
 ---------------------------------------------------*)
let unique_el el lst = remove_duplicaes_of_el el lst false;;


(************ Question 3 -- Find Multiples ************)

(*--------------------------------------------------
 |  Function: member
 |
 |  Parameter(s): el  - an element
 |                lst - a list
 |
 |  Purpose: Returns true if el is in lst, false if 
 |           not.
 |
 |  Calls: member
 |
 ---------------------------------------------------*)
let rec member lst el =
	match lst with
	  [] -> false
	| h::t -> if h = el then true else member t el;;


(*--------------------------------------------------
 |  Function: add_repeats_to_new_list
 |
 |  Parameter(s): lst - a list
 |                new_list - list to be returned
 |
 |  Purpose: Adds all elements in lst that are
 |           repeated to new_list and then returns
 |           new_list.
 |
 |  Calls: member
 |         add_repeats_to_new_list
 |
 ---------------------------------------------------*)
let rec add_repeats_to_new_list lst new_lsit =
	match lst with 
	  [] -> new_lsit
	| h::t -> if member t h && not (member new_lsit h) then 
	             add_repeats_to_new_list t (new_lsit@[h])
	          else
	             add_repeats_to_new_list t new_lsit;;

(*--------------------------------------------------
 |  Function: find_multiples
 |
 |  Parameter(s): lst - a list
 |
 |  Purpose: Returns a list of all elements that occur
 |           in list multiple times.
 |
 |  Calls: add_repeats_to_new_list
 |
 ---------------------------------------------------*)
let find_multiples lst = add_repeats_to_new_list lst [];;


(************ Question 4 -- Keep Two ************)

(*--------------------------------------------------
 |  Function: num_times_in_list
 |
 |  Parameter(s): lst - a list
 |                el - an element
 |
 |  Purpose: returns the number of times el appears
 |           in lst.
 |
 |  Calls: num_times_in_list
 |
 ---------------------------------------------------*)
let rec num_times_in_list lst el =
	match lst with 
	  [] -> 0
	| h::t -> if h = el then 
	            1 + num_times_in_list t el
	          else
	            num_times_in_list t el;;

(*--------------------------------------------------
 |  Function: add_n
 |
 |  Parameter(s): lst - a list
 |                new_list - list to be returned
 |                n - integer, max number of copies
 |                    to be in new_list            
 |
 |  Purpose: adds all elements in lst into new_list 
 |           a max of n times then returns new_list.
 |
 |  Calls: num_times_in_list
 |         add_n
 |
 ---------------------------------------------------*)
let rec add_n lst new_list n =
	match lst with
	  [] -> new_list
	| h::t -> if (num_times_in_list new_list h) < n then 
	             add_n t (new_list@[h]) n
	          else
	             add_n t new_list n;;

(*--------------------------------------------------
 |  Function: keep_two
 |
 |  Parameter(s): lst - a list        
 |
 |  Purpose: returns the list with a maximum of two 
 |           copies of each element
 |
 |  Calls: add_n
 |
 ---------------------------------------------------*)
let keep_two lst = add_n lst [] 2;;
