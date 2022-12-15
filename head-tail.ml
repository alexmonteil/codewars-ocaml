open Printf;;

let head list =
  match list with
  |first :: _ -> first
  |_ -> failwith "Empty";;

let tail list = 
  match list with 
  |_ :: rest -> rest
  |[] -> [];;


let rec init list = 
  match list with
  |[first] -> []
  |first :: rest -> first :: init rest
  |[] -> [];;

let rec last list =
  match list with
  |last :: [] -> last 
  |first :: rest -> last rest
  |_ -> failwith "Empty";;


let nums = [1;2;3;4];;
let allbut = init nums;;

let printThings f l =
  let rec printingThings l =
    match l with
      [] -> () |
      h :: t -> printf " ; " ; printf f h ; printingThings t
  in printf "[" ;
     (match l with
        [] -> () |
        h :: t -> printf f h ; printingThings t) ;
     printf "]\n" ;;


printf "%i\n" (head nums);;
printThings "%i" (tail nums);;
printThings "%i" (init nums);;
printf "%i\n" (last nums);;