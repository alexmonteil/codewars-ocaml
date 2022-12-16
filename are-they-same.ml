open List;;
open Hashtbl;;


let comp a b = 

  let alength = List.length a in
  let blength = List.length b in
  if alength <> blength 
    then false
  else
    let ahash = (Hashtbl.create 50) in
    let bhash = (Hashtbl.create 50) in

    let rec addingToHashtable list tbl = 
      match list with 
      |[] -> ()
      |x :: y -> 
        let value = Hashtbl.find_opt tbl x in
        (match value with
        |None -> Hashtbl.add tbl x 1
        |Some v -> Hashtbl.replace tbl x (v + 1));
        addingToHashtable y tbl in
        

    addingToHashtable a ahash;
    addingToHashtable b bhash;
    let isResultTrue = ref (true) in 
    Hashtbl.iter 
      (fun k v -> 
        match Hashtbl.find_opt bhash k with
        |Some v' -> if v <> v' then isResultTrue := false
        |None -> isResultTrue := false) 
      ahash;

    !isResultTrue in


let lista = [1;2;2;3] in
let listb = [1;2;3;3] in
comp lista listb;;

  


    


  