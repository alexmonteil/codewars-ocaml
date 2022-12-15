open List;;
open Hashtbl;;

let lista = [1;2;2;3];;
let listb = [1;2;2;3];;

let comp a b = 

  let alength = length a in
  let blength = length b in
  if alength <> blength 
    then false
  else
    let ahash = (Hashtbl.create 50) in
    let bhash = (Hashtbl.create 50) in

    let rec addingToHashtable list tbl = 
      match list with 
      |[] -> ()
      |x :: y -> 
        (let value = Hashtbl.find_opt tbl x in
        match value with
        |None -> Hashtbl.add tbl x 1
        |Some v -> Hashtbl.replace tbl x (v + 1));
        addingToHashtable y tbl in
        

    addingToHashtable a ahash;
    addingToHashtable b bhash;
    Hashtbl.iter 
      (fun k v -> 
        match Hashtbl.find_opt bhash k with
        |Some v' -> if v <> v' then false
        |None -> false) 
      ahash;;
  

comp lista listb;;

  


    


  