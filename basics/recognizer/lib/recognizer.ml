let count x l = List.fold_left(fun acc y -> if x=y then acc+1 else acc) 0 l;;

let rec lang1 l = 
  match l with
  | [] -> false
  | '0'::[]
  | '1'::[] -> true
  | '0'::l'
  | '1'::l' -> lang1 l'
  | _ -> false;;

let rec lang2_a l =
  match l with
  | [] -> true
  | '1'::l' -> lang2_a l'
  | _ -> false;;

let lang2 l = 
  match l with
  | [] -> true
  | '0':: l' -> lang2_a l'
  | _ -> false;;

(*Linguaggio che riconosce le parole del tipo [01]*)
let rec lang3_a l =
  match l with
  | [] -> true
  | '0'::l'
  | '1'::l' -> lang3_a l'
  | _ -> false;;

let lang3 l = 
  let length = List.length l in
  let last = length -1 in
  length>=2  && List.nth l 0 = '0' && List.nth l last ='0' && lang3_a l;;

let lang4 l =
  count '1' l = 2 &&  lang3_a l;;

let rec lang5 l = 
  match l with
  | [] -> true
  | '0'::'0'::l' -> lang5 l'
  | '1'::'1'::l' -> lang5 l'
  | _ -> false;;

    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
