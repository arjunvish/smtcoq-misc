(*A symbol can be t1.t2.t3, it is converted to [1;2;3] *)
let symbol_to_id s = 
    let f = (fun s -> let l = (String.length s) - 1 in
                      int_of_string (String.sub s 1 l)) in
    List.map f (String.split_on_char '.' s)
    
let id_to_string id = 
    List.fold_left (fun x y -> x^"."^y) "" (List.map string_of_int id)

let is_first_id id = List.for_all (fun x -> x == 1) id
let lst l = List.hd (List.rev l)
let remove_lst l = List.rev (List.tl (List.rev l))

let rec prev_id id =
  if lst id == 1 then 
    prev_id (remove_lst id)
  else
    (remove_lst id)@[(lst id)-1]
  
let get_prev_id id = 
  if (is_first_id id) then id 
  else prev_id id
