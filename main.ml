type m = int array array

type m2 = int list array

(* let product (m1 : m) (m2 : m) = 
let c = Array.make_matrix (Array.length m1) (Array.length m2) 0 in
for i = 0 to (Array.length m1) - 1 do
for j = 0 to (Array.length m2) - 1 do *)


let strassen (a : m) (b : m) : m  = 
  let c = Array.make_matrix (Array.length a) (Array.length b ) 0 in
  let a1 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let a2 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let a3 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let a4 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let b1 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let b2 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let b3 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let b4 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let c1 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let c2 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let c3 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let c4 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let d1 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2)  0 in
  let d2 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d3 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d4 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d5 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d6 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d7 =  Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  for i = 0 to (Array.length a)/2 - 1 do
    for j = 0 to (Array.length a)/2 - 1 do 
      a1.(i).(j) <- a.(i).(j);
      a2.(i).(j) <- a.(i+ (Array.length b)/2).(j);
      a3.(i).(j) <- a.(i).(j + (Array.length b)/2);
      a4.(i).(j) <- a.(i+ (Array.length b)/2).(j + (Array.length b)/2);
      b1.(i).(j) <- b.(i).(j);
      b2.(i).(j) <- b.(i + (Array.length b)/2).(j);
      b3.(i).(j) <- b.(i).(j + (Array.length b)/2);
      b4.(i).(j) <- b.(i + (Array.length b)/2).(j + (Array.length b)/2);
      d1.(i).(j) <- (a1.(i).(j) + a4.(i).(j))*(b1.(i).(j) + b4.(i).(j));
      d2.(i).(j) <- (a2.(i).(j) + a4.(i).(j))*(b1.(i).(j));
      d3.(i).(j) <- (a1.(i).(j)) * (b3.(i).(j) - b4.(i).(j));
      d4.(i).(j) <- (a4.(i).(j)) * (b3.(i).(j) - b1.(i).(j));
      d5.(i).(j) <- (a1.(i).(j) + a3.(i).(j)) * b4.(i).(j); 
      d6.(i).(j) <- (a2.(i).(j) - a1.(i).(j)) * (b1.(i).(j));
      d7.(i).(j) <- (a3.(i).(j) - a4.(i).(j)) * (b2.(i).(j) + b4.(i).(j));
      c1.(i).(j) <- d1.(i).(j) + d4.(i).(j) - d5.(i).(j) + d7.(i).(j);
      c2.(i).(j) <- d3.(i).(j) + d5.(i).(j);
      c3.(i).(j) <- d2.(i).(j) + d4.(i).(j);
      c4.(i).(j) <- d1.(i).(j) - d2.(i).(j) + d3.(i).(j) + d6.(i).(j);
      c.(i).(j) <- c1.(i).(j);
      c.(i + (Array.length b)/2).(j + (Array.length b)/2) <- c4.(i).(j);
      c.(i + (Array.length b)/2).(j) <- c2.(i).(j);
      c.(i).(j + (Array.length b)/2) <- c3.(i).(j)
    done;
  done;
  c

let print_m (a : m) : unit = 
  for i = 0 to (Array.length a) - 1 do
    for j = 0 to (Array.length a.(0)) - 1 do
      Printf.printf " a.%d.%d = %d " i j (a.(i).(j))
    done;
    Printf.printf "\n"
  done

let () = 
  let m1 = [|[|1; 0|]; [|0; 1|]|] in
  let m2 = [|[|1; 0|]; [|0; 1|]|] in
  let m3 = strassen m1 m2 in
  print_m m3
  



  

      


