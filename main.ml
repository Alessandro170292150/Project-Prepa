type m = int array array

let product (m1 : m) (m2 : m) : m =
  let z = ref 0 in
    let c = Array.make_matrix (Array.length m1) (Array.length m2) 0 in
    for i = 0 to (Array.length m1) - 1 do
      for j = 0 to (Array.length m2) - 1 do
        for k = 0 to (Array.length m2) - 1 do
          z := !z + (m1.(i).(k))*(m2.(k).(j))
        done;
        c.(i).(j) <- !z;
        z := 0
      done;
    done;
  c

let add (m1 : m) (m2 : m) : m = 
  let c = Array.make_matrix (Array.length m1) (Array.length m2) 0 in
  let n = Array.length m1 in 
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      c.(i).(j) <- m1.(i).(j) + m2.(i).(j)
    done;
  done;
  c

let sous (m1 : m) (m2 : m) : m = 
  let c = Array.make_matrix (Array.length m1) (Array.length m2) 0 in
  let n = Array.length m1 in 
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      c.(i).(j) <- m1.(i).(j) - m2.(i).(j)
    done;
  done;
  c



let rec strassen (a : m) (b : m) : m  = 
  match (Array.length a) with
  |1 -> product a b
  |_ ->  let n = Array.length a in
  let c = Array.make_matrix (Array.length a) (Array.length b) 0 in
  let a1 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let a2 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let a3 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let a4 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let b2 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let b1 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let b3 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let b4 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let c1 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let c2 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let c3 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let c4 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d1 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d2 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d3 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d4 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d5 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d6 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  let d7 = Array.make_matrix ((Array.length a)/2) ((Array.length a)/2) 0 in
  for i = 0 to (((Array.length a)/2) - 1) do
    for j = 0 to (((Array.length a)/2) - 1) do 
      a1.(i).(j) <- a.(i).(j);
      a2.(i).(j) <- a.(i+ (Array.length b)/2).(j);
      a3.(i).(j) <- a.(i).(j + (Array.length b)/2);
      a4.(i).(j) <- a.(i+ (Array.length b)/2).(j + (Array.length b)/2);
      b1.(i).(j) <- b.(i).(j);
      b2.(i).(j) <- b.(i + (Array.length b)/2).(j);
      b3.(i).(j) <- b.(i).(j + (Array.length b)/2);
      b4.(i).(j) <- b.(i + (Array.length b)/2).(j + (Array.length b)/2)
    done;
  done;
  for i = 0 to (n/2 - 1) do
    for j = 0 to (n/2 - 1) do 
      d1.(i).(j) <- (strassen (add a1 a4) (add b1 b4)).(i).(j);
      d2.(i).(j) <- (strassen (add a2 a4) b1).(i).(j);
      d3.(i).(j) <- (strassen a1 (sous b3 b4)).(i).(j);
      d4.(i).(j) <- (strassen a4 (sous b2 b1)).(i).(j);
      d5.(i).(j) <- (strassen (add a1 a3) b4).(i).(j);
      d6.(i).(j) <- (strassen  (sous a2 a1) (add b1 b3)).(i).(j);
      d7.(i).(j) <- (strassen (sous a3 a4) (add b2 b4)).(i).(j);
      c1.(i).(j) <- (sous (add (add d1 d4) d7) d5).(i).(j);
      c2.(i).(j) <- (add d3 d5).(i).(j);
      c3.(i).(j) <- (add d2 d4).(i).(j);
      c4.(i).(j) <- (sous (add (add d1 d3) d6) d2).(i).(j);
      c.(i).(j) <- c1.(i).(j);
      c.(n/2 + i).(j) <- c3.(i).(j);
      c.(i).(n/2 + j) <- c2.(i).(j);
      c.(n/2 + i).(n/2 + j) <- c4.(i).(j)
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

let print_m_s (a : m) : unit = 
  for i = 0 to (Array.length a) - 1 do
    for j = 0 to (Array.length a.(0)) - 1 do
      Printf.printf " %d "  (a.(i).(j))
    done;
    Printf.printf "\n"
  done

let () = 
  let m1 = [|[|6;1;3; 5|]; [|3; 1; 0; 3|]; [|7; 0; 1; 5|]; [|2; 1; 5; 9|]|] in
  let m2 = [|[|1; 3; 2; 3|];[|1; 0; 9; 8|]; [|0; 1; 2; 6|]; [|2; 1; 4; 5|]|] in
  let c1 = strassen m1 m2 in
  let c2 = product m1 m2 in
  print_m c2;
  Printf.printf "\n";
  print_m c1
  (* let m3 = Array.make_matrix 4 4 1 in
  let m4 = Array.make_matrix 4 4 1 in
  let c3 = product m3 m4 in
  let c2 = strassen m3 m4 in 
  print_m_s c3;
  print_m_s c2 *)
  


  



  

      


