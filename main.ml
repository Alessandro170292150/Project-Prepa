type m = int array array

type m2 = int list array

let strassen (a : m) (b: m) : m  = 
  let c = Array.make_matrix (Array.length a) (Array.length b ) 0 in
  let a1 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let a2 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let a3 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let a4 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let b1 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let b2 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let b3 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let b4 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let c1 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let c2 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let c3 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  let c4 = Array.make_matrix (Array.make_matrix (Array.length a)/2 (Array.length a)/2 ) 0 in
  for i = 0 to (Array.length a)/2 do
    for j = 0 to (Array.length a)/2 do 
      a1.(i).(j) <- a.(i).(j);
      a2.(i).(j) <- a.(i+ (Array.length b)/2).(j);
      a3.(i).(j) <- a.(i).(j + (Array.length b)/2);
      a4.(i).(j) <- a.(i+ (Array.length b)/2).(j + (Array.length b)/2);
      b1.(i).(j) <- b.(i).(j);
      b2.(i).(j) <- b.(i + (Array.length b)/2).(j);
      b3.(i).(j) <- b.(i).(j + (Array.length b)/2);
      b4.(i).(j) <- b.(i + (Array.length b)/2).(j + (Array.length b)/2);
      c1.(i).(j) <- c.(i).(j);
      c2.(i).(j) <- c.(i + (Array.length b)/2).(j);
      c3.(i).(j) <- c.(i).(j + (Array.length b)/2);
      c4.(i).(j) <- c.(i + (Array.length b)/2).(j + (Array.length b)/2);
    done;
  done;


      


