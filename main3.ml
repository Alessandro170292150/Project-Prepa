open Bigarray
open Domain

type m = (float, float64_elt, c_layout) Array2.t

let create_matrix n : m =
  Array2.create float64 C_layout n n

let init_matrix n f : m =
  let m = create_matrix n in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Array2.set m i j (f i j)
    done
  done;
  m

let add (a : m) (b : m) : m =
  let n = Array2.dim1 a in
  let c = create_matrix n in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.{i, j} <- a.{i, j} +. b.{i, j}
    done
  done;
  c

let sous (a : m) (b : m) : m =
  let n = Array2.dim1 a in
  let c = create_matrix n in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.{i, j} <- a.{i, j} -. b.{i, j}
    done
  done;
  c

let product (a : m) (b : m) : m =
  let n = Array2.dim1 a in
  let c = create_matrix n in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let sum = ref 0. in
      for k = 0 to n - 1 do
        sum := !sum +. a.{i, k} *. b.{k, j}
      done;
      c.{i, j} <- !sum
    done
  done;
  c

let pow (n : int) (k: int) = 
      let res = ref 1 in
      let i = ref k in
      let m = ref n in
      while (!i > 0) do
        if !i mod 2 = 1 then res:= (!res) * (!m); 
        m := (!m) * (!m);
        i := ((!i)/2)
      done;
      !res

let submatrix (a : m) row col size : m =
  let m = create_matrix size in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      m.{i, j} <- a.{i + row, j + col}
    done
  done;
  m

let combine c11 c12 c21 c22 : m =
  let n2 = Array2.dim1 c11 in
  let n = n2 * 2 in
  let c = create_matrix n in
  for i = 0 to n2 - 1 do
    for j = 0 to n2 - 1 do
      c.{i, j} <- c11.{i, j};
      c.{i, j + n2} <- c12.{i, j};
      c.{i + n2, j} <- c21.{i, j};
      c.{i + n2, j + n2} <- c22.{i, j}
    done
  done;
  c


let rec strassen ?(depth=0) (a : m) (b : m) : m =
  let n = Array2.dim1 a in
  if n <= 32 then product a b
  else
    let n2 = n / 2 in
    let a11 = submatrix a 0 0 n2
    and a12 = submatrix a 0 n2 n2
    and a21 = submatrix a n2 0 n2
    and a22 = submatrix a n2 n2 n2
    and b11 = submatrix b 0 0 n2
    and b12 = submatrix b 0 n2 n2
    and b21 = submatrix b n2 0 n2
    and b22 = submatrix b n2 n2 n2 in

    if depth = 0 then
      let t1 = spawn (fun () -> strassen ~depth:1 (add a11 a22) (add b11 b22)) in
      let t2 = spawn (fun () -> strassen ~depth:1 (add a21 a22) b11) in
      let t3 = spawn (fun () -> strassen ~depth:1 a11 (sous b12 b22)) in
      let t4 = spawn (fun () -> strassen ~depth:1 a22 (sous b21 b11)) in
      let t5 = spawn (fun () -> strassen ~depth:1 (add a11 a12) b22) in
      let t6 = spawn (fun () -> strassen ~depth:1 (sous a21 a11) (add b11 b12)) in
      let t7 = spawn (fun () -> strassen ~depth:1 (sous a12 a22) (add b21 b22)) in

      let m1 = join t1
      and m2 = join t2
      and m3 = join t3
      and m4 = join t4
      and m5 = join t5
      and m6 = join t6
      and m7 = join t7 in

      let c11 = sous (add (add m1 m4) m7) m5 in
      let c12 = add m3 m5 in
      let c21 = add m2 m4 in
      let c22 = sous (add (add m1 m3) m6) m2 in
      combine c11 c12 c21 c22

    else
      let m1 = strassen ~depth:(depth+1) (add a11 a22) (add b11 b22) in
      let m2 = strassen ~depth:(depth+1) (add a21 a22) b11 in
      let m3 = strassen ~depth:(depth+1) a11 (sous b12 b22) in
      let m4 = strassen ~depth:(depth+1) a22 (sous b21 b11) in
      let m5 = strassen ~depth:(depth+1) (add a11 a12) b22 in
      let m6 = strassen ~depth:(depth+1) (sous a21 a11) (add b11 b12) in
      let m7 = strassen ~depth:(depth+1) (sous a12 a22) (add b21 b22) in

      let c11 = sous (add (add m1 m4) m7) m5 in
      let c12 = add m3 m5 in
      let c21 = add m2 m4 in
      let c22 = sous (add (add m1 m3) m6) m2 in
      combine c11 c12 c21 c22



let print_m (a : m) : unit =
  let n = Array2.dim1 a in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%.0f " a.{i, j}
    done;
    print_newline ()
  done

let () =
  let i = 12 in
  let n = pow 2 i in
  for k = 0 to i do 
    let m1 = init_matrix (pow 2 k) (fun i j -> if i < j + 1 then 1. else 0.) in
    let m2 = init_matrix  (pow 2 k) (fun i j -> if i > j + 1 then 1. else 0.) in
    let t1 = Sys.time () in
    let c1 = strassen m1 m2 in
    let t2 = Sys.time () in
    let c2 = product m1 m2 in
    let t3 = Sys.time () in
    Printf.printf "Strassen time : %.3fs\n" (t2 -. t1);
    Printf.printf "Naive time    : %.3fs\n" (t3 -. t2)
  done


