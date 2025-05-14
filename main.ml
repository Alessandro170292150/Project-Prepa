type m = int array array


module Mat = struct
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

    let product (m1 : m) (m2 : m) : m =
      let z = ref 0 in
      let c = Array.make_matrix (Array.length m1) (Array.length m1) 0 in
        for i = 0 to (Array.length m1) - 1 do
          for j = 0 to (Array.length m1) - 1 do
            for k = 0 to (Array.length m1) - 1 do
              z := !z + (m1.(i).(k))*(m2.(k).(j))
            done;
            c.(i).(j) <- !z;
            z := 0
          done;
        done;
      c
    let abs (a : int) (b : int) = 
      if a < b then b - a else a - b
    let max (mat : m) : int = 
      let p = Array.length mat.(0) in
      let man = ref mat.(0).(0) in
      for i = 0 to (p - 1) do 
        for j = 0 to (p - 1) do
          if !man < mat.(i).(j) then
            man := mat.(i).(j)
          done;
        done;
      !man
    let rando_m m1 =
      let n = Array.length m1.(0) in 
      let a = Array.make_matrix n n 0 in 
      for i = 0 to n-1 do 
        for j = 0 to n-1 do
          a.(i).(j) <- Random.int (max m1)
        done;
      done;
    a

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
    end

let rec strassen (a : m) (b : m) : m =
  let n = Array.length a in
  if n < 16 then Mat.product a b
  else
    let n2 = n / 2 in

    let submatrix m row col =
      Array.init n2 (fun i -> Array.init n2 (fun j -> m.(i + row).(j + col)))
    in

    let a11 = submatrix a 0 0
    and a12 = submatrix a 0 n2
    and a21 = submatrix a n2 0
    and a22 = submatrix a n2 n2
    and b11 = submatrix b 0 0
    and b12 = submatrix b 0 n2
    and b21 = submatrix b n2 0
    and b22 = submatrix b n2 n2 in

    (* Calcul des 7 produits de Strassen *)
    let m1 = strassen (Mat.add a11 a22) (Mat.add b11 b22) in
    let m2 = strassen (Mat.add a21 a22) b11 in
    let m3 = strassen a11 (Mat.sous b12 b22) in
    let m4 = strassen a22 (Mat.sous b21 b11) in
    let m5 = strassen (Mat.add a11 a12) b22 in
    let m6 = strassen (Mat.sous a21 a11) (Mat.add b11 b12) in
    let m7 = strassen (Mat.sous a12 a22) (Mat.add b21 b22) in

    (* Calcul des sous-blocs du résultat final *)
    let c11 = Mat.sous (Mat.add (Mat.add m1 m4) m7) m5 in
    let c12 = Mat.add m3 m5 in
    let c21 = Mat.add m2 m4 in
    let c22 = Mat.sous (Mat.add (Mat.add m1 m3) m6) m2 in

    (* Assembler le résultat final *)
    let c = Array.make_matrix n n 0 in
    for i = 0 to n2 - 1 do
      for j = 0 to n2 - 1 do
        c.(i).(j) <- c11.(i).(j);
        c.(i).(j + n2) <- c12.(i).(j);
        c.(i + n2).(j) <- c21.(i).(j);
        c.(i + n2).(j + n2) <- c22.(i).(j)
      done
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
  let n = 512 in
  (* let m1 = [|[|6;1;3; 5|]; [|3; 1; 0; 3|]; [|7; 0; 1; 5|]; [|2; 1; 5; 9|]|] in
  let m2 = [|[|1; 3; 2; 3|];[|1; 0; 9; 8|]; [|0; 1; 2; 6|]; [|2; 1; 4; 5|]|] in *)
  let m3 = Array.init n (fun i -> Array.init n (fun j -> if i < j+1 then 1 else 0)) in
  let m4 = Array.init n (fun i -> Array.init n (fun j -> if i > j+1 then 1 else 0)) in 
  let t1 = Sys.time() in
  let c1 = strassen m3 m4 in
  let t2 = Sys.time() in
  let c2 = Mat.product m3 m4 in
  let t3 = Sys.time() in
  Printf.printf "strassen prend %.3f \n" (t2 -. t1);
  Printf.printf "et le produit prend %.3f" (t3 -. t2);
  (* print_m c2;
  Printf.printf "\n";
  print_m c1; *)



  



  

      


