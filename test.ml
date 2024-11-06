(* Fonction pour ajouter deux matrices *)
let add_matrix a b =
  Array.init (Array.length a) (fun i ->
    Array.init (Array.length a.(0)) (fun j ->
      a.(i).(j) + b.(i).(j)))

(* Fonction pour soustraire deux matrices *)
let sub_matrix a b =
  Array.init (Array.length a) (fun i ->
    Array.init (Array.length a.(0)) (fun j ->
      a.(i).(j) - b.(i).(j)))



(* Fonction pour diviser une matrice en quatre sous-matrices *)
let divide_matrix m =
  let n = Array.length m / 2 in
  let a = Array.init n (fun i -> Array.init n (fun j -> m.(i).(j))) in
  let b = Array.init n (fun i -> Array.init n (fun j -> m.(i).(j + n))) in
  let c = Array.init n (fun i -> Array.init n (fun j -> m.(i + n).(j))) in
  let d = Array.init n (fun i -> Array.init n (fun j -> m.(i + n).(j + n))) in
  (a, b, c, d)

(* Fonction principale de multiplication de matrices selon Strassen *)
let rec strassen a b =
  let n = Array.length a in
  if n = 1 then
    Array.make 1 (a.(0).(0) * b.(0).(0))
  else
    let a11, a12, a21, a22 = divide_matrix a in
    let b11, b12, b21, b22 = divide_matrix b in

    let m1 = strassen a11 (sub_matrix b12 b22) in
    let m2 = strassen (add_matrix a11 a12) b22 in
    let m3 = strassen (add_matrix a21 a22) b11 in
    let m4 = strassen a22 (sub_matrix b21 b11) in
    let m5 = strassen (add_matrix a11 a22) (add_matrix b11 b22) in
    let m6 = strassen (sub_matrix a12 a22) (add_matrix b21 b22) in
    let m7 = strassen (sub_matrix a11 a21) (add_matrix b11 b12) in

    let c11 = add_matrix (sub_matrix (add_matrix m5 m4) m2) m6 in
    let c12 = add_matrix m1 m2 in
    let c21 = add_matrix m3 m4 in
    let c22 = sub_matrix (sub_matrix (add_matrix m5 m1) m3) m7 in

    (* Combiner les sous-matrices pour former la matrice résultante *)
    Array.init n (fun i ->
      Array.init n (fun j ->
        if i < n / 2 then
          if j < n / 2 then c11.(i).(j)
          else c12.(i).(j - n / 2)
        else
          if j < n / 2 then c21.(i - n / 2).(j)
          else c22.(i - n / 2).(j - n / 2)))




let print_m (a : int array array) : unit = 
  for i = 0 to (Array.length a) - 1 do
    for j = 0 to (Array.length a.(0)) - 1 do
      Printf.printf " a.%d.%d = %d " i j (a.(i).(j))
    done;
    Printf.printf "\n"
  done

let print_m_s (a : int array array) : unit = 
  for i = 0 to (Array.length a) - 1 do
    for j = 0 to (Array.length a.(0)) - 1 do
      Printf.printf " %d "  (a.(i).(j))
    done;
    Printf.printf "\n"
  done

(* Exemple d'utilisation *)
let () =
  let a = [| [| 1; 2 |]; [| 3; 4 |] |] in
  let b = [| [| 5; 6 |]; [| 7; 8 |] |] in
  let result = strassen a b in
  print_m result
  

