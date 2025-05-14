type mf = float array array

module MatF = struct
  let add (m1 : mf) (m2 : mf) : mf =
    let c = Array.make_matrix (Array.length m1) (Array.length m2) 0. in
      let n = Array.length m1 in 
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          c.(i).(j) <- m1.(i).(j) +. m2.(i).(j)
        done;
      done;
      c

  let sous (m1 : mf) (m2 : mf) : mf =
    let c = Array.make_matrix (Array.length m1) (Array.length m2) 0. in
      let n = Array.length m1 in 
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          c.(i).(j) <- m1.(i).(j) -. m2.(i).(j)
        done;
      done;
      c

  let product (m1 : mf) (m2 : mf) : mf =
    let z = ref 0. in
      let c = Array.make_matrix (Array.length m1) (Array.length m2) 0. in
        for i = 0 to (Array.length m1) - 1 do
          for j = 0 to (Array.length m2) - 1 do
            for k = 0 to (Array.length m2) - 1 do
              z := !z +. (m1.(i).(k))*.(m2.(k).(j))
            done;
            c.(i).(j) <- !z;
            z := 0.
          done;
        done;
      c

  let id (n : int) : mf =
    let ide = Array.make n [||] in
    for i = 0 to (n-1) do
      ide.(i) <- Array.init n (fun j -> if i = j then 1. else 0.) 
    done;
    ide

  let print (a : mf) : unit = 
    for i = 0 to (Array.length a) - 1 do
      for j = 0 to (Array.length a.(0)) - 1 do
        Printf.printf " a.%d.%d = %f " i j (a.(i).(j))
      done;
      Printf.printf "\n"
    done
end

let approx_inv (a : mf) (iterations : int) : mf =
  let n = Array.length a in
  let id = MatF.id n in
  let r = MatF.add a id in

  let r_power = ref id in
  let sum = ref id in

  for _ = 1 to iterations do
    r_power := MatF.product !r_power r;
    sum := MatF.add !sum !r_power
  done;
  !sum
  

let () =
  let m1 = [|
    [|-0.7;0.3;0.1;0.1|];
    [|0.1;-0.8;0.;0.2|];
    [|0.1;0.;-1.;0.5|];
    [|0.1;0.1;0.5;-0.9|]
  |] in

  let approx = approx_inv m1 10000 in
  Printf.printf "Approximation de l’inverse :\n";
  MatF.print approx;

  Printf.printf "\nProduit m1 * approx (devrait être proche de l’identité) :\n";
  let test = MatF.product m1 approx in
  MatF.print test