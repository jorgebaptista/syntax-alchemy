type ichar = char * int

type regexp =
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

let rec null = function
  | Epsilon -> true
  | Character _ -> false
  | Union (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2
  | Star _ -> true

(* --- Função de teste auxiliar --- *)
let test name r =
  Printf.printf "%-25s -> null = %b\n" name (null r)

(* --- Testes automáticos --- *)
let () =
  (* Casos básicos *)
  let e = Epsilon in
  let c = Character ('a', 1) in

  test "Epsilon" e;
  test "Character 'a'" c;

  (* Union *)
  test "Union(Epsilon, Character)" (Union(e, c));
  test "Union(Character, Character)" (Union(c, c));
  test "Union(Epsilon, Epsilon)" (Union(e, e));

  (* Concat *)
  test "Concat(Epsilon, Character)" (Concat(e, c));
  test "Concat(Character, Epsilon)" (Concat(c, e));
  test "Concat(Epsilon, Epsilon)" (Concat(e, e));
  test "Concat(Character, Character)" (Concat(c, c));

  (* Star *)
  test "Star(Epsilon)" (Star e);
  test "Star(Character)" (Star c);

  (* Combinações mais complexas *)
  let complex1 = Star (Union(e, c)) in
  let complex2 = Concat(Star c, Union(e, c)) in
  test "Star(Union(Epsilon, Character))" complex1;
  test "Concat(Star(Character), Union(Epsilon, Character))" complex2;