open Typechecker
open Semantics

let%test "test_div_1" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y * ((1 / 3) * 6);  }
  }"
true

let%test "test_div_2" = test_typecheck
"contract C {
      int x;
      function f(int y) public { x = y * (2 / 3); }
}"
false

(* Divisione per 0 con prodotto *)
let%test "test_div_3" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = 2 * ((1 / 0) * 6); }
  }"
false

(* Divisione per 0 *)
let%test "test_div_4" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = (1 / 0); }
  }"
false

(* Divisione per 0 con una variabile *)
let%test "test_div_5" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = (y / 0); }
  }"
false

(* Divisione normale tra due variabili *)
let%test "test_div_6" = test_typecheck
  "contract C {
    int x;
    function f(int y,int z) public { x = (y/ z); }
  }"
true

(* Divisione normale tra due costanti *)
let%test "test_div_7" = test_typecheck
  "contract C {
    int x;
    function f() public { x = (4/2); }
  }"
true

let%test "test_div_8" = test_typecheck
  "contract C {
      int x;
      function f(int y, int z, int a) public { x = z* (y/ a); }
}"
true

(* Divisione con tipo non numerico *)
let%test "test_div_bool_9" = test_typecheck
  "contract C {
    bool x;
    function f(bool y, int z) public { x = (y / z); }
  }"
false

(*segno meno,deve fallire il typechecker perchè provo ad assegnare ad un uint (senza segno)
delle costanti con segno*)
let%test "test_div_uint" = test_typecheck
  "contract C {
    uint x;
    function f() public returns(uint) { x = ((-10)/2); }
}"
false

  (*segno meno*)
let%test "test_div_uint_2" = test_typecheck
  "contract C {
    uint x;
    function f(int y, int z) public returns(uint) { x = ((-y)/z); }
}"
false

let%test "test_div_10" = test_typecheck
  "contract C {
    int x;
    
    function f(int y,int z) public { x = (-y/z); }
}"
true

let%test "test_div_11" = test_typecheck
  "contract C {
  int x;
  function f() public { x = 7*((-2)/3); }
}"
false

(* Risultato non intero ottenuto da un'addizione *)
let%test "test_div_12" = test_typecheck
  "contract C {
      int x;
      function f() public { x = (1/3)+(1/3);}
}"
false

(* Risultato intero ottenuto da un'addizione *)
let%test "test_div_13" = test_typecheck
  "contract C {
      int x;
      function f() public { x = (2/3)+(1/3);}
}"
true

(* Risultato intero ottenuto da una sottrazione *)
let%test "test_div_13" = test_typecheck
  "contract C {
      int x;
      function f() public { x = (4/3)-(1/3);}
}"
true

let%test "test_div_14" = test_typecheck
  "contract C {
  int x;
  function f() public { x = 3*((-2)/3); }
}"
true

(*-------------------TYPECHECKER OK-----------------------*)


(*-------------------TEST INTERPRETE----------------------*)


(* L'assegnamento non avviene perché la divisione per 0 viene bloccata dal typechecker *)
let%test "test_div_12" = test_exec_tx
  "contract C {
    int x;
    int y;
    int z;
    function f(int y) public {
      y = 2;
      z = 0;
      x = (y/ z); }
}"
  ["0xA:0xC.f()"]
  [("x==0");]

let%test "test_div_13" = test_exec_tx
   "contract C {
      int x;
      function f() public { x = (-4/2); }
}"
  ["0xA:0xC.f()"]
  [("x==-2");]

 let%test "test_div_14" = test_exec_tx
   "contract C {
      int x;
      function f(int y,int z) public { x = z*(y/z); }
}"
  ["0xA:0xC.f(2,4)"]
  [("x==2");]

  let%test "test_div_15" = test_exec_tx
   "contract C {
      int x;
      function f(int y,int z) public { x = y*(z/4); }
}"
  ["0xA:0xC.f(2,4)"]
  [("x==2");]

(*in questo caso non effettuiamo la divisione diretta ma prima semplifichiamo la stessa con il prodotto*)
let%test "test_div_16" = test_exec_tx
   "contract C {
      int x;
      function f(int y,int z) public { x = (-z)*(y/z); }
}"
  ["0xA:0xC.f(9,2)"]
  [("x==-9");]
    
(*segno meno*)
let%test "test_div_17" = test_exec_tx
   "contract C {
      int x;
      function f() public { x = (-10/2); }
}"
  ["0xA:0xC.f()"]
  [("x==-5");]

let%test "test_div_18" = test_exec_tx
   "contract C {
      int x;
      function f(int y,int z) public { x = (-y*z); }
}"
  ["0xA:0xC.f(9,2)"]
  [("x==-18");]


(* Espressione che dovrebbe essere semplificata sinistra*)
let%test "test_div_19" = test_exec_tx
  "contract C {
    int x;
    function f() public { x = 9*(1/3); }
}"
  ["0xA:0xC.f()"]
  [("x==3");]

(* Espressione che dovrebbe essere semplificata destra*)
let%test "test_div_mul_mix" = test_exec_tx
  "contract C {
    int x=1;
    function f() public { x = (1/3)*9; }
}"
  ["0xA:0xC.f()"]
  [("x==3");]

let%test "test_div_20" = test_exec_tx
  "contract C {
    int x;
    function f(int y) public { x = y*(2/7); }
}"
  ["0xA:0xC.f(7)"]
  [("x==2");]

let%test "test_div_21" = test_exec_tx
  "contract C {
    int x=1;
    function f() public { x = 7*(2/3)*3; }
}"
  ["0xA:0xC.f()"]
  [("x==14");]

let%test "test_div_22" = test_exec_tx
  "contract C {
    int x;
    function f() public { x = -7*(2/3)/-2; }
}"
  ["0xA:0xC.f()"]
  [("x==2");]

  let%test "test_div_23" = test_exec_tx
  "contract C {
    int x;
    function f(int y,int z) public  { x = 2*(-y*z)/2; }
}"
  ["0xA:0xC.f(9,2)"]
  [("x==-18");]

  let%test "test_div_24" = test_exec_tx
  "contract C {
    int x;
    function f(int y, int z) public { x = 2-(y/z); }
}"
  ["0xA:0xC.f(9,2)"]
  [("x==-2");]


