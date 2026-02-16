open Typechecker
open Semantics

let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = y * ((1 / 3) * 6); }
}"
true

let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = 2 * ((1 / 0) * 6); }
}"
false

let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
      function f(int y) public returns(int) { x = (1 / 0); }
}"
false

let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
int y = 2;
      function f(int y) public returns(int) { x = (y/ 0); }
}"
false


let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
      int y = 2;
int z= 0;
      function f(int y) public returns(int) { x = (y/ z); }
}"
true


let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x;
      uint y = 2;
      function f(int y) public returns(int) { x = (y/ 0); }
}"
false


let%test "test_constant_int_if_false" = test_typecheck
  "contract C {
      int x=2;
      int y = 2;
      function f(int y) public returns(int) { x = (y/ x); }
}"
true



  let%test "test_constant_int_if_false" = test_typecheck
   "contract C {
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = (4/2); }
}"
true




(*-------------------TYPECHECKER OK-----------------------*)


(*-------------------TEST INTERPRETE----------------------*)

let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x=1;
      int y = 2;
      int z= 0;
      function f() public returns(int) { x = (y/ z); }
}"
["0xA:0xC.f()"]
  [("x==1");]



  let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = (4/2); }
}"
["0xA:0xC.f()"]
  [("x==2");]



 let%test "test_constant_int_if_false" = test_exec_tx
   "contract C {
      int x=5;
      int y = 2;
      int z= 4;
      function f() public returns(int) { x = y*(y/z); }
}"
["0xA:0xC.f()"]
  [("x==1");]


