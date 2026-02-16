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


let%test "test_13_1" = test_exec_tx
  "contract C {  
      uint x;
      function f() public {x = ( 6 * (1/1) );}
  }"
  ["0xA:0xC.f()"] 
  [("x==6");]