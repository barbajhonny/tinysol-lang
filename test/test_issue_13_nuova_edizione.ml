open Typechecker
open Semantics

(* Esempi della issue *)
let%test "test_div_issue_true" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y * ((1 / 3) * 6); }
  }"
true
let%test "test_div_issue_false" = test_typecheck
  "contract C {
    int x;
    function f(int y) public { x = y * (2 / 3); }
  }"
false

let%test "test_div_ok_good_result" = test_exec_tx
  "contract C {
    int x=111;
    int y = 2;
    int z= 4;
    function f() public returns(int) { x = 2*(5/4)*2; }
  }"
  ["0xA:0xC.f()"]
  [("x==5");]