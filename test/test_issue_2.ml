open Semantics

(* g non viene eseguita, f va nel branch true, x diventa 1 *)
let%test "test_2_or_a" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (true || this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==1");]

(* g viene eseguita, x diventa 10, f va nel branch true, x diventa 11 *)
let%test "test_2_or_b" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false || this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==11");]

(* g viene eseguita, x diventa 10, f va nel branch false, x diventa 12 *)
let%test "test_2_or_c" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false || this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==12");]





(* g non viene eseguita, f va nel branch false, x diventa 2 *)
let%test "test_2_and_a" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (false && this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==2");]

(* g viene eseguita, x diventa 10, f va nel branch false, x diventa 12 *)
let%test "test_2_and_b" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (true && this.g()==0) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==12");]

(* g viene eseguita, x diventa 10, f va nel branch true, x diventa 11 *)
let%test "test_2_and_c" = test_exec_tx
  "contract C {  
      uint x;
      function f() public { if (true && this.g()==1) x+=1; else x+=2; }
      function g() public returns(uint) {x=10; return 1;} 
  }"
  ["0xA:0xC.f()"] 
  [("x==11");]