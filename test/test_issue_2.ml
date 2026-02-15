open Semantics

(* === OR === *)

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

(* === AND === *)

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

(* === CONCATENATI === *)
(* Le potenze di 2 sono primi additivi, quindi ciascuna somma si può ottenere solo con una combinazione di addendi *)

(* Viene eseguito il primo metodo, l'espressione è true, il risultato è 16+2 = 8, cioè 10010 *)
let%test "test_2_chain_a" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==18");]

(* Primo e secondo metodo, true, 11010=26 *)
let%test "test_2_chain_b" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==26");]

(* Primo, secondo e terzo metodo, true, 11110=30 *)
let%test "test_2_chain_c" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==30");]

(* Primo, secondo e terzo metodo, false, 11101=29 *)
let%test "test_2_chain_d" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() || this.add01000() || this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==29");]

(* Stessa cosa con and *)

(* 10001 = 17 *)
let%test "test_2_chain_e" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return false; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==17");]

(* 11001 = 25 *)
let%test "test_2_chain_f" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return false; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==25");]

(* 11101 = 29 *)
let%test "test_2_chain_g" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return false; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==29");]

(* 11110 = 30 *)
let%test "test_2_chain_g" = test_exec_tx
  "contract C {  
      uint x;
      function add10000() public returns(bool) { x+=16; return true; }
      function add01000() public returns(bool) { x+=8; return true; }
      function add00100() public returns(bool) { x+=4; return true; }
      function f() public {x=0; if (this.add10000() && this.add01000() && this.add00100()) x+=2; else x+=1;} 
  }"
  ["0xA:0xC.f()"]
  [("x==30");]