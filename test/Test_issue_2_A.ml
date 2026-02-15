open Semantics

(*******************************************)
(*          PARTE DI VERIFICA AND          *)
(*******************************************)
let%test "and_primo_false" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;  // primo termine sarà false (x==1)
          if (x==3 && this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
          y = y + 1;  // contatore chiamate
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==1"; "y==0"]  (* y DEVE essere 0 perché g() non viene chiamata *)


  let%test "and_primo_true" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;  // primo termine sarà true (x==1)
          if (x==1 && this.g()==5) { x = x + 1; } //(x = 1+1)
      }
      function g() public returns(uint) { 
          y = y + 1;  // contatore chiamate
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==1"]  (* y DEVE essere 1 perché g() viene richiamata 1 volta *)



  
(*******************************************)
(*          PARTE DI VERIFICA OR           *)
(*******************************************)

  let%test "or_primo_true" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;  // primo termine sarà true (x==1)
          if (x==1 || this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
          y = y + 1;  // contatore chiamate
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==0"]  (* y DEVE essere 0 perché g() NON deve essere chiamata *)


  let%test "or_primo_false" = test_exec_tx
  "contract C {
      uint x;
      uint y;
      function f() public { 
          x = 1;  // primo termine sarà true (x==1)
          if (x==2 || this.g()==5) { x = x + 1; }
      }
      function g() public returns(uint) { 
          y = y + 1;  // contatore chiamate
          return 5; 
      }
  }"
  ["0xA:0xC.f()"]
  ["x==2"; "y==1"]  (* y DEVE essere 0 perché g() NON deve essere chiamata *)