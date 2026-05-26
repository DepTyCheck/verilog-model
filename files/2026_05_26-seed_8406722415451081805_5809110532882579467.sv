// Seed: 8406722415451081805,5809110532882579467

module atnoxbe (input logic [3:1] bphhu);
  
  or qgc(uftcsgxqww, bphhu, wwrdsc);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   logic [3:1] bphhu -> logic bphhu
  
  or bdj(uftcsgxqww, xxaifk, tieqedrxce);
  
  xnor yyubuhffma(fiquzibv, uftcsgxqww, zndyedw);
  
  not snhu(fiquzibv, zndyedw);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign tieqedrxce = xxaifk;
  assign uftcsgxqww = zndyedw;
endmodule: atnoxbe

module vutsl (output real nvhk, output logic [2:1] slyawqfzq);
  
  xnor rweyyqhqo(ffjnekdyym, nvhk, slyawqfzq);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real nvhk -> logic nvhk
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:1] slyawqfzq -> logic slyawqfzq
  
  and gchp(slyawqfzq, cvpt, nvhk);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:1] slyawqfzq -> logic slyawqfzq
  //
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real nvhk -> logic nvhk
  
  
  // Single-driven assigns
  assign nvhk = nvhk;
  
  // Multi-driven assigns
  assign ffjnekdyym = 'b0;
endmodule: vutsl

module qqi ();
  
  xor vybsent(ekt, ekt, m);
  
  and ojqnuusqr(jn, ekt, ilkoxld);
  
  xnor mdbs(fhue, ekt, jn);
  
  vutsl sctohtnomt(.nvhk(m), .slyawqfzq(fhue));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic m -> real nvhk
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic fhue -> logic [2:1] slyawqfzq
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign jn = 'bx;
  assign ilkoxld = 'bz;
  assign m = ilkoxld;
  assign ekt = fhue;
  assign fhue = 'b011;
endmodule: qqi



// Seed after: 10817167676876189003,5809110532882579467
