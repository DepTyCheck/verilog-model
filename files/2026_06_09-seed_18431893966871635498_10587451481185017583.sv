// Seed: 18431893966871635498,10587451481185017583

module asoahbmbe (input logic [3:4][1:1][2:3][4:2] g);
  
  or f(zsi, zsi, g);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   logic [3:4][1:1][2:3][4:2] g -> logic g
  
  xor dtbodv(vrknwszyvq, tz, mlwnwxx);
  
  not yn(njtpbqyy, rffqmfyj);
  
  and srpbwsly(gulg, njtpbqyy, g);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   logic [3:4][1:1][2:3][4:2] g -> logic g
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign vrknwszyvq = 'b10100;
endmodule: asoahbmbe

module mxto (output reg [1:4] tvpazqgkbj, input bit [1:1][3:2][0:1] eoqfkxj);
  
  
  
  // Single-driven assigns
  assign tvpazqgkbj = '{'b1,'bx111,'bxzz00,'bxz100};
  
  // Multi-driven assigns
endmodule: mxto

module n (input bit gw [1:2]);
  
  mxto fdrm(.tvpazqgkbj(ihwys), .eoqfkxj(e));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic ihwys -> reg [1:4] tvpazqgkbj
  //
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic e -> bit [1:1][3:2][0:1] eoqfkxj
  
  not cjszqp(ihwys, tcampcdvxd);
  
  asoahbmbe kkximidzx(.g(ihwys));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic ihwys -> logic [3:4][1:1][2:3][4:2] g
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign e = ihwys;
endmodule: n



// Seed after: 11566444289227733944,10587451481185017583
