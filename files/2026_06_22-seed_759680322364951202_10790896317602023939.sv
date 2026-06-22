// Seed: 759680322364951202,10790896317602023939

module hlud (output triand logic [4:2][2:2] yx [2:4][3:2], input tri0 logic [1:1][4:0] r [2:2][1:3], output tri0 logic w [3:0][1:1][2:1][4:4]);
  // Multi-driven assignments
  assign yx = yx;
endmodule: hlud

module sxp (inout supply0 logic [4:2][0:4] upde, output bit [1:3][4:2][4:0] z, input bit [0:2][4:3][4:0] cnsazyrd);
  xnor ei(cm, cm, z);
  // warning: implicit conversion of port connection truncates from 45 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:3][4:2][4:0] z -> logic z
  
  nand dg(z, cnsazyrd, pfshpyy);
  // warning: implicit conversion of port connection truncates from 45 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:3][4:2][4:0] z -> logic z
  //
  // warning: implicit conversion of port connection truncates from 30 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:2][4:3][4:0] cnsazyrd -> logic cnsazyrd
  
  
  // Multi-driven assignments
  assign pfshpyy = cm;
  assign upde = '{'{'bz,'b0,'b00110,'bx,'bx1z11},'{'bx,'b1,'bx,'b1,'b0x000},'b000z1};
  assign upde = '{'bxxzzx,'{'bx,'bzz,'bz,'b1,'bz},'{'bz00xx,'b1x001,'bz,'b1,'bz}};
  assign upde = upde;
endmodule: sxp

module cvffd ();
  sxp vegx(.upde(zedj), .z(b), .cnsazyrd(b));
  // warning: implicit conversion of port connection expands from 1 to 15 bits
  //   wire logic zedj -> supply0 logic [4:2][0:4] upde
  //
  // warning: implicit conversion of port connection expands from 1 to 45 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic b -> bit [1:3][4:2][4:0] z
  //
  // warning: implicit conversion of port connection expands from 1 to 30 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic b -> bit [0:2][4:3][4:0] cnsazyrd
  
  
  // Multi-driven assignments
  assign zedj = 'b010z;
endmodule: cvffd



// Seed after: 8245211481190339810,10790896317602023939
