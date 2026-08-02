// Seed: 9668089096848185915,17512270304153351767

module rkef (output supply1 logic [4:0][4:4][1:4][0:1] chg, output wand logic [3:4] qdelgx [3:4][2:4][4:0], output bit o, inout wand logic [3:1][4:1] unbpuyxebr);
  // Single-driven assignments
  assign o = o;
  
  // Multi-driven assignments
  assign qdelgx = '{'{'{'{'b0,'b0},'b11,'{'bx,'b1zx1z},'bxz,'b0},'{'b00zx,'bx1,'{'b110x0,'b1},'b0x,'bxx010},'{'bzx,'bxx,'b01,'{'b01z,'bx},'bxx}},'{'{'bzx,'{'bx10,'b1},'b01,'{'b1,'bx},'b0100},'{'b1x,'{'b00x1,'b111},'{'b0,'bz},'bzx,'b010},'{'b1z,'{'bzz,'bzx},'b11x10,'b11,'b10}}};
endmodule: rkef

module diceryrmr (input supply1 logic [2:3][2:2][0:3] nmzaiwiin [3:3], output reg [1:0][2:3] kc, output bit aqgywfsf);
  // Unpacked net declarations
  wand logic [3:4] eonulyxj [3:4][2:4][4:0];
  
  rkef y(.chg(g), .qdelgx(eonulyxj), .o(g), .unbpuyxebr(g));
  // warning: implicit conversion of port connection expands from 1 to 40 bits
  //   wire logic g -> supply1 logic [4:0][4:4][1:4][0:1] chg
  //
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic g -> bit o
  //
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic g -> wand logic [3:1][4:1] unbpuyxebr
  
  xor odzofjgxsy(aqgywfsf, esmqlj, aqgywfsf);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit aqgywfsf -> logic aqgywfsf
  //
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit aqgywfsf -> logic aqgywfsf
  
  xnor az(gxka, gxka, kc);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   reg [1:0][2:3] kc -> logic kc
  
  
  // Single-driven assignments
  assign kc = '{'{'bx,'bz01x},'bz};
  
  // Multi-driven assignments
  assign g = 'b1;
  assign nmzaiwiin = nmzaiwiin;
  assign nmzaiwiin = '{'{'{'bx111},'{'bz}}};
endmodule: diceryrmr



// Seed after: 9652315714398375838,17512270304153351767
