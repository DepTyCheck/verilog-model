// Seed: 14264400415644134721,1773968524403448883

module fnzxpfvn (input bit [4:0][2:1][4:4][2:1] ecbus, inout supply1 logic r [2:2][2:0], output shortreal hw, input bit [2:3][4:4] o [2:4]);
  nand cgaxzh(jfhqgodyc, jfhqgodyc, hw);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal hw -> logic hw
  
  xnor zziaixxcq(hw, jfhqgodyc, iv);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal hw -> logic hw
  
  
  // Multi-driven assignments
  assign jfhqgodyc = 'bx;
endmodule: fnzxpfvn

module uxhdh ();
  // Unpacked net declarations
  bit [2:3][4:4] lauysyrk [2:4];
  supply1 logic dimkgs [2:2][2:0];
  
  and dgv(vb, vb, b);
  
  fnzxpfvn jrelo(.ecbus(b), .r(dimkgs), .hw(asnlv), .o(lauysyrk));
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic b -> bit [4:0][2:1][4:4][2:1] ecbus
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic asnlv -> shortreal hw
  
  and pjhrrzjg(tvhfplke, cxyzqap, vwce);
  
  
  // Single-driven assignments
  assign lauysyrk = '{'{'b1,'b0},'b0,'{'b00001,'{'b0111}}};
endmodule: uxhdh

module ja (output reg wxkhhcmfiu, output triand logic gcoynmx [4:4]);
  // Unpacked net declarations
  bit [2:3][4:4] lakkyaikxt [2:4];
  supply1 logic fzh [2:2][2:0];
  
  not lu(kebic, wxkhhcmfiu);
  
  fnzxpfvn kjaoqejkkq(.ecbus(wxkhhcmfiu), .r(fzh), .hw(kebic), .o(lakkyaikxt));
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   reg wxkhhcmfiu -> bit [4:0][2:1][4:4][2:1] ecbus
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic kebic -> shortreal hw
  
  xor y(kebic, z, wa);
  
  xor jbejd(klwtupzyi, wxkhhcmfiu, zpuwa);
  
  
  // Single-driven assignments
  assign wxkhhcmfiu = 'b01;
  assign lakkyaikxt = '{'{'b010,'{'b1}},'{'{'b1},'b00},'{'b10111,'b0}};
  
  // Multi-driven assignments
  assign fzh = fzh;
  assign z = 'bx101;
endmodule: ja



// Seed after: 11229728140049646635,1773968524403448883
