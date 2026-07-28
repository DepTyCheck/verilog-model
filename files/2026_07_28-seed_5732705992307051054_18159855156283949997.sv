// Seed: 5732705992307051054,18159855156283949997

module mleulcf (input wand logic [1:1][1:4][0:1] tr [3:2][3:2][1:0], output trireg logic [3:3][2:3] hrju, output wor logic [0:1][3:4] wpael [4:2][3:2]);
  // Multi-driven assignments
  assign wpael = '{'{'{'b10,'bxz0},'{'b1z,'bz0}},'{'b1xzx,'b1},'{'{'{'bzx0,'b0x11},'{'b0,'b0}},'{'b1,'{'bx,'bx1}}}};
  assign tr = '{'{'{'bx1xx101z,'{'b0z1000xx}},'{'{'bx11z110z},'bx0}},'{'{'{'bxx},'{'bx000}},'{'{'bxxxzz},'{'bzx00}}}};
  assign wpael = wpael;
endmodule: mleulcf

module luja (inout wire logic yshz, output uwire logic [0:2][3:4][3:4] ewewuyt [0:3][3:1][4:3][0:0], input bit udztaxmnh);
  // Unpacked net declarations
  wor logic [0:1][3:4] kco [4:2][3:2];
  wor logic [0:1][3:4] hzjdcovol [4:2][3:2];
  wand logic [1:1][1:4][0:1] fccd [3:2][3:2][1:0];
  
  xnor jos(max, znmzfjdyha, udztaxmnh);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit udztaxmnh -> logic udztaxmnh
  
  mleulcf nqkak(.tr(fccd), .hrju(yshz), .wpael(hzjdcovol));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic yshz -> trireg logic [3:3][2:3] hrju
  
  mleulcf mrgykdxr(.tr(fccd), .hrju(yshz), .wpael(kco));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic yshz -> trireg logic [3:3][2:3] hrju
  
endmodule: luja



// Seed after: 1207004996305224580,18159855156283949997
