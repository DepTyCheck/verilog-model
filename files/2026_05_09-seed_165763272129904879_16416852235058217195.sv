// Seed: 165763272129904879,16416852235058217195

module zq (output bit xhsp, input logic [0:2][2:2][0:3][3:4] bhktsv);
  
  xnor zqtjxbkbz(xhsp, dwhzweeb, s);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit xhsp -> logic xhsp
  
  xnor drdjyqc(dwhzweeb, bhktsv, pt);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [0:2][2:2][0:3][3:4] bhktsv -> logic bhktsv
  
  not xzmxu(kridg, pt);
  
  nand lxofqtlwg(dwhzweeb, bhktsv, bhktsv);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [0:2][2:2][0:3][3:4] bhktsv -> logic bhktsv
  //
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [0:2][2:2][0:3][3:4] bhktsv -> logic bhktsv
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign dwhzweeb = 'b01;
  assign kridg = dwhzweeb;
  assign pt = 'b1;
  assign s = 'bx1;
endmodule: zq

module bgzt (inout wor logic [0:0][4:3] zoykpu [2:1][0:1], input reg [3:4] y, output shortreal xu);
  
  xnor rs(dpdmt, xu, shzga);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal xu -> logic xu
  
  zq ntceo(.xhsp(lvu), .bhktsv(qmljhg));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic lvu -> bit xhsp
  //
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic qmljhg -> logic [0:2][2:2][0:3][3:4] bhktsv
  
  not vevbfbgauz(qmljhg, ex);
  
  and s(dpdmt, zcrv, y);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   reg [3:4] y -> logic y
  
  
  // Single-driven assigns
  assign xu = xu;
  
  // Multi-driven assigns
  assign dpdmt = 'bz;
  assign zoykpu = zoykpu;
endmodule: bgzt

module endlj ();
  // Unpacked net declarations
  wor logic [0:0][4:3] k [2:1][0:1];
  
  xnor gjdkeyoi(xkiuzuivnb, xkiuzuivnb, xkiuzuivnb);
  
  bgzt wkuweaemw(.zoykpu(k), .y(xkiuzuivnb), .xu(xkiuzuivnb));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic xkiuzuivnb -> reg [3:4] y
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic xkiuzuivnb -> shortreal xu
  
  and oaipvbad(xkiuzuivnb, sdmljjg, qqxrgjrs);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xkiuzuivnb = qqxrgjrs;
  assign sdmljjg = 'b0100;
  assign qqxrgjrs = 'b1zz;
  assign k = '{'{'{'{'b110,'bzxx1}},'{'{'bx,'b0x}}},'{'{'{'bx0,'bx11}},'{'{'bx0x1,'b110xx}}}};
endmodule: endlj



// Seed after: 16851759749046171377,16416852235058217195
