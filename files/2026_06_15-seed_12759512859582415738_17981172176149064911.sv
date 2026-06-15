// Seed: 12759512859582415738,17981172176149064911

module umy (output wire logic [1:0][1:4][1:1] fcwisb, output bit [1:1] n);
  xnor rodaii(n, fcwisb, c);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:1] n -> logic n
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   wire logic [1:0][1:4][1:1] fcwisb -> logic fcwisb
  
  or nv(fcwisb, psfrxezgh, lgqtcpvid);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   wire logic [1:0][1:4][1:1] fcwisb -> logic fcwisb
  
  xnor uep(c, lgqtcpvid, n);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:1] n -> logic n
  
  
  // Multi-driven assignments
  assign c = 'b0;
endmodule: umy

module etrzuizkf (output trireg logic [3:4] ldf, input bit [4:4][3:4][4:2] xjfhbp, output wire logic [0:1] wxzah [1:1][3:0][2:0]);
  and ngkexovf(luxgdbig, dsmlrg, ldf);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   trireg logic [3:4] ldf -> logic ldf
  
  
  // Multi-driven assignments
  assign wxzah = wxzah;
  assign wxzah = wxzah;
endmodule: etrzuizkf

module aom ();
  xnor lt(cjmk, cjmk, oouucofr);
  
  umy reypgvwvq(.fcwisb(cjmk), .n(cjmk));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic cjmk -> wire logic [1:0][1:4][1:1] fcwisb
  //
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic cjmk -> bit [1:1] n
  
  not mgefmik(cjmk, z);
  
  
  // Multi-driven assignments
  assign z = 'bx;
  assign cjmk = cjmk;
endmodule: aom



// Seed after: 7242247098905239897,17981172176149064911
