// Seed: 8506616257454998478,11252861318739162955

module ew ();
  
  xnor htqofg(o, loj, wruu);
  
  xnor yaiqgp(loj, wruu, loj);
  
  or i(o, o, fmc);
  
  or pfdz(o, eynqwt, wruu);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign loj = wruu;
endmodule: ew

module z (inout triand logic [1:4] vfheq [4:1][1:4][3:4][1:0], output reg [4:3][2:0][2:2][2:3] hrhqxa, inout trireg logic bqkkswrxtd);
  
  nand dkwvbjto(pd, hrhqxa, cgtado);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   reg [4:3][2:0][2:2][2:3] hrhqxa -> logic hrhqxa
  
  
  // Single-driven assigns
  assign hrhqxa = '{'{'{'{'b0zz,'bz}},'{'{'b0,'b0z}},'{'{'bxx1,'bzz111}}},'{'{'{'bx11zx,'b11}},'{'{'b00,'bz0}},'{'{'b01,'b1x0}}}};
  
  // Multi-driven assigns
  assign pd = 'bzx;
endmodule: z

module buax (output bit [0:1][1:1][2:1] irg);
  // Unpacked net declarations
  triand logic [1:4] rj [4:1][1:4][3:4][1:0];
  
  z spwcqd(.vfheq(rj), .hrhqxa(a), .bqkkswrxtd(a));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic a -> reg [4:3][2:0][2:2][2:3] hrhqxa
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign rj = rj;
  assign a = a;
endmodule: buax



// Seed after: 16408112915017274315,11252861318739162955
