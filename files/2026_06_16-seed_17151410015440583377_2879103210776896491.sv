// Seed: 17151410015440583377,2879103210776896491

module suoil ( output wand logic [3:1][0:2] tsgnyn [2:0]
             , input bit [0:0][4:1] jkt
             , output triand logic [4:0] blvmukmod [1:2][1:3][3:4][3:0]
             , input logic [4:3][4:3][0:3] coxsyxtt [3:3]
             );
  nand nvrek(qw, jkt, jkt);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:0][4:1] jkt -> logic jkt
  //
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:0][4:1] jkt -> logic jkt
  
  xor ecgqqsa(bb, qw, bb);
  
  
  // Multi-driven assignments
  assign blvmukmod = '{'{'{'{'b1x,'bx,'bz11,'bx1z0},'{'bz10x1,'b00z,'b1101,'bx01z1}},'{'{'b110zz,'b11x10,'bx10zx,'b1zx0z},'{'b1x,'b1,'bxx1xx,'bx0z11}},'{'{'b0x011,'bxx01x,'b0,'b0xxz0},'{'b11110,'bzxxz,'bxx1,'b1x}}},'{'{'{'b1x0z,'bx00x0,'bx0,'b01},'{'bzz0z0,'bx0,'b0,'bx0x00}},'{'{'bzxz,'bxx1,'bxx,'bx},'{'b0zx01,'bx0x00,'bz,'b1z11}},'{'{'b0,'bx1z,'bz01,'bzzxxx},'{'b0z0,'b0110z,'b101xz,'b11xzx}}}};
endmodule: suoil



// Seed after: 11507139006778093076,2879103210776896491
