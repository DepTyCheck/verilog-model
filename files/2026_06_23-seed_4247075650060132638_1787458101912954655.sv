// Seed: 4247075650060132638,1787458101912954655

module dnlr ( output trireg logic [1:2][0:4][3:4][2:4] zzjanrwlpa [2:4]
            , inout tri0 logic [4:3] mjqhsafk [1:0][1:0][0:4][4:4]
            , input logic [3:2] iuoengzpbj [2:2][3:3]
            , output realtime syjctgqoah
            );
  nand luqr(njnik, syjctgqoah, njnik);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime syjctgqoah -> logic syjctgqoah
  
  xor kghrww(c, ebevu, njnik);
  
  
  // Multi-driven assignments
  assign ebevu = 'b1;
  assign c = njnik;
endmodule: dnlr

module jstnl (inout tri0 logic [4:2] huvjwpa);
  // Unpacked net declarations
  logic [3:2] ssdkt [2:2][3:3];
  logic [3:2] zovex [2:2][3:3];
  tri0 logic [4:3] sxxv [1:0][1:0][0:4][4:4];
  trireg logic [1:2][0:4][3:4][2:4] ubrknnzdw [2:4];
  
  dnlr tjx(.zzjanrwlpa(ubrknnzdw), .mjqhsafk(sxxv), .iuoengzpbj(zovex), .syjctgqoah(rsbgrwffkd));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic rsbgrwffkd -> realtime syjctgqoah
  
  dnlr vv(.zzjanrwlpa(ubrknnzdw), .mjqhsafk(sxxv), .iuoengzpbj(zovex), .syjctgqoah(dteypbhi));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic dteypbhi -> realtime syjctgqoah
  
  dnlr xy(.zzjanrwlpa(ubrknnzdw), .mjqhsafk(sxxv), .iuoengzpbj(ssdkt), .syjctgqoah(rsbgrwffkd));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic rsbgrwffkd -> realtime syjctgqoah
  
  xnor j(cyy, u, dteypbhi);
  
  
  // Single-driven assignments
  assign zovex = '{'{'{'b0,'bx}}};
  assign ssdkt = ssdkt;
  
  // Multi-driven assignments
  assign huvjwpa = '{'b1z111,'bz,'bx};
endmodule: jstnl



// Seed after: 2300801777073559223,1787458101912954655
