// Seed: 13374911416722724486,16789183692605617453

module piafalv ( output reg [0:2][3:3][1:0] yxbkho
               , input tri1 logic [1:3][1:2][4:0][0:4] lj [1:1][0:0][1:4]
               , inout trireg logic [0:2][1:4][2:3][3:3] exdenxpkjo [1:1][1:3][2:4]
               );
  // Single-driven assignments
  assign yxbkho = '{'{'bx1},'{'{'bz,'bz}},'{'{'bxzz,'bx}}};
  
  // Multi-driven assignments
  assign exdenxpkjo = '{'{'{'bz1,'bxxx00,'{'bxx,'b10xx00x0,'b11zz1}},'{'{'b111z0,'bz1,'b0},'{'b0x10zxz1,'b0xzx101z,'b1z1x1},'b0z1xz0xxz0z1z0zz1xxxz11z},'{'bzzz0x1xxz101z001011z01z1,'b1z111zzxxxz1z1z0zzzzxxxz,'b0xz1xx1zxxxx11z010x001zx}}};
endmodule: piafalv

module pt (output trireg logic [1:2] bnby [3:2][0:4][4:3], output wor logic [0:2][3:0][1:0][2:1] qhjymxlhd);
  // Unpacked net declarations
  trireg logic [0:2][1:4][2:3][3:3] xgidhlhm [1:1][1:3][2:4];
  tri1 logic [1:3][1:2][4:0][0:4] xrf [1:1][0:0][1:4];
  
  xor jq(qhjymxlhd, ckgucye, w);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   wor logic [0:2][3:0][1:0][2:1] qhjymxlhd -> logic qhjymxlhd
  
  piafalv ctvzjx(.yxbkho(w), .lj(xrf), .exdenxpkjo(xgidhlhm));
  // warning: implicit conversion of port connection expands from 1 to 6 bits
  //   wire logic w -> reg [0:2][3:3][1:0] yxbkho
  
endmodule: pt



// Seed after: 2905224904613862059,16789183692605617453
