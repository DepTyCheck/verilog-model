// Seed: 499399615933172029,11889904685390824937

module xbi ( output bit vsm
           , input tri1 logic [1:3][1:3][3:3][4:1] ielvdhda
           , input logic [2:2][3:4][4:2][4:0] bihjj
           , output triand logic [1:3][4:0][0:3] y [3:3][0:2]
           );
  // Single-driven assignments
  assign vsm = 'b1;
  
  // Multi-driven assignments
  assign y = '{'{'{'{'b1,'b01,'bz11x,'bzxx0,'b00zz},'bx101,'b0xz0},'{'b00x0111xz1x1xx10z10z,'{'bzxxz,'b0xxz,'b0101,'b11,'bzxz},'b11x1z},'{'b100x1zzz0xx10xz1zx10,'b1z00,'{'b101z,'b0zz1,'b0,'b000z,'bzxxz0}}}};
  assign ielvdhda = '{'bxzx,'b1z1zx110x111,'{'b1z1z,'b0zz0,'{'{'bxz,'b00z,'bz110x,'b0}}}};
  assign y = '{'{'b00z00z1zx0z11011xx101x10111x0001111z0x11110x10z0x0zxx001x0xx,'{'{'b1z,'bx1z0,'b10xz,'b111x,'bx1xx},'bx0zxx0xxzx11x1010000,'{'b0x1z,'bxx10,'bxxz,'bx001,'bx001}},'{'b1z01,'bx0zxxzz001x0z0xx0zxz,'bzx0}}};
  assign y = y;
endmodule: xbi

module bhjh (input supply0 logic [4:1][4:3][4:0] jpjmu [4:2]);
  // Unpacked net declarations
  triand logic [1:3][4:0][0:3] evwtd [3:3][0:2];
  
  xbi ecqvi(.vsm(mpz), .ielvdhda(fnrixv), .bihjj(hnk), .y(evwtd));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic mpz -> bit vsm
  //
  // warning: implicit conversion of port connection expands from 1 to 36 bits
  //   wire logic fnrixv -> tri1 logic [1:3][1:3][3:3][4:1] ielvdhda
  //
  // warning: implicit conversion of port connection expands from 1 to 30 bits
  //   wire logic hnk -> logic [2:2][3:4][4:2][4:0] bihjj
  
  xnor cwhn(mpz, hnk, hnk);
  
  
  // Multi-driven assignments
  assign jpjmu = '{'bx,'{'{'b0000x,'{'b110z,'b1,'bz1,'bxz1,'bx}},'{'{'bx1x,'b1,'bz,'bx,'bz},'b110x1},'{'b1001x,'{'bxzz11,'bz11,'b0,'b1,'b1}},'b11zx},'{'{'{'bx1,'bx,'b0z0,'b01,'b0},'{'bx0,'bz,'bz1z,'b0x0,'b0}},'{'b0x,'b01100},'{'{'bx,'bz,'b0x,'bz1x,'b1},'bzxz0x},'{'{'bz,'bx,'bx,'b1,'b1zx0},'b1zz10}}};
  assign jpjmu = jpjmu;
  assign jpjmu = jpjmu;
endmodule: bhjh



// Seed after: 14431885113607467891,11889904685390824937
