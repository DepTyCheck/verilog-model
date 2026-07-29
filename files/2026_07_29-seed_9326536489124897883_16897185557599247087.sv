// Seed: 9326536489124897883,16897185557599247087

module dicm (output reg [1:4][3:0][1:4] sxcc [1:4], inout tri1 logic [4:3][0:2][3:3] h, inout wire logic [0:4][3:3][3:0][0:0] jvppb [1:1]);
  // Single-driven assignments
  assign sxcc = '{'{'b11,'{'b01x,'{'bxx,'b0,'b0,'b0},'{'bzx0,'b0xz,'bx,'b1},'{'b1,'b1,'bz,'b0}},'{'bx10x,'{'b0,'bx0,'bx,'b11xx},'b1,'b0xz0},'{'b010x,'{'b1,'bx,'b10,'b1},'b1xx0,'{'bz1,'b1,'b0,'b0}}},'{'{'{'bxx1zx,'b1,'bx,'b1xxx},'{'b0011,'bx,'bz00,'bz},'b0,'{'bz,'b1,'b1,'b0}},'bz,'{'b1zxx,'{'bx,'bxz,'bx0,'b0x1x},'{'b0,'b010,'b01z10,'b1},'bxx01},'{'bzz0z,'bxx,'b0010z,'{'bz,'b00zx0,'b0,'b1}}},'{'{'bz1,'{'b0,'b1,'b0xx0,'bz},'{'bz,'b0,'bxx,'bx},'{'bx,'bz,'bx,'bx}},'bx,'bz,'{'b10x,'bxzx1,'b0,'b0xz}},'b10001zz1z001000z1xzz0z0z001xxx1z0zzxxx0x0z1zzx1z11x10z1z0zz01z0z};
  
  // Multi-driven assignments
  assign jvppb = '{'{'bxxz0,'{'bxzx0},'{'{'bz,'bx,'b00,'b0}},'{'b1z1z},'{'{'b101,'b01z0,'bz,'bz}}}};
  assign h = h;
  assign jvppb = '{'{'b1zx0,'b0,'bx,'{'{'b1,'b11,'bzxx0,'b0}},'bxz11}};
endmodule: dicm



// Seed after: 8132814055425870096,16897185557599247087
