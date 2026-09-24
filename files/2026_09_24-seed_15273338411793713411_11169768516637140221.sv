// Seed: 15273338411793713411,11169768516637140221

module e ( input wire logic [1:0][3:1][0:1] kaehu [2:2][1:0][1:3][0:3]
         , input supply0 logic [1:0] kqds [3:0][4:0]
         , inout supply0 logic [2:0][1:0][4:1][2:3] d [4:3][4:0][0:1][0:1]
         , inout tri1 logic s [0:3][4:0]
         );
  // Multi-driven assignments
  assign s = '{'{'b0,'bzxx,'bx,'b0zxx,'bz},'{'b110,'bx010z,'b1,'b0,'b1},'{'bx,'bx0x1,'bz,'b1x11z,'b1},'{'bz,'bx1zz,'b1,'bzxxz,'b1}};
  assign s = '{'{'b0x,'b11,'bx,'b0,'bz},'{'b1z,'bz,'b01x,'bxx110,'b0z1},'{'bx0z,'b11z0,'bxx,'bx,'b1},'{'bz,'b0,'b0xx1,'bx00xx,'b10}};
  assign kaehu = '{'{'{'{'b01111z10xxxx,'bx1,'b00,'bxx1xx},'{'b101z10zx0zxz,'bz1z01z0zx111,'bxzxzzzzz1101,'b1z010},'{'b01x1xxx0x1x0,'bxx010x0z0zx1,'b0z,'b0zz1z0z11zz1}},'{'{'b10x1zx0xz101,'b0z0xxx00zzx0,'b10xzx00xx1xx,'bx},'{'bx0,'bxz1x10010x11,'b0z1,'b100z1zzz0x1x},'{'bz0z10z1xxx0z,'bx,'bzz,'bxz010}}}};
endmodule: e



// Seed after: 13593612139356577844,11169768516637140221
