// Seed: 3254076230366623791,3362223774762004793

module qe ( inout wor logic [1:1][1:3][1:2][3:4] mkprr [4:2][2:4]
          , input tri1 logic vnln
          , output logic [3:1][2:2] wvvtg
          , inout tri logic [0:3][2:4][2:3][4:2] phh [0:2][0:3]
          );
  nand l(wvvtg, ch, wvvtg);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   logic [3:1][2:2] wvvtg -> logic wvvtg
  //
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   logic [3:1][2:2] wvvtg -> logic wvvtg
  
  
  // Multi-driven assignments
  assign phh = '{'{'{'{'bxz00x0,'bzz11z0,'b10x0z},'{'b1z1100,'bx1,'b0z1},'{'b1x,'b10xx1x,'b01z11z},'b0z1},'{'b111x1zxz0101zz0xzz,'{'b00z01,'b0,'bx1x1zx},'{'bzx00zx,'b000,'b00x11z},'bxzxz1},'{'b1010,'b0,'b01,'{'bz1,'b1xz,'bz0xxz1}},'{'{'bxzx,'b0z,'b01xzzx},'{'b11xzxz,'b0xz100,'b1x1xxx},'{'b0x0x0x,'b0zxx00,'b10x},'bx1z0x}},'{'{'{'bzxx,'bx000zz,'bzxx101},'{'b0,'b0zz1z0,'b00xx0x},'{'b1xx110,'b0x,'bx01},'{'bz1zxzz,'b01xx1x,'b1x1xzz}},'b0,'b0z0,'{'{'bx0xzzz,'bx1z,'b0zzx},'bxzx,'b0zzzx0x1xz1zxz0xz0,'bz1zz}},'{'{'bz,'{'bxxx11,'bz,'b0x0xx1},'{'bxzz11,'bzxxz0z,'bzx1},'bx0x0010x1x1011xzxz},'{'{'b0011z0,'bzzxz1z,'b1},'{'b1111xz,'bzx1xx0,'b1xx100},'bxz0xxz10z0011z10xx,'{'b00z010,'b11,'bxz}},'b1z,'{'{'b1,'bzx11z,'bz1xz0},'b011xx11x0z01010x0x,'{'bzx0x0,'b1zz,'b11},'{'bx1z1,'bzx01z0,'b1x11zx}}}};
endmodule: qe



// Seed after: 11743033343424374259,3362223774762004793
