// Seed: 1455528683466838481,6941252564834625607

module x (output wand logic mdapg [1:4][2:3], inout trior logic [2:3][0:4] ly [4:1][2:3][3:3][0:2]);
  // Multi-driven assignments
  assign mdapg = '{'{'bz,'bz},'{'b0,'b0},'{'b0,'b0},'{'bz,'bx}};
  assign ly = '{'{'{'{'bxx0xxx1000,'bz1,'bxzx}},'{'{'b0xxx0zz101,'b1,'bxz011}}},'{'{'{'b011z1,'b01z11,'bx01x1}},'{'{'b0zzz0z100z,'bz01,'b0z1zzx00zz}}},'{'{'{'bx1xx0z0z01,'bxxzzx,'b0x1z0x01zx}},'{'{'bxx1111xz0x,'b10zz,'bx0x0}}},'{'{'{'b1xxx,'b0z0,'bz}},'{'{'bx,'bx,'b0}}}};
  assign mdapg = '{'{'b1,'bx},'{'bx,'bz},'{'b0,'b0},'{'b1,'b1}};
  assign ly = '{'{'{'{'bx10,'bx000,'bx0z1}},'{'{'b00,'b0x1x0,'b1z0z1}}},'{'{'{'bxxxz100zz1,'bz11xzx011z,'b0xxx1zx10x}},'{'{'b1zxz001x10,'b1x011,'bzzzx01xz00}}},'{'{'{'b1z1,'b011x1,'bzz01z11z10}},'{'{'bzz011zx1xx,'bz0101z110z,'bz1z0010x0x}}},'{'{'{'bz1z11xx111,'b0xx1z001x1,'bx0z}},'{'{'bx1z1zx10x0,'bzx,'b0zzz1z1x1z}}}};
endmodule: x



// Seed after: 12090029367589561938,6941252564834625607
