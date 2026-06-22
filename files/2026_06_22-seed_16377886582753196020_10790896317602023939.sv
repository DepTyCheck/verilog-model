// Seed: 16377886582753196020,10790896317602023939

module k ( inout trireg logic [0:4] inlsyp [4:1][1:4][0:2]
         , inout trireg logic [4:3] tqtglluc [0:4][1:4][1:3][0:0]
         , output supply1 logic [0:4][3:0] g [2:3][1:2][1:2][0:3]
         , output bit [3:3][3:1][0:1][3:2] qtv
         );
  xnor mali(w, qtv, qtv);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:3][3:1][0:1][3:2] qtv -> logic qtv
  //
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:3][3:1][0:1][3:2] qtv -> logic qtv
  
  
  // Single-driven assignments
  assign qtv = '{'{'b0011,'{'{'b001,'b1},'b101},'b11000}};
  
  // Multi-driven assignments
  assign g = '{'{'{'{'b0z100100z10xz11001z1,'b1zzx,'b1xx10,'b1},'{'bzx,'bz,'b00x10zzz001xzzxzz11z,'b11100xx10zxzzz011x10}},'{'{'b1x0,'bz,'b1,'bxxzz1xxxx0011x1xx0xx},'{'b00x0xzxzz0x0110zx001,'bzz01z1011101zzx101xx,'bx1z010xxx11x11z10x01,'bz001}}},'{'{'{'b0zx0xzx0xx110x0zzxxx,'bxxx10x1zz1xzz10x10z0,'b00zz1x1zzz010z1xz101,'bxx0010xx11z1xxzx0101},'{'bz0z,'bz00110x1x11xxzz111z0,'bxx10x,'bxxxx0x011z00100xzx0x}},'{'{'b01x01z00xzz1z1x010z1,'b00zzz010zxz110xx0xx0,'b0xz11x0z1zz1011xzz0z,'b0xxz01x001zx1zxx0xx1},'{'bx1z01zzzxxx10xz1xz10,'b1101z111100zx1xzz0xx,'bzzz1x000z11zz00x0001,'b0xxxz11x00z1z00xz0x1}}}};
  assign inlsyp = '{'{'{'{'bz,'bz0zxz,'b1,'b0,'bz1},'bx10x1,'{'bx,'bz,'bx,'b1,'b0}},'{'{'b1,'bx10z,'bz,'bz,'bz1x1x},'b1z101,'bz0xz1},'{'bx,'{'bx,'bz,'bz,'bx01z,'b0},'{'bx,'bxzz0x,'bz1z,'b0,'b0}},'{'{'bz,'bz,'b1,'bx,'b0100x},'b10001,'{'b1zzz,'bz,'b00,'bxzx,'b0}}},'{'{'bx1001,'bzx00z,'b0zxx0},'{'{'b1,'b0,'bz,'bz,'bz},'{'bz,'b1,'b1,'b1,'bx},'b01xxx},'{'b100z1,'bxx,'{'b01z11,'b001,'b100,'b0,'bzz10}},'{'{'b0,'bx,'bz,'bz,'b1zz0},'{'bxzz,'bz,'bx,'bx00,'bxxxz},'{'b1,'bzz10,'b01x,'bx,'b1}}},'{'{'b0x1zx,'b10,'b0x0z1},'{'bx0,'{'b11xz,'bz,'b0,'bzx,'bzzz0x},'{'bx,'b0,'bz,'bz1,'bz}},'{'{'b0,'b0,'bxx,'b11,'bx01},'{'b0,'bz,'bz,'bzz,'bx},'bxzz1},'{'bxx,'bx0xzz,'{'b1,'b0,'b0,'bxzx,'b1xx1x}}},'{'{'b0x,'{'bz00z,'bz,'b0,'b0,'bxx1},'{'bz,'bz,'b0,'bz,'bx}},'{'bx,'bz1x00,'{'bz,'bxzz,'bx,'bx,'bz1zxx}},'{'b1zzz,'{'bx,'bzxx,'b0,'bx0x,'bz},'{'b10x,'b01,'b1,'bz,'b1}},'{'b00x,'bxzz0z,'bzzx}}};
endmodule: k

module ulmzpedkq (inout tri1 logic [0:3] trjdc [2:3][3:0], output trireg logic [3:1][3:3] tse [4:0][1:3][0:3]);
  // Unpacked net declarations
  supply1 logic [0:4][3:0] aabbs [2:3][1:2][1:2][0:3];
  trireg logic [4:3] kcngpnqraj [0:4][1:4][1:3][0:0];
  trireg logic [0:4] rvjuedzxsr [4:1][1:4][0:2];
  
  not gnmus(h, h);
  
  xnor xqbbxkjuai(h, lpkn, h);
  
  k gfugn(.inlsyp(rvjuedzxsr), .tqtglluc(kcngpnqraj), .g(aabbs), .qtv(yqbzqzejpz));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic yqbzqzejpz -> bit [3:3][3:1][0:1][3:2] qtv
  
  and vmpaqr(drxhioi, t, xkfp);
  
  
  // Multi-driven assignments
  assign rvjuedzxsr = rvjuedzxsr;
  assign drxhioi = 'bzz0x;
  assign tse = '{'{'{'{'b0xz0x,'b0,'b0},'{'b0,'b1,'b1xx1},'b0,'{'bx,'b1,'b1}},'{'{'bz,'bx,'bxz0},'{'bz,'bx,'b010z},'b110,'{'b10z,'b1,'b1}},'{'bz0z,'bz00,'b01,'b1z1}},'{'{'bz,'bzx,'b1z000,'b0xzxx},'{'{'bx,'b1,'b0},'bz11,'b0zx,'b1z0},'{'b011z,'{'bx,'b1x0x,'bz00},'b1,'{'b1zx,'bz111,'bz}}},'{'{'{'b1xx1x,'bx,'bx1100},'{'bx,'bx,'bx},'{'bz1,'bzz0,'bx},'{'b0,'b1,'b10xx}},'{'bz1z1,'b0z1,'{'b0z,'b1,'bzxxz},'{'b00,'b01zzz,'b11}},'{'{'b00110,'bx,'bxxz10},'bx,'b0zx,'bz0x0}},'{'{'{'bx1,'bz,'b1},'{'bx,'b10zx1,'b10},'{'b1x,'bzx10,'b00},'{'b0,'bx0x,'b0}},'{'b001,'b0,'{'b10,'b0,'b1000},'{'b0,'b0,'b1}},'{'b0zz,'{'bz01x,'bx,'bx00},'b0x,'{'bz,'bx,'bx11z}}},'{'{'{'b0,'bx,'bz},'b1zz,'{'b0,'bx0,'bz},'b1x},'{'{'b1xz0,'b11x1x,'b0},'{'bz,'b0,'b1},'bx01x,'b01x},'{'b0x0,'{'b0,'bx,'bx},'{'b1,'b0,'bz},'b1xz00}}};
  assign tse = tse;
endmodule: ulmzpedkq



// Seed after: 7222325198397943666,10790896317602023939
