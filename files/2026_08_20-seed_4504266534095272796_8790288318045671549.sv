// Seed: 4504266534095272796,8790288318045671549

module nfijxu (inout tri logic [2:2][3:3][3:1][2:0] ppgr [4:3][3:0][3:2][0:1], output time du);
  // Single-driven assignments
  assign du = du;
  
  // Multi-driven assignments
  assign ppgr = '{'{'{'{'b00z110zz0,'b001x1},'{'bzz10,'bx1z}},'{'{'b0x111zzz0,'bxz1zz11z1},'{'bxx,'b1}},'{'{'b1xzx1,'bx1zxx0zzx},'{'b0z,'b01xx1x11z}},'{'{'bx,'bzx00x},'{'bzzzz10011,'bx000z}}},'{'{'{'bx1,'b0x1101z1z},'{'b01zxz1z11,'b10x01}},'{'{'b00x0011x1,'b0z0zxzxxz},'{'bxx1xx0101,'b11}},'{'{'bx,'b01110z11x},'{'bz11z0z0x0,'b1z00z1x1z}},'{'{'bzxz1,'b11z},'{'bx,'bz01z0xxz1}}}};
endmodule: nfijxu

module ptl ();
  // Unpacked net declarations
  tri logic [2:2][3:3][3:1][2:0] lqjr [4:3][3:0][3:2][0:1];
  
  nfijxu ulpeilgh(.ppgr(lqjr), .du(pl));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  //   wire logic pl -> time du
  
  xor qrciktu(pl, hxky, yafufex);
  
  xor tkyeitx(pl, uiao, yafufex);
  
  
  // Multi-driven assignments
  assign lqjr = '{'{'{'{'b01101x000,'bz101zzzxx},'{'b000z00010,'b11x1}},'{'{'bzzx1xxzz1,'bxz1xzx110},'{'b1,'bzxzz0x0xx}},'{'{'b0xz,'bz1z1},'{'bx101zxzz0,'bxz}},'{'{'bxzx1x1xz0,'bz1110z10z},'{'bx0zzzxxxz,'b0xx1}}},'{'{'{'b1xx0x1x0x,'b101010xz1},'{'b10z0xzzz0,'b01}},'{'{'b100zxz0zx,'bx0xz100x0},'{'bx,'b00z10}},'{'{'bz1x00,'b10x},'{'bxx1zx0zxx,'b01x0x1xx0}},'{'{'b00x0xx100,'bzz1z0xx11},'{'b1zzz11zx1,'b1}}}};
  assign uiao = pl;
endmodule: ptl

module eechooavan ( inout supply0 logic [4:1][1:1] nmcq
                  , output trireg logic [1:3] jtyanc [2:1][0:4][2:2]
                  , output trior logic [0:1][4:4][1:3] muocoyi [4:3]
                  );
  and rsamt(sypr, sypr, zfut);
  
  
  // Multi-driven assignments
  assign zfut = sypr;
  assign sypr = sypr;
  assign nmcq = 'b01x0;
endmodule: eechooavan

module skws (input tri logic [0:4] djp [1:0][4:4][0:3][3:4]);
  // Unpacked net declarations
  trior logic [0:1][4:4][1:3] wrgkevj [4:3];
  trireg logic [1:3] mgjdtp [2:1][0:4][2:2];
  
  eechooavan oykgs(.nmcq(rdgsvcbsg), .jtyanc(mgjdtp), .muocoyi(wrgkevj));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic rdgsvcbsg -> supply0 logic [4:1][1:1] nmcq
  
endmodule: skws



// Seed after: 16394995752195392870,8790288318045671549
