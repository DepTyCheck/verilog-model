// Seed: 12195505898847753931,345024335239850945

module p (input supply1 logic [4:3][0:3] ygo, input realtime zyu);
  xnor px(ygo, zyu, ygo);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   supply1 logic [4:3][0:3] ygo -> logic ygo
  //
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime zyu -> logic zyu
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   supply1 logic [4:3][0:3] ygo -> logic ygo
  
  
  // Multi-driven assignments
  assign ygo = ygo;
endmodule: p

module ox (output realtime nrxh);
  xor h(nrxh, rfdews, rfdews);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime nrxh -> logic nrxh
  
  
  // Multi-driven assignments
  assign rfdews = 'bx;
endmodule: ox

module guzwzqffq (input trireg logic [3:0][4:3][2:4] jxgs [4:4][0:1][3:1], inout trior logic [3:2] ameaigcd);
  and pdvpnjs(unwel, unwel, unwel);
  
  xor ocmedpj(unwel, avfkwvms, jvw);
  
  
  // Multi-driven assignments
  assign jxgs = jxgs;
endmodule: guzwzqffq

module uvqlhjyvdm (input reg [2:1][0:2][3:2] dzzlurwnbr, input trireg logic s, input uwire logic [1:4] nal [0:2][1:3][4:1][3:1], input uwire logic [1:3][3:0] irngjrc);
  // Unpacked net declarations
  trireg logic [3:0][4:3][2:4] xh [4:4][0:1][3:1];
  
  p gvgoeildtl(.ygo(s), .zyu(irngjrc));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   trireg logic s -> supply1 logic [4:3][0:3] ygo
  //
  // warning: implicit conversion of port connection expands from 12 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   uwire logic [1:3][3:0] irngjrc -> realtime zyu
  
  or svu(ah, cdrv, s);
  
  guzwzqffq deehsv(.jxgs(xh), .ameaigcd(s));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   trireg logic s -> trior logic [3:2] ameaigcd
  
  
  // Multi-driven assignments
  assign xh = '{'{'{'b1xzxxx0z0zz010xxx0z0zxx1,'{'b1xx001,'b1z0xxz,'bz0xx0z,'b1zzz11},'{'b1110z0,'b1zx0x1,'b1,'bzz1}},'{'{'bzzz0,'bz1,'b11zz,'b0111xz},'bxx0,'{'b01,'b0010zx,'b010x1x,'b01011x}}}};
  assign s = 'bx0;
  assign xh = '{'{'{'b1xx01x01100z101x0z11zzzx,'b01z0110xz0z1xxxxxx0zz0xx,'{'bxx,'b01x,'bx0zz,'b01z1xx}},'{'bzzzz1xx100z0z11z1zzx1z0x,'b1z00z1z00xz0zz0x1z101011,'b1x0xx0xxz1z1z1zz1x000xz0}}};
endmodule: uvqlhjyvdm



// Seed after: 6262502764568459552,345024335239850945
