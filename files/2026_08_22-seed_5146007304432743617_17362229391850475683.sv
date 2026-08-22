// Seed: 5146007304432743617,17362229391850475683

module tyy (output reg [0:1][0:4][4:2][2:2] azesm, inout supply1 logic [4:2][1:1][2:0] hdl [3:2], input reg [3:2][0:0] p);
  // Single-driven assignments
  assign azesm = '{'{'bxxxx,'{'b11,'b0,'{'bz}},'{'{'b1},'bz,'b0},'{'{'bz},'bz,'{'b1}},'{'bz,'{'b0zzz},'{'b1}}},'{'{'b0x,'{'b1},'b1},'{'bz,'bz,'b0},'bz11,'{'b00,'{'b0},'{'b1}},'{'{'bz},'{'b1},'{'b1zz0x}}}};
  
  // Multi-driven assignments
  assign hdl = hdl;
  assign hdl = '{'b1x110x110,'{'b01z,'{'{'b0x,'b0,'bx1}},'{'{'b1,'bz,'bzx000}}}};
endmodule: tyy

module wbtfkpou (inout supply0 logic [0:2][2:3][0:4] qt [2:4], output reg llfq, input wor logic [2:1][2:1] tqcfx [0:2][4:1][3:2][0:3]);
  // Unpacked net declarations
  supply1 logic [4:2][1:1][2:0] ynatfuprj [3:2];
  
  tyy edmzx(.azesm(hmbrhxi), .hdl(ynatfuprj), .p(llfq));
  // warning: implicit conversion of port connection expands from 1 to 30 bits
  //   wire logic hmbrhxi -> reg [0:1][0:4][4:2][2:2] azesm
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   reg llfq -> reg [3:2][0:0] p
  
  xor yrqytbapx(llfq, oqdjrcywql, hmbrhxi);
  
  nand v(irca, llfq, llfq);
  
  xnor cswckqpb(hmbrhxi, iwpe, llfq);
  
  
  // Multi-driven assignments
  assign qt = qt;
  assign tqcfx = '{'{'{'{'bx11z,'b0z1,'b0x01,'bx},'{'bz,'bx1x1,'bz111z,'bz}},'{'{'bx10z,'bz1z0,'b00,'b1x0},'{'bx,'b10x00,'b0x11,'b1}},'{'{'bz01z,'b11x1x,'bz01x,'b1xzx1},'{'b01zz,'b1zzz,'b0zzz,'b0x01x}},'{'{'bzxz0,'bzxz0,'bz01,'bz0},'{'bxzxx,'bz101,'bz110,'b00z0}}},'{'{'{'b1z1,'bz11,'b111x,'b1x1z},'{'b1z1z1,'b001z,'bx0z00,'b10}},'{'{'bxzxx,'bxxzx,'bxz11,'b10},'{'bx1xx0,'bzzx1z,'bz001z,'b0zz}},'{'{'b0,'b111zz,'b100,'b0x1z},'{'bz0100,'bx01x,'b0x,'b110x}},'{'{'b1x1x,'bx1x0,'bx11x,'b0101},'{'b1,'bx100,'bx00z,'b0zx}}},'{'{'{'bzx0z,'b10zx,'bz10z,'bz},'{'b11,'bx1xz,'bzz1z,'bx1xz}},'{'{'b0z1,'b1z10,'b1,'bx0z1},'{'b01,'bz10,'bz1z1,'bx000}},'{'{'b0zz1,'bz0zx0,'b1z0,'bxz1x},'{'bz1x,'bx,'bxx10,'bz}},'{'{'bxx1x,'bzxx0,'b0zz0,'b00z1},'{'bz10z,'b0,'b1z1z,'bx1xz}}}};
  assign iwpe = iwpe;
  assign iwpe = oqdjrcywql;
endmodule: wbtfkpou

module tmvohy ();
  // Unpacked net declarations
  supply1 logic [4:2][1:1][2:0] xza [3:2];
  wor logic [2:1][2:1] qsxvytbfrv [0:2][4:1][3:2][0:3];
  supply0 logic [0:2][2:3][0:4] vovqnuxtj [2:4];
  
  wbtfkpou mayjpyaiiy(.qt(vovqnuxtj), .llfq(oqolzv), .tqcfx(qsxvytbfrv));
  
  not kr(oqolzv, inoututque);
  
  tyy disudohli(.azesm(inoututque), .hdl(xza), .p(oqolzv));
  // warning: implicit conversion of port connection expands from 1 to 30 bits
  //   wire logic inoututque -> reg [0:1][0:4][4:2][2:2] azesm
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic oqolzv -> reg [3:2][0:0] p
  
  and ndzdfhwkfr(inoututque, oqolzv, inoututque);
  
  
  // Multi-driven assignments
  assign oqolzv = 'b1zxx0;
endmodule: tmvohy



// Seed after: 3486016287453186163,17362229391850475683
