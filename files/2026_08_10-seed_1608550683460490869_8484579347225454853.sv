// Seed: 1608550683460490869,8484579347225454853

module fyjkfb (input wire logic [0:4][3:2][2:1] krhujf, inout tri logic [2:1][4:2][0:2][2:0] zmw [2:2][3:3][0:3], output bit [0:2][3:2] vh [2:4]);
  xnor ogk(wgrdo, krhujf, krhujf);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   wire logic [0:4][3:2][2:1] krhujf -> logic krhujf
  //
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   wire logic [0:4][3:2][2:1] krhujf -> logic krhujf
  
  or kbjcf(krhujf, ksxv, zmxqeuuix);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   wire logic [0:4][3:2][2:1] krhujf -> logic krhujf
  
  xor mmhcgggs(vpw, z, wgrdo);
  
  
  // Single-driven assignments
  assign vh = '{'{'{'b0,'b00000},'{'b10,'b11000},'{'b0111,'b100}},'b001011,'{'{'b1,'b0},'b00,'{'b0,'b1}}};
  
  // Multi-driven assignments
  assign zmw = '{'{'{'b0,'b01zzzz010x1z1xz1x11z0z1111xx1xz10zzz0xz1z00011010z01zx,'b01xxzxzz0z001zx10xxzx0010zz11zxx0x0x1x0z100xxzxz0z0zz1,'bxz0}}};
endmodule: fyjkfb

module v ();
  // Unpacked net declarations
  bit [0:2][3:2] adfh [2:4];
  bit [0:2][3:2] g [2:4];
  tri logic [2:1][4:2][0:2][2:0] jxngnv [2:2][3:3][0:3];
  
  xor ogbzmuahn(avibqi, paaorl, avibqi);
  
  fyjkfb whtikk(.krhujf(avibqi), .zmw(jxngnv), .vh(g));
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic avibqi -> wire logic [0:4][3:2][2:1] krhujf
  
  fyjkfb yycp(.krhujf(avibqi), .zmw(jxngnv), .vh(adfh));
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic avibqi -> wire logic [0:4][3:2][2:1] krhujf
  
  
  // Multi-driven assignments
  assign paaorl = avibqi;
  assign avibqi = 'bxx;
endmodule: v



// Seed after: 10732832589930832515,8484579347225454853
