// Seed: 13001382156491741948,12266860541749937009

module oubpoxe (output bit soc, inout supply0 logic [1:3][3:1] qflj [3:1], input uwire logic [3:4] lz);
  // Multi-driven assignments
  assign qflj = '{'{'b101,'{'bx,'bz,'bx},'{'b1xz1,'b11z,'b0zzz1}},'bx1,'{'{'b0000,'bx,'b0z0z},'{'b1,'bz,'bx},'{'b00,'b1,'b1}}};
endmodule: oubpoxe

module ca (input reg [4:4] drnrvmjfr, inout tri1 logic [3:0][0:4] tmkhb, input realtime tzicko [3:4]);
  // Unpacked net declarations
  supply0 logic [1:3][3:1] vinoozj [3:1];
  
  oubpoxe nc(.soc(zwwm), .qflj(vinoozj), .lz(jtfnx));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic zwwm -> bit soc
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic jtfnx -> uwire logic [3:4] lz
  
  oubpoxe x(.soc(jjquwjx), .qflj(vinoozj), .lz(iiqxcoyd));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic jjquwjx -> bit soc
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic iiqxcoyd -> uwire logic [3:4] lz
  
  xnor pzngsxwsg(zwwm, drnrvmjfr, srig);
  
  
  // Multi-driven assignments
  assign tmkhb = tmkhb;
endmodule: ca

module qpf (input supply1 logic [0:4][3:4][4:1][3:4] n [4:0][0:1][1:1], output bit [1:3][1:3] pt);
  // Unpacked net declarations
  supply0 logic [1:3][3:1] ytuuawrz [3:1];
  
  xnor qs(ymy, zwtlgxi, pt);
  // warning: implicit conversion of port connection truncates from 9 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:3][1:3] pt -> logic pt
  
  xor hjidos(vbdwxdi, hgnfdmyo, ymy);
  
  oubpoxe d(.soc(hgnfdmyo), .qflj(ytuuawrz), .lz(tlbu));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic hgnfdmyo -> bit soc
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic tlbu -> uwire logic [3:4] lz
  
  
  // Single-driven assignments
  assign pt = pt;
  
  // Multi-driven assignments
  assign n = '{'{'{'{'bzx01101zxxxz11zz,'b0x101,'b0x0x0xxzz1zzxx0z,'bx1zz0xzxzzz1zx1z,'bxxzxx0xx01xx1xz0}},'{'bxx1zz1z0zzzzzz10x0z1000xxzz011zz011x1x0zz11xzz1101z11zx10zzxx1zzz00xzzz0zx1110xx}},'{'{'bz1z0},'{'bz0xx10xxzxx1zxz0z001x11011z00z0x0xxxxz1zxx01zxz00zzz11x0zzxxz110xz10xx0x1x1zz000}},'{'{'b1xz1z10x0z0zz1101xxz11xx11zzzz01z11111x11x110x1xx1zz1x0101xz0z101x1x0x0zxxx0zz01},'{'{'bx,'b0,'bz100,'bz011010100x0x1xx,'b01z}}},'{'{'{'b0,'b10zxzxx1zz1010x1,'bz1,'bzz1100110zx1z1z1,'b1xxx0x1xzx0zx0zz}},'{'b0100z}},'{'{'{'bz0001111z0zzz000,'b1z1z0,'b0x1z,'bzz,'b0x}},'{'{'b0x01x,'b011z,'b10z101xx1z0x00xx,'bzz00z011z01zxxx1,'b01100zxz0z1zx01z}}}};
  assign ymy = 'bz;
endmodule: qpf



// Seed after: 7247459337330677452,12266860541749937009
