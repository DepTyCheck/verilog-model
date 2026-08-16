// Seed: 2669823137288065746,16503819601019979169

module sumf (output longint qtwpgcbji, output trireg logic [2:1][1:4] gkgpwkunvs);
  xnor fvtfvh(gkgpwkunvs, gkgpwkunvs, gkgpwkunvs);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [2:1][1:4] gkgpwkunvs -> logic gkgpwkunvs
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [2:1][1:4] gkgpwkunvs -> logic gkgpwkunvs
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [2:1][1:4] gkgpwkunvs -> logic gkgpwkunvs
  
  xnor dgd(b, lnuotgqyw, gkgpwkunvs);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [2:1][1:4] gkgpwkunvs -> logic gkgpwkunvs
  
  not nqyr(ptqmxzxjn, lnuotgqyw);
  
  
  // Single-driven assignments
  assign qtwpgcbji = 'b11;
endmodule: sumf

module zbhmz (output shortreal lmiljg, input real pju, output trireg logic yeuwzvkodb, inout trireg logic [4:0][4:1] dcm);
  not cpy(yeuwzvkodb, t);
  
  xnor wuftevj(nqrmhcl, lmiljg, lmiljg);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal lmiljg -> logic lmiljg
  //
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal lmiljg -> logic lmiljg
  
  nand qv(xyt, yeuwzvkodb, tdah);
  
  not cs(yeuwzvkodb, fyxv);
  
  
  // Single-driven assignments
  assign lmiljg = 'bz;
  
  // Multi-driven assignments
  assign yeuwzvkodb = 'bx10z;
endmodule: zbhmz

module mbm (inout wand logic [0:1][0:4][4:4][3:0] ko [0:1][1:0], input supply0 logic [3:1][1:3][4:1] issdlkojp [0:3]);
  xor bnihgal(lkou, fdcyka, intkud);
  
  sumf drc(.qtwpgcbji(lkou), .gkgpwkunvs(mb));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic lkou -> longint qtwpgcbji
  //
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic mb -> trireg logic [2:1][1:4] gkgpwkunvs
  
  zbhmz v(.lmiljg(ghuqkfkt), .pju(ghuqkfkt), .yeuwzvkodb(epbjgjrohk), .dcm(epbjgjrohk));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic ghuqkfkt -> shortreal lmiljg
  //
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic ghuqkfkt -> real pju
  //
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic epbjgjrohk -> trireg logic [4:0][4:1] dcm
  
  zbhmz bem(.lmiljg(c), .pju(afhddr), .yeuwzvkodb(nlqsp), .dcm(ghuqkfkt));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic c -> shortreal lmiljg
  //
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic afhddr -> real pju
  //
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic ghuqkfkt -> trireg logic [4:0][4:1] dcm
  
  
  // Multi-driven assignments
  assign intkud = intkud;
  assign nlqsp = 'b0;
  assign lkou = lkou;
  assign epbjgjrohk = epbjgjrohk;
endmodule: mbm

module ownjahx (output reg ni [0:2][4:2], inout supply1 logic [4:3][0:3][1:4][0:3] fqnxjvlgk [1:4][2:1][4:1]);
  // Unpacked net declarations
  supply0 logic [3:1][1:3][4:1] x [0:3];
  wand logic [0:1][0:4][4:4][3:0] hjfisn [0:1][1:0];
  
  mbm grvpbvb(.ko(hjfisn), .issdlkojp(x));
  
  zbhmz tvrsc(.lmiljg(daopnilhm), .pju(pspyiwdus), .yeuwzvkodb(pspyiwdus), .dcm(pspyiwdus));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic daopnilhm -> shortreal lmiljg
  //
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic pspyiwdus -> real pju
  //
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic pspyiwdus -> trireg logic [4:0][4:1] dcm
  
  
  // Multi-driven assignments
  assign x = '{'{'{'{'b1,'bz0x,'b1xxx,'b0},'{'b0zz,'bx0,'bx,'b1},'bxx10},'{'b00xz,'bx,'{'b1x1x,'b0,'bz,'bxz00}},'{'b100,'bzzz0,'{'bz,'bz00zz,'b1,'bz}}},'{'bx0z,'{'bzx1z,'{'b1,'bx,'b00,'bx},'bxxz0},'{'bx0z0,'bzx1z,'{'b1,'bx0,'b1,'b1}}},'{'{'bzxz,'{'b101z0,'bxzzzx,'b1,'bx1zx1},'{'bx,'b11,'bz,'b0}},'bxz0,'{'b00x1,'{'bx,'b1x,'b1z,'b0},'bxz00}},'bzzx1zxzxz1zx11xz0z00zxzxzz1x1zz010x0};
  assign fqnxjvlgk = '{'{'{'{'b0x1z0x011zxzx0x0x00xxz1zxxz001xxxzx0x0xxz010z0xxxx0z1x001xz1zx00,'bxxxxxzxx1011x000111z0zx1z1x0x1x11zxzz11zxzxx0x0001001111xzxz1000},'{'bxx,'b10xz1x1zx01x0z0z11x1x110zz011000x1zx0x110z11x0zx0000x1001x010x11},'{'bzx01zx00z10zxx101z010x1zzx11x1xz010xz01001xz1zx11x010zzx11xz1x0x,'bz10x},'{'b0z01z1z0xx00zxzz0zx0110z00z0xzz11010x1z00001x1x00010x0x0z001z10z,'b10z0111x100xx01x0zzx101zz1x0101x1xx1x00xxxzx1z10xzzzxxx1x1100zzx}},'{'{'bzz,'bz11101z001z0zxx0x01x00001000x0000zx1z11z0xz0x1x00x0zz1z00z00zz0x},'{'bzx1z1z00zz01z0x0z101x01001xx000111011xz00x1zx110xzzz00z0xx1zz011,'bx0111},'bzx01xx0xx0x100x1zzzzx01x01z0z1110xxzz00xx1zz1xx0z01xz1x1zxx100zzz0z100x0xxx1x010101xxx0zxz100z10z10zz1x0z010x1z01111xz001xxxz1x1,'b01xxx01x1xz0z1z1011zz11010zxz0x1xzxzxz0zz1zx1x0zzz0zz01xx01z1xzxz0z110x10xxzxxx0zx0000zzxx0zx0zxzxx0101zzxzx0xz0xz1zz01z0x10z01x}},'{'{'{'bzz0xx,'bzx0x010x01x1z00100zxx0z1x1xzx001z0x0z00101zxz11z1zz10z1xx01zz0z0},'b10,'bz110z101xz0zz10z1z1zx10zx10z0101z001x1xz1z1000x000110x01xzz00z1x0z011z0110z0z0z01zx1z1z1zzzx011101xzz110z0xx01xz101z101xx111x110,'bx11x0zzxxzxzzz1zx1x010101z011z10xx110z0zx0z111111111xxxzx000zx11xz1z1x0x1xzx10x11xz1xzz01zxz110x01z10zzx0xxxxx1xxxzzz0z10z101x01},'{'{'b0z1x0,'b1},'b1xx1x01xz0x110zz0111xz0zx0z1zzz0z01xx10zz1x0xzxxz11x01xx1001x011z0x11x100x0zz01xxz10z00x0000xzz101x1100x1x0xx1xz111z11z11x0x0xxz,'{'b10,'b0xx10011z11z0100xxxxxx0zz11zx1xz110xxxx1100011zzxxzxz11xx001xzzx},'b1xxz11000x1x1x10zzx011x0x1xxx0z0x10xz0x1z10z0zzx0xxx0zz0000x0zxzzzxzx0100z00x1000x11xx011110x01x010zx0zz01xxxzzxx10z0z1xx1010x00}},'{'{'b00x0,'{'b00z,'b0},'b011x1xx01xxzxzzxz11x0zxxzz00xzx11zzxx1zxzxz000x000zx10xxzzx00001x1zzzz1zxz0010x01xxz10z00100x0z101z11z01000z00z1z0z0z011x1z1xxx1,'bz10xzxxxxx1110zz1z1x01zz101z1x1zzx10xzxzxzzz1x1xx1011101000x10xx01z1010x1x111zxz1x101xzzzxxzx01x101zx1x0zz10xxx0zz00xz10x1x1001x},'{'bzzxxxzzzxx0x000110zz1011zzz1x00zzzzx11xx1x1xxx1xzx1zxxzx0z001z1zzx0x1z10xzx0xxxzxz01z11z1111x010xzxzx0z10x1z1z0z1xx001zz0z000x0z,'b1,'{'b1x100zzxz111xx11z0z0110x00xx11zxxz0x0z1x1z100z00z0z1001z0x0x0100,'b1z0z1z01z00xxz1011z0z01x00zzz001x01x11xxxx00110z110110zxz0zx0zzz},'{'bx1zzzxxxx1zz0zx11x1z111xzz0xx0zxzxxz10xzxz00zz100x0z1z010z1xz01x,'bxz0}}},'{'{'bz10,'bz00x0x0x11z11zx10x0z01zzx10z1xxz1z00xxxz0x11zxx0x111xx00z0x01xz1x1xx010xz0x001xzxzz1x0xzz0x00x1101x0x01100100xz010xxzxxxx0zx00z1,'{'bzz00x01110x110zz1zzz1xxx1100x11zx0111x0101x110010x11z011x11111z1,'b110},'bx},'{'{'b11zz1x0z1xx1zzzzzz1z101z101x1xx0z0z0z0z1000x10zxxz00x01x11z110x1,'bz111z},'bz1z,'{'b0x,'b10zx001z1xxx1zxx0xxx1z01xz100x11xz0zz1xx0zxx0xzx1z0010x01101zx10},'bzz}}};
  assign fqnxjvlgk = '{'{'{'bx1z11z11101xzz1x0zx10xz01xz11z0xzxxzzz0z00zxz1xzxxx0zx101z100x1z0x10zxx100101xx0011x010xx011z1zz11xzzx01zzxz011xx1x1xx1xz10zz11z,'{'b110zz11zx1zzzx11101z010xxz0z0z0xx0111zxzz10z10001zxz1111z01x00z1,'bzx1zzx0z1z10101zxzxzzz1xzzz0xz01zxzzzxx0z1z0xxx0x0zz101zx01x0zzx},'bz0x11,'{'b00x1z000x01xz100z0xx01x1x0x1011x10x110x1z0z1zxx01z0z000101zx0z0z,'bzx00z}},'{'bx100z0000z00z01x1001zx1z0zxxzxz0zz1000011z110zz1zzxxz1x0zzx11011xz100zx0z00xzx0x0z1z11z0010110xzzz000xx1zz10zz001xx1x000zz010x1x,'b1,'{'b01zz001x10010x1x0zzz1z1xx11000zx1z111z0z10z111x0zz10xxxzzzz10x00,'bxx},'{'b0,'b1}}},'{'{'{'bz0011,'bxzz0zz1xzz0x11xxxz10x100xz01xz1101xxx01z1z10zzx0zzzzzz1zxx10zz01},'{'bxxxz1x1zz0z100zzzz1z0z00z1x0zxzx11x0xz1z100x1x0x01x1xzzx001010zx,'bxx0z0},'b011x10zzxx1z1xz0x1110zx1xzxx10zxzx10xxz1zz1zz1xxxx000xzx0z1001xxx1xzzz1x10111z011zz1zxzxxzzzz001xxzx00z010z1101xz00z0z11xx1z01zz,'b111z01xxx0x101zx110x0z10z001z0111zz00zx1zzz1xz01100100z1zx10x0100zzzx110zx0x00zxzx0z0z0xz010zxzzz00010xzx1101z000zzz10zz1z01z100},'{'b11,'bz011z0101z0z010xzxxz0zz1z1x0xx1x0zx0xzzz11zzx0001z0x0xxz11z00x1zx0x000zz0zz00xzzxzxz0z10z111zz100z1000010z0xz0zzxzxz10x0zz001zzz,'{'bxz11z111z1xx00z11zxx01z0011z1zz1zx11100001011111xx1x11x0xzxx0xzx,'b1xx11x011xzz1zxx01zz100x11z01z001010zzx0zzxxzzxx00xx00x10z0x0zxx},'{'bzxx000x0zz011x1xxzzz1z1z1zz100x1z0zxzz0z0zx0zx0011xx1z1x0zx0100x,'bx1x1}}},'{'{'bzz1,'bz00,'{'bzxx0z1x10z111xzz00000z0xz0z1110z1xx1x1z00x10101111011z00x01z11x1,'b00zzzx01x1z0z10001xzz0xz01011z0zz0xx10xz0xxz01xzzxzx1x0z100101x0},'b0101},'{'{'bx,'b1},'{'bx,'b11z},'bxx,'b01110z1x1z0xz1zz01zzzzz0zx0z10z10z0x1xx1x1100z1x0z00z0101x0zx00z1z11x00xxzx0z00z0001zx1111xz0x0zx01010x1x00x1110xx000z0x100111xx}},'{'{'b0z0x,'bz1z1z0zx1xx1x1z0x0zx01z00x0xxx101z1100x1x11x0xx1001zzzxx01z00zxx01x1zzzz1z1xx1xx0zx1xz00z1xzzx01zx1z0xz011x01zx1x01z10xzx0zxxx0x,'{'bxxz01xz0x00z01zz010x1x11xzzxz000z111zz0x0xzzxz111xz1xz10x10xz0x0,'bxzx1z1011xzz0zz1zzz0zx0111xx0x0111zx011x0111zzxx0101x1x1x11zxz10},'{'b010,'b01x1}},'{'{'b11101zzxz01101xxzzzxz01x0x10000xzx1zz1xz1z01z0x0zz0011111z01zzz0,'b1xzz1zzxx0x00z0zxx0zx1zz1x0z01z1zz010zz00zx11100xz01z00110100011},'{'b100xx00zzzxz010x01010x1z00xxxx0zxx11xxxzzzxzz0x10xz10zz0z1x01x01,'bx10xx},'bx1z0z00z0x110z0xz01xxxxx000110z0z10zxx0z1x01zx1x100xzz1x0z0zx00zx000zzz00z10xx1xzx1z0x1x1x0x11xzzxx10z0011x11zxz00x00xzx100zx0zx,'bz}}};
endmodule: ownjahx



// Seed after: 4703990899608189859,16503819601019979169
