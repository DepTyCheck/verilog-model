// Seed: 9764813158411683174,9425180628280318413

module ry ( output logic [2:1][3:1] jthzgexu [0:4]
          , inout tri logic [3:1][3:4][2:1] jw [2:1][1:0]
          , output bit [2:1][4:4] w
          , output realtime oeohuqod [4:4]
          );
  xnor c(w, kys, w);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][4:4] w -> logic w
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][4:4] w -> logic w
  
  xor rxq(atqyfp, w, mxzg);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][4:4] w -> logic w
  
  not su(wap, sxho);
  
  and yib(kys, fhhrkqx, sfzbbnr);
  
  
  // Single-driven assignments
  assign jthzgexu = '{'{'{'b0,'bx,'bxx10x},'bz0x},'bz1x01x,'{'{'b1,'bx,'bz1x1},'{'bz,'bz,'b10xz}},'{'b0xx,'{'bz,'bx,'bz}},'{'{'b1,'bz,'bz},'bx1x}};
  assign oeohuqod = '{'bxx11x01zz0xzz0xx0z1x1zz001101z1zz1x0100xz10xx0xx0xx1x1xzz00z0111};
endmodule: ry

module hlqrkvp (input realtime oksfripiuy, input bit zywiha, inout wand logic [3:4][2:3] tx [4:0][1:2][0:1][0:0]);
  // Unpacked net declarations
  realtime jzmhcgr [4:4];
  tri logic [3:1][3:4][2:1] tvvrkwpklj [2:1][1:0];
  logic [2:1][3:1] yzpflvk [0:4];
  
  ry wu(.jthzgexu(yzpflvk), .jw(tvvrkwpklj), .w(uzwdlwlx), .oeohuqod(jzmhcgr));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic uzwdlwlx -> bit [2:1][4:4] w
  
  xnor ux(uzwdlwlx, kljznrcq, kljznrcq);
  
  xnor vomkvffg(kljznrcq, ycjkjczd, oksfripiuy);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime oksfripiuy -> logic oksfripiuy
  
  
  // Multi-driven assignments
  assign tx = tx;
endmodule: hlqrkvp

module vwdoa ( inout triand logic [3:0][4:4][3:3][0:3] kjzjdu
             , inout triand logic [4:1][3:3][1:2][0:2] yanxjsp [1:1][2:4][0:0]
             , input reg [4:3] fzruspxj
             );
  // Unpacked net declarations
  wand logic [3:4][2:3] mzesuaicpz [4:0][1:2][0:1][0:0];
  realtime zfb [4:4];
  tri logic [3:1][3:4][2:1] idzwaz [2:1][1:0];
  logic [2:1][3:1] xgkjeoa [0:4];
  
  ry pya(.jthzgexu(xgkjeoa), .jw(idzwaz), .w(wge), .oeohuqod(zfb));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic wge -> bit [2:1][4:4] w
  
  hlqrkvp bxwenctf(.oksfripiuy(hyc), .zywiha(fzruspxj), .tx(mzesuaicpz));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic hyc -> realtime oksfripiuy
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   reg [4:3] fzruspxj -> bit zywiha
  
  
  // Multi-driven assignments
  assign kjzjdu = kjzjdu;
endmodule: vwdoa

module ng ( input wor logic iuzxehtpe [4:3][2:1]
          , inout supply1 logic [0:4][2:4] fyzszcds [2:4][0:2][0:3][4:1]
          , output reg [0:3][2:1][4:3] oaewrfzy [0:2]
          , output wire logic qqdroohp [3:2][4:4][3:1][2:4]
          );
  // Unpacked net declarations
  triand logic [4:1][3:3][1:2][0:2] innatxwgm [1:1][2:4][0:0];
  
  vwdoa twg(.kjzjdu(wi), .yanxjsp(innatxwgm), .fzruspxj(qf));
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic wi -> triand logic [3:0][4:4][3:3][0:3] kjzjdu
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic qf -> reg [4:3] fzruspxj
  
endmodule: ng



// Seed after: 17609973325202695350,9425180628280318413
