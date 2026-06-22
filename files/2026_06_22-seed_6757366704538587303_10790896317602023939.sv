// Seed: 6757366704538587303,10790896317602023939

module jnoulky (input uwire logic [4:0][0:1][4:4][2:4] qwechnb, inout tri1 logic [4:1][4:0][1:4] ek, input reg [0:4] ilct);
  // Multi-driven assignments
  assign ek = 'bxz0x0;
  assign ek = '{'{'{'bz,'b1,'b0,'bx},'{'b110,'b1z,'bxzxz,'bz},'{'bz,'bz0,'b0,'b01x},'bzz0z,'{'b0,'bx,'bx,'bx100x}},'{'{'bz,'bz,'bx,'bz1zz1},'{'bx,'b0,'bx,'bz},'{'bzz1,'bz,'bz,'b1},'b1x0x,'{'b10,'bzx,'b0,'bzxz}},'b0z,'{'{'b0zxx1,'bx101,'b1,'b1},'b1x11,'{'bxx00,'b0000,'b0,'bx},'b0,'{'b00x,'b0zzz,'b0x1,'b0}}};
endmodule: jnoulky

module coapvlnja (inout tri logic i [1:1][1:2][4:1], output reg kjh, output int swzwg);
  xnor en(m, za, m);
  
  xor euggxjc(zchdctiimn, swzwg, kjh);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   int swzwg -> logic swzwg
  
  
  // Single-driven assignments
  assign swzwg = 'b0000;
  assign kjh = 'b1;
  
  // Multi-driven assignments
  assign za = 'bz11;
  assign zchdctiimn = 'bx;
  assign i = '{'{'{'bx,'bz,'b010zz,'bxz1},'{'b1zx00,'bz,'b0,'b01}}};
endmodule: coapvlnja

module ta ();
  // Unpacked net declarations
  tri logic kq [1:1][1:2][4:1];
  
  coapvlnja aiwqryi(.i(kq), .kjh(dp), .swzwg(ro));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic ro -> int swzwg
  
  xnor eogsjv(ppnaq, b, ro);
  
  xnor qqgsvti(vsjip, b, mtirrdesog);
  
  
  // Multi-driven assignments
  assign kq = kq;
endmodule: ta

module f (output reg [0:2][2:2][2:3] aqudteeokz [2:2], output wire logic e [4:2][2:4][0:1], output tri0 logic jh [2:0]);
  ta bz();
  
  jnoulky abavq(.qwechnb(ssxm), .ek(jagrf), .ilct(krrnivg));
  // warning: implicit conversion of port connection expands from 1 to 30 bits
  //   wire logic ssxm -> uwire logic [4:0][0:1][4:4][2:4] qwechnb
  //
  // warning: implicit conversion of port connection expands from 1 to 80 bits
  //   wire logic jagrf -> tri1 logic [4:1][4:0][1:4] ek
  //
  // warning: implicit conversion of port connection expands from 1 to 5 bits
  //   wire logic krrnivg -> reg [0:4] ilct
  
  
  // Single-driven assignments
  assign aqudteeokz = aqudteeokz;
  
  // Multi-driven assignments
  assign jh = '{'b00z1x,'bx,'b0};
  assign jh = jh;
  assign jh = '{'b1,'bz,'bz01x};
endmodule: f



// Seed after: 14688528197282354881,10790896317602023939
