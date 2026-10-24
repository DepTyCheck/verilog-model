// Seed: 13773972521893517396,3802310547804228439

module lpp (output wire logic iiz [1:2][1:4][3:0][2:1], input realtime pkuztzk);
  xnor yszkpdnhi(cvupxw, pkuztzk, faqhssrys);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime pkuztzk -> logic pkuztzk
  
  
  // Multi-driven assignments
  assign iiz = '{'{'{'{'b1,'b0z000},'{'b0,'bxzzzz},'{'bx,'b0},'{'b1zzz,'bx}},'{'{'b0,'b1z0},'{'bx0z0z,'bxxx},'{'b1,'bz},'{'bxzzx,'b011}},'{'{'b0,'b1},'{'bz,'b1},'{'bz,'bx},'{'bx,'b1}},'{'{'bz,'b1},'{'bx,'bz},'{'b0,'b0},'{'bx,'b1}}},'{'{'{'bx01,'bz},'{'b0,'b1},'{'b0xxz,'bx},'{'b1,'bx1z10}},'{'{'bz,'b0x0},'{'b0,'b0zz1},'{'bx1,'bz},'{'b0,'bz}},'{'{'bz1,'bxx001},'{'b01111,'bz},'{'b1,'b011},'{'b1,'b0}},'{'{'bx,'bx},'{'b1,'b0xz},'{'b1,'bz},'{'bz1z0,'bz}}}};
endmodule: lpp

module lmwmap (output byte t, output bit sndpbg [1:2]);
  // Unpacked net declarations
  wire logic aeqodeo [1:2][1:4][3:0][2:1];
  
  lpp z(.iiz(aeqodeo), .pkuztzk(t));
  // warning: implicit conversion of port connection expands from 8 to 64 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   byte t -> realtime pkuztzk
  
  nand fp(t, q, p);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   byte t -> logic t
  
  not fpzyykqhbl(q, t);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   byte t -> logic t
  
  
  // Multi-driven assignments
  assign aeqodeo = aeqodeo;
  assign aeqodeo = '{'{'{'{'b0,'b1011},'{'b1,'bzz},'{'bx,'bz},'{'bz,'b0}},'{'{'bz0x,'bx},'{'bxz,'bx},'{'bx,'b0},'{'bxxx00,'bzx}},'{'{'bz,'b11},'{'bz,'bx},'{'b1,'b1},'{'bz,'b1}},'{'{'bzz,'b0},'{'b0zx,'b1zz},'{'bzx,'bz},'{'b101,'b1z}}},'{'{'{'bz,'bx},'{'bz0,'bz},'{'b1,'bzx},'{'bx,'bxz01}},'{'{'bx11xx,'b0},'{'b011z,'b1},'{'bz01,'bz},'{'b00111,'b1}},'{'{'bzz,'bzz110},'{'bzz1z1,'b0},'{'b1,'b00},'{'b0z,'b0xx}},'{'{'b1,'b0},'{'bx,'b1},'{'b0,'bx},'{'bz,'b0}}}};
  assign aeqodeo = aeqodeo;
  assign p = 'bx;
endmodule: lmwmap



// Seed after: 7755906687363159420,3802310547804228439
