// Seed: 1029404758363229040,10260911658500337197

module wnxo (output tri1 logic ovcw [1:4][4:2]);
  // Multi-driven assignments
  assign ovcw = '{'{'b10x,'b1,'b1},'{'bx,'b0,'b0},'{'b10,'b1z0x,'b0xz},'{'b1z,'b0xx0,'bz}};
  assign ovcw = '{'{'bxx,'bz,'bx0},'{'b1,'bz,'bx},'{'b1,'b1,'bx},'{'bx0xx,'bz00xx,'bx1}};
  assign ovcw = '{'{'bz,'b0,'bzz1},'{'bz,'bx,'bx},'{'bx0,'bxzz0,'b11},'{'b1,'bz0,'b1}};
endmodule: wnxo



// Seed after: 10617393226414186644,10260911658500337197
