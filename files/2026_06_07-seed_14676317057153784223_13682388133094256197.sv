// Seed: 14676317057153784223,13682388133094256197

module dwdr (output tri1 logic [4:3][1:3][4:4] rs [4:3], output trior logic [1:0][3:4][4:0] wjk);
  
  nand rkic(wjk, qll, sreo);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   trior logic [1:0][3:4][4:0] wjk -> logic wjk
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign wjk = '{'{'{'bxx,'bzz,'bz0x0,'b0,'b0},'{'b1,'bz11,'bz,'b1xz,'bxx}},'{'{'bzxz,'b100x,'bz01x,'bz1xz,'bzz},'{'bx100,'b1,'b111zx,'bzx,'b00xzz}}};
  assign qll = qll;
  assign rs = rs;
endmodule: dwdr



// Seed after: 15739862764068030301,13682388133094256197
