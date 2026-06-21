// Seed: 6947295160314986958,16774409576788355131

module kvrudh (output wor logic [1:3][1:0] kd [3:3][0:4][1:2][2:4], output supply1 logic [1:1][1:2][3:3] cka [0:2][4:4][4:0][2:4]);
  // Multi-driven assignments
  assign cka = '{'{'{'{'b00z,'b01z,'b10xz},'{'bz1,'b0z,'b10},'{'bx0,'b111zz,'bz0},'{'bx1,'b11,'bx1},'{'b10,'b1x,'b1z}}},'{'{'{'b0x,'b0,'b1},'{'b1xx,'bz1xx,'bz0},'{'b0,'bzxz,'b00},'{'b0,'bz0zz0,'b0z},'{'bzz,'bx,'b11010}}},'{'{'{'b1x,'b1z,'b10},'{'bzz,'bzx,'bz11x},'{'b01,'b01,'bxz},'{'b0z,'b110z0,'b1000},'{'b0,'b0x,'bzz}}}};
endmodule: kvrudh



// Seed after: 9947435059044671620,16774409576788355131
