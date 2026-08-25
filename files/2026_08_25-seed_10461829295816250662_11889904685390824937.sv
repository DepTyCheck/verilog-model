// Seed: 10461829295816250662,11889904685390824937

module jss (inout trireg logic [4:1][1:1][2:2] gr [3:4], inout tri0 logic bh [3:0][0:0][3:0][0:4]);
  // Multi-driven assignments
  assign bh = '{'{'{'{'b01x,'bx0110,'bz,'b1,'b0},'{'b10x,'b0,'b01,'b1,'bxx},'{'b01,'b1,'b0x,'b1,'b01xz0},'{'b100zx,'bz01,'b0,'bz1xz,'b1z0}}},'{'{'{'bzx,'b1,'bx0x,'bzz1x,'b1},'{'b0xz0,'bx,'b00,'bz,'b0},'{'b01,'b0,'b1x0,'b1,'b1z},'{'bxz,'b0,'b010,'bz,'b11}}},'{'{'{'b0x,'b1,'b10z,'b1z01,'b0z},'{'bzx,'b0zxz,'b1,'b0xz,'b0z1},'{'bzx00,'b1,'b110,'b10z,'b1},'{'b10xx1,'bxx,'b1,'bx10zx,'bx}}},'{'{'{'bz1x,'bz,'bx1,'bx,'b0},'{'bz,'b0,'b11xx0,'bx,'bz0z},'{'bx1,'bz10,'b0,'bz,'bx10},'{'b0,'bz,'bx10z,'b0zx1x,'b1x10}}}};
  assign bh = '{'{'{'{'bx,'b0,'bz,'bz,'b0},'{'b1zx1,'bz,'bxxz,'b0,'b10z},'{'bx,'bx1,'bz,'b0,'bz1x0},'{'bz11,'b00zz,'b0,'bxx1,'b0}}},'{'{'{'bx,'b0,'bz,'bz,'b1},'{'b1,'bx00zz,'bzxxx,'bxzxz,'b1z},'{'b01,'b1,'bz,'bz,'bz1},'{'b1,'b1,'bz,'b1xxx,'b1}}},'{'{'{'bzx1,'b0,'b0,'b0,'b1},'{'b0,'b0z,'b1xx,'b1z0x,'bx},'{'bz,'bz,'b0,'b000z,'bz},'{'b0zz0,'bz1z1z,'bxx0,'b1,'bx}}},'{'{'{'bz,'b1,'bx,'b0,'bz},'{'b0,'bz,'bz,'b1,'b1},'{'bxz0,'bz01x,'bzz0xz,'b1,'bx},'{'b1,'b1,'b0,'bx,'b0}}}};
  assign bh = '{'{'{'{'b10z1x,'bz0,'bz,'bz,'bx},'{'bx1x0x,'b1,'b1,'b1z,'b0},'{'b0x00,'bx,'bxz,'bz,'bz},'{'bz,'b1,'bx0zx,'b1,'b1}}},'{'{'{'b0,'bz,'b0x1,'bx,'b1},'{'bz1z10,'b10xz1,'b0,'bz0,'b1x0},'{'bz,'b1,'bz10z,'b0,'bx},'{'b1,'b0,'b0,'b0,'bz0001}}},'{'{'{'b10x1,'bx,'bz001,'bxz11,'bxxz},'{'b1x0x,'bx,'b110,'b0,'b1x},'{'b0,'b1,'b0,'bzx,'b1x},'{'bzz00,'bz,'b0,'bz,'bz0x}}},'{'{'{'bx,'bx,'bz,'b1,'b0},'{'bx,'bz,'b0100,'b0,'bz0zx},'{'bx1x,'bz001,'bz,'b001,'bx},'{'bz,'b1z,'bx,'bz,'bx}}}};
endmodule: jss



// Seed after: 13718030714890971645,11889904685390824937
