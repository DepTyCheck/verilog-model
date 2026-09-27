// Seed: 9369140172696948130,1773968524403448883

module kh (input supply1 logic [0:4][3:4] glbuwb [4:4][4:0][1:2], output trireg logic h [1:0][0:3][4:2]);
  xnor qbp(p, p, paulken);
  
  and a(trtt, paulken, trtt);
  
  
  // Multi-driven assignments
  assign h = '{'{'{'b0xzx,'b0,'b0x00},'{'bzzx,'bz11x0,'bx},'{'bz,'bz,'b0},'{'bxx,'b0,'b1}},'{'{'bz,'b1x,'bxz},'{'b00,'bzx,'b1},'{'b1,'bx0z0,'b1},'{'b1,'bx,'bx010}}};
  assign trtt = p;
  assign h = h;
  assign glbuwb = '{'{'{'bx1,'bzxzz00zzzx},'{'bzx,'{'b00,'bz,'b01zx0,'b1x,'bxz}},'{'bz11z0xzx10,'bzx0x10z00z},'{'{'bx0,'bz1zxx,'bz1,'bzxzx,'b10x},'{'bxx,'bz0,'bz1,'bzz,'bxx1}},'{'b10x11x0zx0,'b1}}};
endmodule: kh



// Seed after: 12375319691933005008,1773968524403448883
