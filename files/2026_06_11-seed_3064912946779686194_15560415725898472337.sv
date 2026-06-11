// Seed: 3064912946779686194,15560415725898472337

module rsh (input tri0 logic [2:2] o [4:3][1:1][3:4], input tri0 logic [2:3][0:4][1:0][4:3] i, output logic [4:1][4:0] hhdamfe);
  
  not kihitpfam(xhiqv, hhdamfe);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   logic [4:1][4:0] hhdamfe -> logic hhdamfe
  
  
  // Single-driven assigns
  assign hhdamfe = '{'{'bzzxx1,'bx1z0x,'b1,'bz,'b11zx},'{'b0x010,'b000,'b01,'b0z1xx,'bz01zx},'{'b0,'bzx0,'bzzx,'b0xzx,'b1},'{'b1x,'bzx0z,'bx110x,'bzzz,'b11}};
  
  // Multi-driven assigns
  assign xhiqv = 'bxx00z;
  assign o = o;
  assign i = '{'{'{'{'bx0xxz,'b1z0z0},'{'b11,'bzzx}},'{'{'bxzxz,'bx100z},'{'bz1x,'b11}},'{'{'b1,'b0},'{'bzz00,'bzx10z}},'{'{'bz0,'b1zx0z},'{'b11xx0,'bx11z}},'{'{'b1zzz,'bzx},'{'bz01,'bx1}}},'{'{'{'b11,'b11x11},'{'bx1,'bzx01}},'{'{'b0,'bxx1},'{'bx,'bx1x1}},'{'{'b10zzx,'bxxz0},'{'b1xzz0,'bzx101}},'{'{'bz,'b011x1},'{'bzz,'bx00z}},'{'{'b00x,'b101x},'{'bzzx,'bzzx}}}};
endmodule: rsh



// Seed after: 1746703630255085355,15560415725898472337
