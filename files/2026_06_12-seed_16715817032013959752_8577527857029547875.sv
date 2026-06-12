// Seed: 16715817032013959752,8577527857029547875

module bzpvvbqeeu (output logic [1:0][1:4][2:2][1:3] pesqc);
  
  xnor id(tfpnt, pesqc, r);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [1:0][1:4][2:2][1:3] pesqc -> logic pesqc
  
  nand y(x, tfpnt, pesqc);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [1:0][1:4][2:2][1:3] pesqc -> logic pesqc
  
  
  // Single-driven assigns
  assign pesqc = '{'{'{'{'bz,'b1011,'b1x1zx}},'{'{'b0,'bz,'bx0z0}},'{'{'b00x,'b00z1,'bx1x}},'{'{'b1z0,'b0zxxz,'bx}}},'{'{'{'b001,'bzzz,'bzzx}},'{'{'bz1x,'bxxx1,'bz10}},'{'{'b00zz,'bx,'bzxz}},'{'{'b1x0,'bxx,'bzz0z}}}};
  
  // Multi-driven assigns
  assign tfpnt = 'bxzx1;
  assign x = x;
  assign r = tfpnt;
endmodule: bzpvvbqeeu



// Seed after: 18150935181768600570,8577527857029547875
