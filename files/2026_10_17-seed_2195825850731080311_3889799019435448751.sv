// Seed: 2195825850731080311,3889799019435448751

module hlhvmavcp (output trior logic [3:1][1:2][3:0] xh);
  xnor rbvb(gs, xh, ammsr);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   trior logic [3:1][1:2][3:0] xh -> logic xh
  
  xnor z(gs, bru, g);
  
  nand tvjxtnbtq(xh, ammsr, hwt);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   trior logic [3:1][1:2][3:0] xh -> logic xh
  
  
  // Multi-driven assignments
  assign xh = '{'b0x1z0xz0,'b1zx1,'{'b1zx0,'{'bz,'bzz0,'b1,'b1}}};
  assign g = gs;
endmodule: hlhvmavcp



// Seed after: 1170364871988243747,3889799019435448751
