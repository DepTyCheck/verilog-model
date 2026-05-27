// Seed: 7875228784895123148,14629700832272117749

module qwilpkcr (input reg [4:4][2:1][1:2][2:1] concakd);
  
  xnor wk(isf, concakd, concakd);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   reg [4:4][2:1][1:2][2:1] concakd -> logic concakd
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   reg [4:4][2:1][1:2][2:1] concakd -> logic concakd
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign isf = 'b0x1;
endmodule: qwilpkcr



// Seed after: 3327338893759249590,14629700832272117749
