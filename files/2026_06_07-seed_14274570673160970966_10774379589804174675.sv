// Seed: 14274570673160970966,10774379589804174675

module gxj (output logic [3:2][3:0][0:3][4:1] u, input byte urf [0:3][2:4][4:4]);
  
  nand ihcurba(u, mc, u);
  // warning: implicit conversion of port connection truncates from 128 to 1 bits
  //   logic [3:2][3:0][0:3][4:1] u -> logic u
  //
  // warning: implicit conversion of port connection truncates from 128 to 1 bits
  //   logic [3:2][3:0][0:3][4:1] u -> logic u
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign mc = mc;
endmodule: gxj



// Seed after: 8883136675074385766,10774379589804174675
