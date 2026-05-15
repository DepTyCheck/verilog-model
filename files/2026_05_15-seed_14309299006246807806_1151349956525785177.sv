// Seed: 14309299006246807806,1151349956525785177

module tu (output reg [2:0] yx, output bit [2:0][0:1][4:1][4:4] res);
  
  not qcy(yx, res);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   reg [2:0] yx -> logic yx
  //
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:0][0:1][4:1][4:4] res -> logic res
  
  
  // Single-driven assigns
  assign res = res;
  
  // Multi-driven assigns
endmodule: tu



// Seed after: 14273535374678421248,1151349956525785177
