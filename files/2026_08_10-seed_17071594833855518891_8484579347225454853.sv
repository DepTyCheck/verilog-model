// Seed: 17071594833855518891,8484579347225454853

module njs (input logic [4:3][4:4][0:4] tpnfib);
  nand xy(cnly, cnly, tpnfib);
  // warning: implicit conversion of port connection truncates from 10 to 1 bits
  //   logic [4:3][4:4][0:4] tpnfib -> logic tpnfib
  
endmodule: njs



// Seed after: 18261442642638070564,8484579347225454853
