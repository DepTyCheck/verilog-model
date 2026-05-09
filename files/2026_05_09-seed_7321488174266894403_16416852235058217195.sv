// Seed: 7321488174266894403,16416852235058217195

module g (input bit [4:0][0:3][3:4][3:1] ghhohoku, input logic [1:3][4:3][4:3][2:3] cfp);
  
  nand pcb(jddimtj, ebyqr, ghhohoku);
  // warning: implicit conversion of port connection truncates from 120 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:0][0:3][3:4][3:1] ghhohoku -> logic ghhohoku
  
  or j(jddimtj, ghhohoku, cfp);
  // warning: implicit conversion of port connection truncates from 120 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:0][0:3][3:4][3:1] ghhohoku -> logic ghhohoku
  //
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   logic [1:3][4:3][4:3][2:3] cfp -> logic cfp
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign ebyqr = jddimtj;
  assign jddimtj = 'b0x;
endmodule: g



// Seed after: 8823144807160831016,16416852235058217195
