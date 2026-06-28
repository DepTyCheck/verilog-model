// Seed: 17622173616510976542,14301837635999661855

module drnelpi (output bit [2:4][3:0][1:4] r, output bit [1:3][0:0] uq [3:1]);
  xnor mrblg(khym, khym, r);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:4][3:0][1:4] r -> logic r
  
  xnor rsk(thlgtlavp, thlgtlavp, khym);
  
  nand gy(xr, r, xr);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:4][3:0][1:4] r -> logic r
  
  
  // Single-driven assignments
  assign uq = uq;
  
  // Multi-driven assignments
  assign khym = khym;
  assign khym = 'bx1;
  assign khym = 'b0;
endmodule: drnelpi



// Seed after: 4399798390964310119,14301837635999661855
