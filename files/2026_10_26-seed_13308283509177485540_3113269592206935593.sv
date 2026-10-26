// Seed: 13308283509177485540,3113269592206935593

module rtr (inout tri0 logic a [3:0][4:4], input logic [1:3][1:0] nddn);
  xor v(qbi, e, nddn);
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  //   logic [1:3][1:0] nddn -> logic nddn
  
  xnor pnxmr(pnp, u, iby);
  
  
  // Multi-driven assignments
  assign pnp = qbi;
  assign a = a;
  assign a = a;
  assign u = 'bz;
endmodule: rtr



// Seed after: 15092145657842907781,3113269592206935593
