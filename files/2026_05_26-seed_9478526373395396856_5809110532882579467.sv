// Seed: 9478526373395396856,5809110532882579467

module fwetsw (input int cf [0:4], input logic [2:2][3:4] ovp);
  
  xnor rpyy(psg, ns, psg);
  
  xnor pbq(imvnmvct, ovp, ovp);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:2][3:4] ovp -> logic ovp
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:2][3:4] ovp -> logic ovp
  
  not l(psg, ovp);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:2][3:4] ovp -> logic ovp
  
  nand jbjwefs(nq, vtg, ns);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign vtg = psg;
  assign ns = psg;
  assign nq = psg;
  assign psg = 'bzz11;
endmodule: fwetsw



// Seed after: 5825100410879606509,5809110532882579467
