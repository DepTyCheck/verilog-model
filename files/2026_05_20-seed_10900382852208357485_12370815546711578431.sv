// Seed: 10900382852208357485,12370815546711578431

module fjd (output logic [4:2][0:3] uay, input tri logic [0:4][3:4][4:2][1:1] artjq [4:0][4:1][4:4]);
  
  nand qdahehxwo(uay, rmmajcq, mmadis);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   logic [4:2][0:3] uay -> logic uay
  
  xnor bmmxjjbte(xkrnw, uay, mmadis);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   logic [4:2][0:3] uay -> logic uay
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign artjq = artjq;
  assign rmmajcq = 'b1;
endmodule: fjd

module dfv (input trireg logic lnwqqdg [2:1][3:1], inout tri logic bqlcdquvdb [1:4]);
  // Unpacked net declarations
  tri logic [0:4][3:4][4:2][1:1] hb [4:0][4:1][4:4];
  
  xnor usgunli(pjvjyn, pjvjyn, pjvjyn);
  
  and zup(pjvjyn, ohft, ohft);
  
  fjd v(.uay(ohft), .artjq(hb));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic ohft -> logic [4:2][0:3] uay
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: dfv

module nfcd ();
  
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: nfcd

module jvtfixgx (input shortreal tng);
  // Unpacked net declarations
  tri logic crftexujle [1:4];
  trireg logic vbye [2:1][3:1];
  
  nand ctrfnyx(llam, niyrp, tng);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal tng -> logic tng
  
  or hhn(z, tng, niyrp);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal tng -> logic tng
  
  dfv wxrac(.lnwqqdg(vbye), .bqlcdquvdb(crftexujle));
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign llam = 'b0z001;
endmodule: jvtfixgx



// Seed after: 14096656375047902346,12370815546711578431
