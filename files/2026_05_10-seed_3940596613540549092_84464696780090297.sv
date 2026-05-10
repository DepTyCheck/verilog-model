// Seed: 3940596613540549092,84464696780090297

module hhy (inout tri logic [4:0][4:0] klqoxk [2:1][0:3][2:0], input triand logic [0:4][3:3] klczgn [2:1][2:2], input logic [2:2][2:1] worqjgk);
  
  xnor ftytoqms(xsphvh, xsphvh, pruottkv);
  
  xor mjsi(d, fuqnjxkwk, worqjgk);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   logic [2:2][2:1] worqjgk -> logic worqjgk
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign klqoxk = klqoxk;
  assign d = pruottkv;
  assign xsphvh = pruottkv;
  assign klczgn = '{'{'{'{'b0zz},'{'b01},'{'bxzzx},'{'bxx0},'{'b01010}}},'{'{'{'b10zz},'{'b0z1},'{'bzx},'{'b0zx1},'{'b0}}}};
endmodule: hhy

module toddrotvi (output logic ntjzbain, inout tri logic [3:0][4:0] piybtc [3:3], output wand logic [3:2][1:4][3:0] jljyeyzuv [2:4][3:4][4:2]);
  // Unpacked net declarations
  triand logic [0:4][3:3] dxdqmfgh [2:1][2:2];
  tri logic [4:0][4:0] hyocko [2:1][0:3][2:0];
  
  xnor xkdgtuimj(ntjzbain, ntjzbain, njcdtqtv);
  
  xnor ntqybw(njcdtqtv, ntjzbain, njcdtqtv);
  
  hhy ugz(.klqoxk(hyocko), .klczgn(dxdqmfgh), .worqjgk(rskopio));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic rskopio -> logic [2:2][2:1] worqjgk
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: toddrotvi

module map (input bit [2:4][1:1] ahpy [3:3]);
  // Unpacked net declarations
  wand logic [3:2][1:4][3:0] nqvcrif [2:4][3:4][4:2];
  tri logic [3:0][4:0] jzixa [3:3];
  
  toddrotvi xlz(.ntjzbain(jpc), .piybtc(jzixa), .jljyeyzuv(nqvcrif));
  
  nand dynmq(jpc, jpc, jpc);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign jpc = 'b1z1xz;
  assign nqvcrif = nqvcrif;
endmodule: map



// Seed after: 9025763573454429352,84464696780090297
