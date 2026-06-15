// Seed: 8844266162851522395,6310664522635210649

module wyf (inout supply0 logic [1:3][1:2][3:2] pl [1:4][2:2][3:0][3:1]);
  
  nand z(fclgtx, c, esi);
  
  xnor osjpb(vg, mr, fclgtx);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign c = fclgtx;
endmodule: wyf



// Seed after: 12023113794469113572,6310664522635210649
