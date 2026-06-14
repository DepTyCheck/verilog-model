// Seed: 11301448749840988758,2061911109456030553

module pldxf (output logic [4:2][3:2][4:3] eb, output wand logic [4:4][4:0] xbp);
  // Single-driven assignments
  assign eb = '{'{'{'bxz01,'b0},'bzz011},'{'{'b1,'b1},'{'b01z,'bz}},'{'b01,'{'b1,'b0xz}}};
  
  // Multi-driven assignments
  assign xbp = xbp;
  assign xbp = '{'{'bx,'bx,'b1,'bx,'b1}};
endmodule: pldxf



// Seed after: 9312635473945217931,2061911109456030553
