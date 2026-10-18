// Seed: 6094607170363355405,5271151552867628667

module xof (output logic [1:3][1:1][0:2] x, inout wand logic [0:0][3:1] qb [2:0][4:3][2:4], inout supply1 logic [2:2][4:2][2:4][2:2] gs);
  and ro(ekiza, x, ekiza);
  // warning: implicit conversion of port connection truncates from 9 to 1 bits
  //   logic [1:3][1:1][0:2] x -> logic x
  
  
  // Single-driven assignments
  assign x = gs;
  
  // Multi-driven assignments
  assign gs = gs;
  assign qb = '{'{'{'{'b0zz},'b1zz,'bz0z1},'{'b1xz,'{'b101},'{'b1xxz}}},'{'{'bxx01,'{'bz1x},'b010},'{'{'bx},'{'bzxx},'{'b0zx}}},'{'{'bxz0,'b00x,'bxz0},'{'b11x,'{'b000},'{'b0x1}}}};
  assign gs = gs;
  assign ekiza = 'b1zx;
endmodule: xof



// Seed after: 5198952363889715850,5271151552867628667
