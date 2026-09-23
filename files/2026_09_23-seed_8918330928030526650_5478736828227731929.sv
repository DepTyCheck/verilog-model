// Seed: 8918330928030526650,5478736828227731929

module lp (inout supply0 logic [4:3] pr [2:4][4:3][0:1]);
  // Multi-driven assignments
  assign pr = '{'{'{'{'bz1,'b1},'bxxxz},'{'bz1,'{'b1,'b0zz0}}},'{'{'{'bz,'b0},'{'bz,'bz}},'{'{'b1zx,'b0z1x},'b1xz}},'{'{'bxx,'bzx},'{'b0x,'{'b0z,'b1}}}};
  assign pr = pr;
endmodule: lp

module vcmmfd ();
  // Unpacked net declarations
  supply0 logic [4:3] ha [2:4][4:3][0:1];
  
  lp wstkybvwvd(.pr(ha));
  
  
  // Multi-driven assignments
  assign ha = ha;
  assign ha = ha;
  assign ha = '{'{'{'bzx,'{'bzz0z0,'bz}},'{'bx1,'{'b0,'bx}}},'{'{'{'bz11z,'b0},'b100},'{'bzz,'{'bx0,'b11x}}},'{'{'bz0,'{'bx0x,'b00110}},'{'{'bzzz0z,'b10zzx},'{'b0,'bzx0zz}}}};
endmodule: vcmmfd

module eqv (output trior logic [4:3][1:1][1:0] ekz [4:2][3:2], output shortreal qn [0:0][0:1]);
  // Single-driven assignments
  assign qn = qn;
  
  // Multi-driven assignments
  assign ekz = '{'{'b010x,'{'bxz,'bz1xx0}},'{'{'{'b00101},'{'bxz}},'{'bx1,'bz0}},'{'bzx0x1,'{'{'b00},'bxx}}};
endmodule: eqv



// Seed after: 8227566635450746330,5478736828227731929
