// Seed: 16641749926060965664,5478736828227731929

module joaldzbv (input logic cwdhtzya, output supply1 logic hpahvdh [1:3][1:0][3:1][2:1]);
  
endmodule: joaldzbv

module njtjwggrfd (input supply0 logic [0:4] zkqbz [4:1], output logic [0:2][1:1][0:1][1:3] nkag, inout supply0 logic ydomuoxbf [0:4][4:3][3:2]);
  and pnogsvav(hz, nkag, yohp);
  // warning: implicit conversion of port connection truncates from 18 to 1 bits
  //   logic [0:2][1:1][0:1][1:3] nkag -> logic nkag
  
  
  // Single-driven assignments
  assign nkag = '{'{'{'b1z1,'bxz0}},'{'{'{'b0,'b10,'b1},'b0x1}},'{'{'{'b0,'b1,'b0},'bxz1}}};
endmodule: njtjwggrfd

module f (inout triand logic [2:4] aitmikzfuq [0:1]);
  // Unpacked net declarations
  supply0 logic a [0:4][4:3][3:2];
  supply0 logic [0:4] utln [4:1];
  supply1 logic sfmkp [1:3][1:0][3:1][2:1];
  supply1 logic qniveh [1:3][1:0][3:1][2:1];
  
  joaldzbv fslwtup(.cwdhtzya(gsigyqpwid), .hpahvdh(qniveh));
  
  joaldzbv ouksjpbw(.cwdhtzya(gsigyqpwid), .hpahvdh(sfmkp));
  
  xnor i(qwnfu, snthgy, qwnfu);
  
  njtjwggrfd vdv(.zkqbz(utln), .nkag(gsigyqpwid), .ydomuoxbf(a));
  // warning: implicit conversion of port connection expands from 1 to 18 bits
  //   wire logic gsigyqpwid -> logic [0:2][1:1][0:1][1:3] nkag
  
  
  // Multi-driven assignments
  assign a = a;
endmodule: f

module slpqh (inout wire logic [4:1] gt);
  // Unpacked net declarations
  triand logic [2:4] igirguheu [0:1];
  
  not uxr(vqmrvuylyn, bzuuhyzou);
  
  f xw(.aitmikzfuq(igirguheu));
  
  
  // Multi-driven assignments
  assign gt = gt;
endmodule: slpqh



// Seed after: 13767080308042170891,5478736828227731929
