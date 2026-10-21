// Seed: 5279753679299581994,15949316591243099433

module hilc (input shortint q [3:3], inout wire logic [1:1][1:1][1:4] ldb [3:2][0:0]);
  // Multi-driven assignments
  assign ldb = '{'{'{'{'b0xxx}}},'{'b01xx}};
  assign ldb = ldb;
  assign ldb = '{'{'{'b1zzx}},'{'{'b1}}};
  assign ldb = ldb;
endmodule: hilc

module q (output triand logic [1:3][0:3][4:2] anow [0:0], output reg [1:3][4:2] tiufrftapa);
  
endmodule: q



// Seed after: 514916571330354602,15949316591243099433
