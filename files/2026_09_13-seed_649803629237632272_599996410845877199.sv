// Seed: 649803629237632272,599996410845877199

module exudq ( inout wor logic [4:1] dcwp [0:0][4:4][3:3][1:3]
             , output logic [4:0] oimk [3:2][0:0][1:4]
             , input triand logic [0:1][3:0][2:2] tcu [3:4]
             , inout tri logic [4:4][4:4][1:2][3:4] unq [1:1][0:0]
             );
  // Single-driven assignments
  assign oimk = '{'{'{'{'b0,'b11zz0,'b0,'bz1x0,'bx1},'b1011z,'{'b0,'bz,'b0,'bx,'bz},'{'bx,'bz,'bx,'bz,'bxxxx0}}},'{'{'b1,'b011zz,'b010zx,'{'bx,'b0,'bx,'bz1,'bz}}}};
  
  // Multi-driven assignments
  assign unq = '{'{'{'b1x00}}};
  assign dcwp = dcwp;
endmodule: exudq

module gqshigy ();
  // Unpacked net declarations
  tri logic [4:4][4:4][1:2][3:4] kryt [1:1][0:0];
  triand logic [0:1][3:0][2:2] isbdpy [3:4];
  logic [4:0] ubqbi [3:2][0:0][1:4];
  wor logic [4:1] iiwkthg [0:0][4:4][3:3][1:3];
  
  not t(eopmr, eopmr);
  
  exudq md(.dcwp(iiwkthg), .oimk(ubqbi), .tcu(isbdpy), .unq(kryt));
  
  
  // Multi-driven assignments
  assign eopmr = 'b0;
  assign kryt = kryt;
  assign isbdpy = '{'{'b00z1,'b01z1},'bx1};
  assign iiwkthg = iiwkthg;
endmodule: gqshigy



// Seed after: 10637637826060653462,599996410845877199
