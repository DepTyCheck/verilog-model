// Seed: 3613979892676015060,6572880414129413999

module cugwit ( output trireg logic [2:2] o [3:4][3:3][3:4][2:1]
              , output trireg logic [0:3][2:3][0:0][4:4] pghint [0:0][3:3][0:3]
              , output bit [0:2][1:1] hhgju
              , output wor logic [1:1][4:1][1:4] hztvzgs
              );
  
  nand zkbj(cnthtgkg, m, onaicywvvs);
  
  nand daxieeg(hztvzgs, x, cnthtgkg);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   wor logic [1:1][4:1][1:4] hztvzgs -> logic hztvzgs
  
  xnor slkdp(hztvzgs, onaicywvvs, ydptqpx);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   wor logic [1:1][4:1][1:4] hztvzgs -> logic hztvzgs
  
  
  // Single-driven assigns
  assign hhgju = '{'{'b10},'{'b0001},'{'b00110}};
  
  // Multi-driven assigns
  assign cnthtgkg = cnthtgkg;
  assign hztvzgs = hztvzgs;
  assign onaicywvvs = 'b01;
endmodule: cugwit

module kdohbexi (input wand logic [0:4][3:3][2:0][2:3] dpqm [2:3][1:1][3:3]);
  // Unpacked net declarations
  trireg logic [0:3][2:3][0:0][4:4] mlzdnfry [0:0][3:3][0:3];
  trireg logic [2:2] kqozvckfd [3:4][3:3][3:4][2:1];
  
  cugwit eneasw(.o(kqozvckfd), .pghint(mlzdnfry), .hhgju(jvtbwnrt), .hztvzgs(ckqdnatbz));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic jvtbwnrt -> bit [0:2][1:1] hhgju
  //
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic ckqdnatbz -> wor logic [1:1][4:1][1:4] hztvzgs
  
  xnor cu(jvtbwnrt, lizik, jvtbwnrt);
  
  and z(jvtbwnrt, jvtbwnrt, pkvqau);
  
  xnor lt(o, pkvqau, o);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign kqozvckfd = kqozvckfd;
  assign pkvqau = o;
  assign o = jvtbwnrt;
endmodule: kdohbexi

module jmkln ( inout supply0 logic [0:1][0:1][4:1][2:2] my [0:1]
             , inout wire logic [3:3][1:3][1:4] vw
             , output wor logic [0:4][2:1][0:4] fkpnf [4:0][3:4][4:2][3:2]
             );
  // Unpacked net declarations
  trireg logic [0:3][2:3][0:0][4:4] wpuujbje [0:0][3:3][0:3];
  trireg logic [2:2] ksjwdlyzxh [3:4][3:3][3:4][2:1];
  wand logic [0:4][3:3][2:0][2:3] dkeqpot [2:3][1:1][3:3];
  
  kdohbexi hnwtybdd(.dpqm(dkeqpot));
  
  not sl(jpkkps, jpkkps);
  
  xnor jrekzfq(jpkkps, jpkkps, jpkkps);
  
  cugwit x(.o(ksjwdlyzxh), .pghint(wpuujbje), .hhgju(r), .hztvzgs(vw));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic r -> bit [0:2][1:1] hhgju
  //
  // warning: implicit conversion of port connection expands from 12 to 16 bits
  //   wire logic [3:3][1:3][1:4] vw -> wor logic [1:1][4:1][1:4] hztvzgs
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign r = jpkkps;
endmodule: jmkln

module s (input triand logic xh [1:1][3:3][1:4][4:3]);
  // Unpacked net declarations
  trireg logic [0:3][2:3][0:0][4:4] hhktplpzh [0:0][3:3][0:3];
  trireg logic [2:2] h [3:4][3:3][3:4][2:1];
  
  not dqujuve(d, d);
  
  cugwit zcf(.o(h), .pghint(hhktplpzh), .hhgju(rozztaudz), .hztvzgs(d));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic rozztaudz -> bit [0:2][1:1] hhgju
  //
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic d -> wor logic [1:1][4:1][1:4] hztvzgs
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xh = '{'{'{'{'b0,'b1xx},'{'b11xx,'bx},'{'b0xx,'bxz},'{'b0,'b1xz1}}}};
  assign hhktplpzh = hhktplpzh;
  assign h = h;
  assign d = d;
  assign rozztaudz = d;
endmodule: s



// Seed after: 341464192737021972,6572880414129413999
