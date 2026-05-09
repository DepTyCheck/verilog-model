// Seed: 16381257292647527167,16416852235058217195

module rl (inout tri logic m, inout trireg logic uoudltkxiu [1:2][3:2][3:3][2:4], input real xuzuxibvx);
  
  xnor i(pgcrzje, kyrvltvn, xuzuxibvx);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real xuzuxibvx -> logic xuzuxibvx
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign pgcrzje = m;
  assign kyrvltvn = 'b000x;
  assign m = kyrvltvn;
  assign uoudltkxiu = uoudltkxiu;
endmodule: rl

module qy (input bit jzesmyxx, inout wor logic [0:3][4:3][0:4] cdzgbiy [0:4][4:1][4:2][2:2], input logic [4:1][1:1][4:4] lgs);
  
  xnor ftrvni(bzd, mjyxxwnkej, lgs);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   logic [4:1][1:1][4:4] lgs -> logic lgs
  
  not cnzxtoy(bzd, zyyp);
  
  nand p(mjyxxwnkej, whnhykzf, bwkkb);
  
  or hckhgapwz(bzd, hwsnrmjco, jzesmyxx);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit jzesmyxx -> logic jzesmyxx
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign cdzgbiy = cdzgbiy;
  assign bzd = bzd;
endmodule: qy

module cz ();
  // Unpacked net declarations
  trireg logic qpicso [1:2][3:2][3:3][2:4];
  
  rl acfklwedc(.m(xjtotves), .uoudltkxiu(qpicso), .xuzuxibvx(uz));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic uz -> real xuzuxibvx
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xjtotves = 'bzx11x;
  assign uz = xjtotves;
  assign qpicso = '{'{'{'{'bz,'bx,'bz}},'{'{'bx1xzx,'bz10,'bx0}}},'{'{'{'b1z11,'b0xz,'bxz}},'{'{'b0z,'bx,'bx1xz}}}};
endmodule: cz



// Seed after: 15709616896345058517,16416852235058217195
