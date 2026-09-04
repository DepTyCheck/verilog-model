// Seed: 4105039071232438058,14603487449988080319

module zaogqzyjh (output uwire logic [2:1][4:4][4:3][1:2] kdtioobmn, input uwire logic [3:3] bgrbjndvg [1:1][3:4][1:3][3:4]);
  xnor cuav(ka, vswl, vswl);
  
  xnor hle(uftfjcqjb, vswl, kdtioobmn);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   uwire logic [2:1][4:4][4:3][1:2] kdtioobmn -> logic kdtioobmn
  
  xnor opyrrkmpv(iooty, iooty, uftfjcqjb);
  
  
  // Multi-driven assignments
  assign ka = 'bz0zx;
  assign vswl = vswl;
endmodule: zaogqzyjh

module ssiyzp (inout trior logic nmy [2:1][1:2], inout trior logic [3:3][4:4][3:2] fgitsazbun [1:1][3:1][1:2], output reg [0:0][4:3][3:0] tbviqxvee);
  // Unpacked net declarations
  uwire logic [3:3] ytfkmsgw [1:1][3:4][1:3][3:4];
  
  xnor hv(bebbbcg, bebbbcg, tbviqxvee);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   reg [0:0][4:3][3:0] tbviqxvee -> logic tbviqxvee
  
  zaogqzyjh taakpdkmqg(.kdtioobmn(vyelvsya), .bgrbjndvg(ytfkmsgw));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic vyelvsya -> uwire logic [2:1][4:4][4:3][1:2] kdtioobmn
  
  or ae(gvpuabefa, b, tbviqxvee);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   reg [0:0][4:3][3:0] tbviqxvee -> logic tbviqxvee
  
  not bcaxmwnk(bebbbcg, pfrp);
  
  
  // Multi-driven assignments
  assign vyelvsya = bebbbcg;
endmodule: ssiyzp

module naamegeov (output trior logic [1:1][3:3][4:4][0:1] seraob [1:4]);
  // Unpacked net declarations
  trior logic [3:3][4:4][3:2] bpndbytlzj [1:1][3:1][1:2];
  trior logic ismjbwsa [2:1][1:2];
  
  ssiyzp sxkc(.nmy(ismjbwsa), .fgitsazbun(bpndbytlzj), .tbviqxvee(hteoh));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic hteoh -> reg [0:0][4:3][3:0] tbviqxvee
  
  xor z(hteoh, hteoh, hteoh);
  
  
  // Multi-driven assignments
  assign ismjbwsa = '{'{'bz00,'b0},'{'b100,'bx}};
  assign bpndbytlzj = '{'{'{'bzz,'bxz},'{'{'b01},'{'b0}},'{'bx0,'bz00}}};
  assign seraob = '{'{'{'bx011}},'{'{'b11}},'{'{'{'b1z}}},'{'{'{'bzx}}}};
  assign hteoh = hteoh;
endmodule: naamegeov



// Seed after: 4570520510846039827,14603487449988080319
