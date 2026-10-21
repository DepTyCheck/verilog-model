// Seed: 8826481825743886091,15949316591243099433

module rlmdsthrwl (output bit [0:0] smocqrjx, output reg [1:1][4:1][1:3][3:4] sxzvujxl, inout tri0 logic [4:2][4:1][0:2][3:1] jxmn [4:0]);
  xnor afe(sxzvujxl, sxzvujxl, g);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   reg [1:1][4:1][1:3][3:4] sxzvujxl -> logic sxzvujxl
  //
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   reg [1:1][4:1][1:3][3:4] sxzvujxl -> logic sxzvujxl
  
  
  // Single-driven assignments
  assign smocqrjx = '{'b0};
endmodule: rlmdsthrwl

module bgxzc (output tri1 logic [2:1][2:3] dlh [2:4][4:0], input logic [1:3][1:3][4:2][4:3] wj, output int cwwzmapwwi [0:2]);
  // Unpacked net declarations
  tri0 logic [4:2][4:1][0:2][3:1] qgnikff [4:0];
  
  xor cdtg(scb, scb, cfmwo);
  
  rlmdsthrwl cornjbbmq(.smocqrjx(scb), .sxzvujxl(scb), .jxmn(qgnikff));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic scb -> bit [0:0] smocqrjx
  //
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic scb -> reg [1:1][4:1][1:3][3:4] sxzvujxl
  
  xnor qexrav(u, wbg, cfmwo);
  
  
  // Single-driven assignments
  assign cwwzmapwwi = cwwzmapwwi;
  
  // Multi-driven assignments
  assign u = 'bz0x0;
  assign u = 'b1x;
  assign cfmwo = 'b1;
endmodule: bgxzc

module fwjq ( inout trireg logic [4:4] johy [3:4][1:4]
            , inout tri logic [2:2][1:4][3:4] fk
            , output tri0 logic [3:1][3:0] z
            , inout trireg logic [3:2][4:1][4:2][4:1] q [0:1][0:2][1:1]
            );
  // Unpacked net declarations
  int bzsu [0:2];
  tri1 logic [2:1][2:3] jl [2:4][4:0];
  int rpbdbvl [0:2];
  tri1 logic [2:1][2:3] c [2:4][4:0];
  
  bgxzc gilor(.dlh(c), .wj(qs), .cwwzmapwwi(rpbdbvl));
  // warning: implicit conversion of port connection expands from 1 to 54 bits
  //   wire logic qs -> logic [1:3][1:3][4:2][4:3] wj
  
  xor gbvmjrerc(qs, rge, z);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   tri0 logic [3:1][3:0] z -> logic z
  
  bgxzc as(.dlh(jl), .wj(qs), .cwwzmapwwi(bzsu));
  // warning: implicit conversion of port connection expands from 1 to 54 bits
  //   wire logic qs -> logic [1:3][1:3][4:2][4:3] wj
  
  xnor jcecfzt(qs, g, uimrmtrws);
  
  
  // Multi-driven assignments
  assign jl = c;
  assign q = q;
endmodule: fwjq

module djpwqt (output byte ga [0:2], output bit [1:4][3:0][2:2][0:3] s);
  // Unpacked net declarations
  trireg logic [3:2][4:1][4:2][4:1] ckr [0:1][0:2][1:1];
  trireg logic [4:4] uo [3:4][1:4];
  
  xnor q(zgmkmqvehx, l, s);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:4][3:0][2:2][0:3] s -> logic s
  
  fwjq cseivxtwk(.johy(uo), .fk(uhhgf), .z(uhhgf), .q(ckr));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic uhhgf -> tri logic [2:2][1:4][3:4] fk
  //
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic uhhgf -> tri0 logic [3:1][3:0] z
  
  
  // Multi-driven assignments
  assign zgmkmqvehx = zgmkmqvehx;
endmodule: djpwqt



// Seed after: 8103083266357457046,15949316591243099433
