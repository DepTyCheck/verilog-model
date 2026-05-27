// Seed: 16418945528543739513,14629700832272117749

module linnjg (output real tvi [3:0], input bit [2:4][2:3][1:2] cnlr [2:3], input reg [4:3] hpictvl, inout wor logic [1:3] qju [0:2]);
  
  xnor ogtn(dgxr, riu, tqjniv);
  
  nand zw(dgxr, bnxvoelhx, rxtfxcykc);
  
  not pg(v, hpictvl);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   reg [4:3] hpictvl -> logic hpictvl
  
  xor tulnpoerjt(bxmup, qs, yabtg);
  
  
  // Single-driven assigns
  assign tvi = '{'b1,'bzzx,'bz,'b1};
  
  // Multi-driven assigns
  assign yabtg = dgxr;
  assign tqjniv = 'b010;
  assign qju = qju;
  assign dgxr = 'bxzz0;
  assign bnxvoelhx = 'bx00zx;
endmodule: linnjg

module x (output reg [4:0][3:0] cxjsxkeog, output reg [0:2][3:3][2:2][2:0] prm);
  // Unpacked net declarations
  wor logic [1:3] sdkvbqet [0:2];
  bit [2:4][2:3][1:2] i [2:3];
  real appzqsb [3:0];
  
  xnor cbobhdnjmk(prm, cczlek, rk);
  // warning: implicit conversion of port connection truncates from 9 to 1 bits
  //   reg [0:2][3:3][2:2][2:0] prm -> logic prm
  
  xnor kvvlnzfomi(cxjsxkeog, rk, fsyu);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   reg [4:0][3:0] cxjsxkeog -> logic cxjsxkeog
  
  linnjg aphtk(.tvi(appzqsb), .cnlr(i), .hpictvl(ofd), .qju(sdkvbqet));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic ofd -> reg [4:3] hpictvl
  
  
  // Single-driven assigns
  assign i = i;
  
  // Multi-driven assigns
  assign rk = 'b0;
  assign fsyu = rk;
  assign cczlek = cczlek;
  assign sdkvbqet = '{'{'bz,'bzz11,'b10},'{'bz1z0,'b110,'bz1x0},'{'b101,'bz010z,'bx0}};
endmodule: x



// Seed after: 1091617244789863329,14629700832272117749
