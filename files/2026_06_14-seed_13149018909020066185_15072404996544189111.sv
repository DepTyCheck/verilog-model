// Seed: 13149018909020066185,15072404996544189111

module djvp ( input real q
            , inout tri logic [3:0][0:4][0:1] egvpgfxmub [1:3][4:1][0:4][0:3]
            , input reg zw [1:3][2:0][4:2]
            , output reg [0:3][1:0][2:4] rpgsxsepb [4:4]
            );
  
  xnor gqzi(co, qlgp, q);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real q -> logic q
  
  not x(qlgp, boxevadxj);
  
  nand me(f, r, co);
  
  and rhgujh(co, rfjohtbc, q);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real q -> logic q
  
  
  // Single-driven assigns
  assign rpgsxsepb = rpgsxsepb;
  
  // Multi-driven assigns
endmodule: djvp

module e (inout wire logic [0:3][1:1][1:3][1:3] lousaletr, output realtime frpkiv, input reg [3:0][3:4][1:3][4:1] b, input bit rikkijys);
  // Unpacked net declarations
  reg [0:3][1:0][2:4] yfinlam [4:4];
  reg ylf [1:3][2:0][4:2];
  tri logic [3:0][0:4][0:1] ndvzatce [1:3][4:1][0:4][0:3];
  
  djvp gc(.q(rikkijys), .egvpgfxmub(ndvzatce), .zw(ylf), .rpgsxsepb(yfinlam));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit rikkijys -> real q
  
  nand anotdz(hxqycoebtk, hxqycoebtk, rikkijys);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit rikkijys -> logic rikkijys
  
  
  // Single-driven assigns
  assign frpkiv = 'bxxx11;
  assign ylf = '{'{'{'b1,'b1zx,'b1},'{'b1,'bzx001,'bx1},'{'bx,'b11,'b0z10z}},'{'{'bx01x,'b0zx10,'b0z10},'{'bxz0,'bzz,'b111},'{'b1,'b0zz01,'b00z}},'{'{'bzx,'bz1z1z,'bxxzz},'{'b1101,'b01,'b0x1x},'{'bzzx01,'bzxx10,'bxx0z0}}};
  
  // Multi-driven assigns
endmodule: e



// Seed after: 11570842377381277875,15072404996544189111
