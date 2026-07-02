// Seed: 2642148507080013567,13268557810146797515

module sg ( output reg [3:1][3:1][4:1] bdpswxvx [4:0]
          , output integer tlxmgrxgs [3:2][4:3]
          , output reg [2:4][4:2][3:2][2:4] cjtbx
          , inout wire logic gcbbdxkkjf [0:4]
          );
  not gter(hzhecf, cjtbx);
  // warning: implicit conversion of port connection truncates from 54 to 1 bits
  //   reg [2:4][4:2][3:2][2:4] cjtbx -> logic cjtbx
  
  not sqlh(hzhecf, cjtbx);
  // warning: implicit conversion of port connection truncates from 54 to 1 bits
  //   reg [2:4][4:2][3:2][2:4] cjtbx -> logic cjtbx
  
  xnor c(lg, urpm, jeosx);
  
  
  // Single-driven assignments
  assign tlxmgrxgs = '{'{'b00x1,'b100100110zz11z0xz11zxx1x10zxx1z0},'{'bxz0xx,'bz00zx0zz000x00xx0z11x011x110xzx0}};
  assign bdpswxvx = '{'{'bxxzzz111zzz1,'{'{'b10x1z,'bz,'b1,'bz},'{'b1,'bz1xzz,'b0,'b01001},'bxz1x},'{'b1xx,'{'bzx0z0,'b0z0,'bx,'b111},'{'b1,'bzx001,'b000,'b10zxz}}},'b0xz,'{'bxz00x1zzz00z,'bx1z01,'b1zx0x1101zxx},'{'{'{'bz0,'b00z,'bxz0,'b1},'b00z0,'{'b1011x,'b1,'bz,'b1z}},'{'{'b0,'b11,'bx,'b1},'b0z,'{'b0xx11,'b1,'b1x1z,'b1}},'{'b1xz,'b0,'bxxx1}},'{'bx0zxz,'b10xz,'{'{'bx,'bx,'b0x1,'b0},'bxzz1,'bx10z}}};
  assign cjtbx = 'bzxx;
  
  // Multi-driven assignments
  assign urpm = jeosx;
  assign urpm = 'bzz11x;
  assign lg = hzhecf;
  assign hzhecf = urpm;
endmodule: sg

module gw (input tri0 logic jnszkscq, inout wire logic [1:0][3:4] irdrnz);
  // Unpacked net declarations
  wire logic tlhrzmmifp [0:4];
  integer qjqmv [3:2][4:3];
  reg [3:1][3:1][4:1] dltbfswydj [4:0];
  
  sg ttifjpgcto(.bdpswxvx(dltbfswydj), .tlxmgrxgs(qjqmv), .cjtbx(jnszkscq), .gcbbdxkkjf(tlhrzmmifp));
  // warning: implicit conversion of port connection expands from 1 to 54 bits
  //   tri0 logic jnszkscq -> reg [2:4][4:2][3:2][2:4] cjtbx
  
  not f(vbro, bl);
  
  and jpx(bl, jnszkscq, eonvi);
  
  
  // Multi-driven assignments
  assign irdrnz = '{'{'b1,'b000z},'{'b010zz,'b0}};
  assign vbro = jnszkscq;
endmodule: gw

module ilm (output tri1 logic [2:3][4:3][3:2][3:1] rerj);
  // Unpacked net declarations
  wire logic idxff [0:4];
  integer radizpg [3:2][4:3];
  reg [3:1][3:1][4:1] dwizbj [4:0];
  
  gw y(.jnszkscq(rerj), .irdrnz(rerj));
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   tri1 logic [2:3][4:3][3:2][3:1] rerj -> tri0 logic jnszkscq
  //
  // warning: implicit conversion of port connection truncates from 24 to 4 bits
  //   tri1 logic [2:3][4:3][3:2][3:1] rerj -> wire logic [1:0][3:4] irdrnz
  
  xnor h(rerj, qaqiujez, qbtm);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   tri1 logic [2:3][4:3][3:2][3:1] rerj -> logic rerj
  
  xnor qczwupylw(uxwkgcabp, gagn, qbtm);
  
  sg w(.bdpswxvx(dwizbj), .tlxmgrxgs(radizpg), .cjtbx(qaqiujez), .gcbbdxkkjf(idxff));
  // warning: implicit conversion of port connection expands from 1 to 54 bits
  //   wire logic qaqiujez -> reg [2:4][4:2][3:2][2:4] cjtbx
  
  
  // Multi-driven assignments
  assign qbtm = 'bzzx1;
  assign idxff = idxff;
  assign rerj = '{'{'{'{'bz,'bx1xx,'b1},'{'b0,'bx,'bx00zz}},'{'bx1xx,'{'bx,'bx,'b0}}},'bx1xx};
endmodule: ilm



// Seed after: 13903380272254484827,13268557810146797515
