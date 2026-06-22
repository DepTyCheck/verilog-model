// Seed: 1374640554465907511,10790896317602023939

module edj ( input tri0 logic [1:3][1:4] hnbsvujm
           , output bit [3:1][3:2][0:3] iyibmwxqot
           , input reg [1:4][4:2][3:1][0:3] hvvxmtfoqz
           , input supply0 logic [1:3][3:0] qpfl [1:3]
           );
  xnor afgzxht(e, pz, e);
  
  xor injlti(vxhm, plrao, hnbsvujm);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   tri0 logic [1:3][1:4] hnbsvujm -> logic hnbsvujm
  
  xor bfxwkkw(iyibmwxqot, hvvxmtfoqz, hvvxmtfoqz);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:1][3:2][0:3] iyibmwxqot -> logic iyibmwxqot
  //
  // warning: implicit conversion of port connection truncates from 144 to 1 bits
  //   reg [1:4][4:2][3:1][0:3] hvvxmtfoqz -> logic hvvxmtfoqz
  //
  // warning: implicit conversion of port connection truncates from 144 to 1 bits
  //   reg [1:4][4:2][3:1][0:3] hvvxmtfoqz -> logic hvvxmtfoqz
  
  not igjl(vx, yqoez);
  
  
  // Multi-driven assignments
  assign plrao = e;
endmodule: edj

module fokxpbf ();
  // Unpacked net declarations
  supply0 logic [1:3][3:0] gwc [1:3];
  
  edj ilxsrbma(.hnbsvujm(c), .iyibmwxqot(arnvv), .hvvxmtfoqz(c), .qpfl(gwc));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic c -> tri0 logic [1:3][1:4] hnbsvujm
  //
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic arnvv -> bit [3:1][3:2][0:3] iyibmwxqot
  //
  // warning: implicit conversion of port connection expands from 1 to 144 bits
  //   wire logic c -> reg [1:4][4:2][3:1][0:3] hvvxmtfoqz
  
  
  // Multi-driven assignments
  assign arnvv = c;
  assign gwc = '{'{'{'bxx0x,'b0,'bz,'b0},'{'bx,'bz,'bz1x1,'bz},'{'bzz,'bz,'b0,'b0}},'bx00,'{'bxz0z0,'{'bx,'bx,'bx1z,'bx},'{'b1,'bxzx,'bx,'b0x0}}};
endmodule: fokxpbf



// Seed after: 9685809169031741039,10790896317602023939
