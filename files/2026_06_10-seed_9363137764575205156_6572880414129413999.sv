// Seed: 9363137764575205156,6572880414129413999

module hluic ( inout supply1 logic [4:0][3:2][1:3] iuhvfo [0:2][1:2][1:3][4:2]
             , inout supply0 logic [1:2][0:4][2:4][4:2] pamthkxan
             , inout tri logic dwje [2:1][1:0][4:2][4:1]
             , inout triand logic ewliepcez
             );
  
  xnor sjuwwdori(nquvymx, nquvymx, i);
  
  xnor pgczarv(nquvymx, kp, nquvymx);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign ewliepcez = ewliepcez;
  assign iuhvfo = iuhvfo;
  assign i = nquvymx;
  assign pamthkxan = '{'{'{'{'bz1xx,'b1,'b11x},'{'b1,'b1xx,'b1x0z1},'{'bzx,'bz100,'bz0x1}},'{'{'bz11,'b00,'b0x0x1},'{'b0,'b1zz0z,'bx},'{'bx00x,'bzx,'bz111}},'{'{'bx0x00,'b1xz,'b0zz},'{'b0,'b1,'b110x1},'{'b01,'bxzx,'b0x}},'{'{'b0xx1,'b0x1z,'bx},'{'bz,'b00z,'bx},'{'b1z1,'b011z0,'b1}},'{'{'bz,'b01z1,'bxx},'{'bx,'bz110z,'b0011x},'{'bx1,'bz,'bxzx0}}},'{'{'{'bzz,'b1001,'b0x1z},'{'bxzzz,'bx,'bzx0x},'{'b0x,'b11x,'bxx}},'{'{'bxx0,'bz0z1,'bz},'{'bz0,'b0z1x,'b0zz1z},'{'bzz,'b0110z,'b0zzx}},'{'{'b111,'bzxxx,'b1},'{'bxxx,'bx1,'bx01},'{'bzzx,'b010x,'b0xz1}},'{'{'b0x0z0,'bxz,'b00},'{'b00x,'bzzz0z,'b11},'{'bz,'bz,'b1x0x}},'{'{'bxx010,'b011,'b1},'{'bz00x0,'b1,'b11x},'{'b01z1,'bz0,'b1xx11}}}};
  assign dwje = dwje;
endmodule: hluic

module pn ( output bit [3:4][0:3][4:4] xpabrxd [4:3]
          , output wire logic [4:3][3:1][1:1][0:2] qlyi [3:1][3:4][3:4][1:0]
          , output supply0 logic [3:0][1:4] gzjvljud [3:3][1:4]
          , inout supply0 logic [4:1] ths
          );
  
  
  
  // Single-driven assigns
  assign xpabrxd = '{'{'{'{'b00},'{'b10},'{'b01111},'{'b10001}},'{'{'b10010},'{'b0},'{'b11000},'{'b1}}},'{'{'{'b11011},'{'b0},'{'b001},'{'b10}},'{'{'b1},'{'b10111},'{'b0111},'{'b1}}}};
  
  // Multi-driven assigns
  assign qlyi = qlyi;
  assign ths = '{'bxx1z,'b1z,'bx1z,'bxx};
endmodule: pn

module wdyhvlbu (input tri1 logic [4:0] cg, input reg [3:3] rhoim [1:1]);
  // Unpacked net declarations
  bit [3:4][0:3][4:4] svrucduv [4:3];
  supply0 logic [3:0][1:4] pzfre [3:3][1:4];
  wire logic [4:3][3:1][1:1][0:2] nzbumie [3:1][3:4][3:4][1:0];
  bit [3:4][0:3][4:4] e [4:3];
  
  pn ewicg(.xpabrxd(e), .qlyi(nzbumie), .gzjvljud(pzfre), .ths(cg));
  // warning: implicit conversion of port connection truncates from 5 to 4 bits
  //   tri1 logic [4:0] cg -> supply0 logic [4:1] ths
  
  xnor jf(qehhv, qehhv, cg);
  // warning: implicit conversion of port connection truncates from 5 to 1 bits
  //   tri1 logic [4:0] cg -> logic cg
  
  nand lpykiskmt(qehhv, w, qewtrm);
  
  pn z(.xpabrxd(svrucduv), .qlyi(nzbumie), .gzjvljud(pzfre), .ths(qewtrm));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic qewtrm -> supply0 logic [4:1] ths
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign cg = cg;
  assign qehhv = w;
  assign w = qewtrm;
endmodule: wdyhvlbu

module mdqkqgauh (output supply0 logic [3:1][2:3][0:1][2:0] aegwjd [1:3][0:4][0:1], inout supply1 logic nnhk [1:0][4:3]);
  // Unpacked net declarations
  reg [3:3] iq [1:1];
  
  and qscjtm(qbmhtm, qbmhtm, qbmhtm);
  
  nand nfvxwdwlq(dn, qbmhtm, bla);
  
  wdyhvlbu cpcgdzlz(.cg(bla), .rhoim(iq));
  // warning: implicit conversion of port connection expands from 1 to 5 bits
  //   wire logic bla -> tri1 logic [4:0] cg
  
  nand cxtcvuhyt(qbmhtm, moyiumawu, vpqhhyuvl);
  
  
  // Single-driven assigns
  assign iq = iq;
  
  // Multi-driven assigns
  assign vpqhhyuvl = qbmhtm;
endmodule: mdqkqgauh



// Seed after: 7652146105684604190,6572880414129413999
