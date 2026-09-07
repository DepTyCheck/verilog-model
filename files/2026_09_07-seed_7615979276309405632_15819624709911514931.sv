// Seed: 7615979276309405632,15819624709911514931

module wrpdojno ( output tri0 logic [2:0][0:2] oaaex [1:4]
                , output tri logic [1:3][4:0][3:0][2:2] onxlvnuo [1:4][0:1][3:2][0:0]
                , inout wire logic fnkavbno [2:2][0:4]
                , output logic [3:0][3:0][3:4][2:1] korctwb
                );
  nand twoji(korctwb, hytofeif, nyfmslramn);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   logic [3:0][3:0][3:4][2:1] korctwb -> logic korctwb
  
  xnor kjdgbahejd(nyfmslramn, hytofeif, korctwb);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   logic [3:0][3:0][3:4][2:1] korctwb -> logic korctwb
  
  xnor wnundza(uh, ddoqcu, cij);
  
  
  // Multi-driven assignments
  assign hytofeif = 'bz;
  assign fnkavbno = fnkavbno;
  assign uh = nyfmslramn;
endmodule: wrpdojno

module jjusa (output logic [4:0][4:1][4:4][3:1] bovkrhtvl);
  // Unpacked net declarations
  wire logic mf [2:2][0:4];
  tri logic [1:3][4:0][3:0][2:2] lokk [1:4][0:1][3:2][0:0];
  tri0 logic [2:0][0:2] qqjb [1:4];
  
  xor u(bovkrhtvl, g, bovkrhtvl);
  // warning: implicit conversion of port connection truncates from 60 to 1 bits
  //   logic [4:0][4:1][4:4][3:1] bovkrhtvl -> logic bovkrhtvl
  //
  // warning: implicit conversion of port connection truncates from 60 to 1 bits
  //   logic [4:0][4:1][4:4][3:1] bovkrhtvl -> logic bovkrhtvl
  
  wrpdojno ruqdwugt(.oaaex(qqjb), .onxlvnuo(lokk), .fnkavbno(mf), .korctwb(g));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  //   wire logic g -> logic [3:0][3:0][3:4][2:1] korctwb
  
  xnor krnbqpzlf(g, g, bovkrhtvl);
  // warning: implicit conversion of port connection truncates from 60 to 1 bits
  //   logic [4:0][4:1][4:4][3:1] bovkrhtvl -> logic bovkrhtvl
  
  
  // Multi-driven assignments
  assign lokk = lokk;
  assign mf = mf;
  assign g = 'b1;
endmodule: jjusa

module jnmjr ();
  // Unpacked net declarations
  wire logic xg [2:2][0:4];
  tri logic [1:3][4:0][3:0][2:2] yhebchiwv [1:4][0:1][3:2][0:0];
  tri0 logic [2:0][0:2] kwbx [1:4];
  
  wrpdojno eekmoprx(.oaaex(kwbx), .onxlvnuo(yhebchiwv), .fnkavbno(xg), .korctwb(zflpmexumo));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  //   wire logic zflpmexumo -> logic [3:0][3:0][3:4][2:1] korctwb
  
  nand suhuxl(v, zflpmexumo, srqpim);
  
  and glu(rmnvkbg, b, imk);
  
  
  // Multi-driven assignments
  assign rmnvkbg = 'b1;
  assign kwbx = kwbx;
  assign v = zflpmexumo;
endmodule: jnmjr

module gj (input logic loikw [0:2], inout trior logic [0:3][4:4][2:2][3:4] lkowilwh [3:1][0:2]);
  jjusa thlld(.bovkrhtvl(aakom));
  // warning: implicit conversion of port connection expands from 1 to 60 bits
  //   wire logic aakom -> logic [4:0][4:1][4:4][3:1] bovkrhtvl
  
  jnmjr irr();
  
  xnor lwvkpmhkw(aakom, aquyrl, rkj);
  
  
  // Multi-driven assignments
  assign lkowilwh = '{'{'b1xxx1xx1,'{'bxx,'bx0,'b0x,'{'b1x}},'{'{'b0x},'{'b00z},'{'b0},'b0z}},'{'{'b1zxz,'{'b1x},'bxz,'bz0},'{'b00,'{'b0xxx},'bxz,'bxz},'{'b1z001,'{'b110},'b00,'{'bxz}}},'{'{'bx0,'{'b0z00x},'{'bx0x},'bzz},'{'bzz,'{'b1x},'bzx,'bzx},'{'{'b1z},'bx1,'{'b0z},'{'b01}}}};
  assign aquyrl = aakom;
  assign lkowilwh = lkowilwh;
endmodule: gj



// Seed after: 1810494866886866734,15819624709911514931
