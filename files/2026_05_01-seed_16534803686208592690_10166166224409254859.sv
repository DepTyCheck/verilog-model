// Seed: 16534803686208592690,10166166224409254859

module kqerfm (input trior logic [0:3][4:2][3:1] mndqc);
  
  xnor dvufbzr(qzbpwedd, zkxk, mndqc);
  // warning: implicit conversion of port connection truncates from 36 to 1 bits
  //   trior logic [0:3][4:2][3:1] mndqc -> logic mndqc
  
  not tjtisbhpt(mndqc, zkxk);
  // warning: implicit conversion of port connection truncates from 36 to 1 bits
  //   trior logic [0:3][4:2][3:1] mndqc -> logic mndqc
  
  xnor fveaurb(ueoxrmsy, j, ueoxrmsy);
  
  or cogtfrxvqg(b, vshzdukcc, ueoxrmsy);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign mndqc = '{'{'{'b1x1,'bx,'b0},'{'b1,'b1z,'b01},'{'bxxxz1,'bzxx0,'bx1}},'{'{'b10,'b110xx,'bxxxz},'{'bz1zzx,'b001,'bz},'{'bzx00z,'bx,'b101}},'{'{'b1xz,'bz,'b111},'{'b00zz,'bx0xzx,'bx},'{'b1,'bzz,'b0x0}},'{'{'b0000z,'b0zzz,'b001xz},'{'b01,'bz1,'bz1x0},'{'bxz,'bx1zz,'b0}}};
  assign vshzdukcc = 'b10;
  assign qzbpwedd = zkxk;
  assign j = 'b1x111;
endmodule: kqerfm

module xjosle (output triand logic [3:1][2:3][3:0] aebyctvxgs [4:1]);
  
  xnor wgdecaw(ssthk, rcgca, n);
  
  and vwfakrbk(ssthk, rcgca, ckbma);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign n = ssthk;
  assign rcgca = 'b1x;
  assign ssthk = ssthk;
  assign aebyctvxgs = '{'{'{'{'b00z,'bzx1x,'bz,'bzzxx1},'{'b1x,'b1,'bx,'bzxxzx}},'{'{'b0x001,'bx0z1,'b0,'b1x01},'{'b0x1x,'b1x00,'bz0zz1,'b00}},'{'{'bz0001,'bx10,'b0x,'b0},'{'bz010,'b1xz,'bz01,'bz}}},'{'{'{'bzx0,'bz10z0,'b100z1,'bx01xx},'{'bxx11,'bz111,'b11z1,'b0zz}},'{'{'bx,'b0011,'b00,'bzx},'{'bzzxxz,'b1zz0z,'b0zz1x,'b1x111}},'{'{'bzx0,'b11,'b1z,'b11},'{'b0zz1,'b0,'bx,'b101}}},'{'{'{'b0x0x,'b0x,'b1xxz0,'bxx},'{'bxz00,'bx,'bx0,'b1}},'{'{'bx0xz0,'bzx10,'bzzz,'bx1zx0},'{'b0011,'b10,'bzzx1,'bx0x0z}},'{'{'b1,'b0z,'bx,'bz0zz0},'{'bx01xz,'bxz0z0,'b00x0x,'b0}}},'{'{'{'bz,'bx0z00,'b0zz0,'b0},'{'b1zz,'b011x,'b1,'bzz}},'{'{'bz1zz,'bzz,'bx,'b10},'{'b01z1z,'b0zzxx,'bz0,'b0x0x1}},'{'{'b11z,'bxxzx0,'bx0zz,'bzz1x},'{'b10,'b1,'bzx0,'bx}}}};
endmodule: xjosle

module tvpgzcrf ( input realtime xtrskzhava [4:1]
                , input bit gcv
                , output triand logic [1:2][4:0][4:2][4:0] pqspgr [4:3][4:1][0:1]
                , output logic [1:2][4:2][1:4][2:0] bw
                );
  // Unpacked net declarations
  triand logic [3:1][2:3][3:0] zlce [4:1];
  
  and ua(bw, xodzp, bw);
  // warning: implicit conversion of port connection truncates from 72 to 1 bits
  //   logic [1:2][4:2][1:4][2:0] bw -> logic bw
  //
  // warning: implicit conversion of port connection truncates from 72 to 1 bits
  //   logic [1:2][4:2][1:4][2:0] bw -> logic bw
  
  kqerfm frjrjdxxs(.mndqc(gcv));
  // warning: implicit conversion of port connection expands from 1 to 36 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit gcv -> trior logic [0:3][4:2][3:1] mndqc
  
  and fsztqr(xodzp, isindsnszh, bw);
  // warning: implicit conversion of port connection truncates from 72 to 1 bits
  //   logic [1:2][4:2][1:4][2:0] bw -> logic bw
  
  xjosle pdhsx(.aebyctvxgs(zlce));
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: tvpgzcrf

module huk (inout trireg logic [2:4][3:1][4:4] k [0:0]);
  // Unpacked net declarations
  triand logic [3:1][2:3][3:0] lzjsbflvvi [4:1];
  triand logic [1:2][4:0][4:2][4:0] yrazasdqff [4:3][4:1][0:1];
  realtime ri [4:1];
  
  tvpgzcrf srhyoavck(.xtrskzhava(ri), .gcv(vwotreiddg), .pqspgr(yrazasdqff), .bw(vwotreiddg));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic vwotreiddg -> bit gcv
  //
  // warning: implicit conversion of port connection expands from 1 to 72 bits
  //   wire logic vwotreiddg -> logic [1:2][4:2][1:4][2:0] bw
  
  xjosle qyvcr(.aebyctvxgs(lzjsbflvvi));
  
  
  // Single-driven assigns
  assign ri = ri;
  
  // Multi-driven assigns
  assign yrazasdqff = yrazasdqff;
endmodule: huk



// Seed after: 3088840793073689314,10166166224409254859
