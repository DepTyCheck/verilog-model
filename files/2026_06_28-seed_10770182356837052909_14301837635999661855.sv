// Seed: 10770182356837052909,14301837635999661855

module xpeofi ( output tri0 logic [4:2][3:0][0:2] up
              , output trior logic [3:0][3:1][4:1] gao
              , output triand logic [0:3][0:2][3:4][0:1] obu [3:0][2:4][2:1][3:2]
              , inout triand logic [2:3][4:2][2:3][4:1] pplqigu [0:4][0:1][3:3]
              );
  nand mpwnx(vgljanfnz, gao, up);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   trior logic [3:0][3:1][4:1] gao -> logic gao
  //
  // warning: implicit conversion of port connection truncates from 36 to 1 bits
  //   tri0 logic [4:2][3:0][0:2] up -> logic up
  
  or sj(vgljanfnz, xlyz, vgljanfnz);
  
  xor he(amfm, mbvnif, gao);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   trior logic [3:0][3:1][4:1] gao -> logic gao
  
  xnor ju(axnzucjc, amfm, tvxmtsxfdm);
  
endmodule: xpeofi

module t (input time l, input wand logic [3:0] os, inout tri0 logic [3:2] v [3:3], inout trireg logic [2:4][3:2][0:2] xehrty [0:4][0:0][1:1]);
  // Unpacked net declarations
  triand logic [2:3][4:2][2:3][4:1] mmkilmccs [0:4][0:1][3:3];
  triand logic [0:3][0:2][3:4][0:1] wxiyrtpk [3:0][2:4][2:1][3:2];
  
  xpeofi nlgl(.up(os), .gao(os), .obu(wxiyrtpk), .pplqigu(mmkilmccs));
  // warning: implicit conversion of port connection expands from 4 to 36 bits
  //   wand logic [3:0] os -> tri0 logic [4:2][3:0][0:2] up
  //
  // warning: implicit conversion of port connection expands from 4 to 48 bits
  //   wand logic [3:0] os -> trior logic [3:0][3:1][4:1] gao
  
  xnor xpytgtg(os, nhuwno, l);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   wand logic [3:0] os -> logic os
  //
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   time l -> logic l
  
  not pzlgutjjlf(rok, rltd);
  
  xnor m(dzb, ovmhuiagr, rok);
  
  
  // Multi-driven assignments
  assign mmkilmccs = '{'{'{'{'bx0111xx0x11z1101zxzz101z,'b1xz0x01z101zxz0z0xzz01x0}},'{'b1x1x0111xz0zx10z1z0x01x0xzz1z0z00x000zz1000xzz10}},'{'{'{'b0zxzx,'bxzxz}},'{'bxzzx}},'{'{'{'b1xxzx,'b0010}},'{'{'b11x1xxz1xx1x10zx0xzzzxx1,'bx110zz11xz1z00xx00zx01x1}}},'{'{'{'b00,'b0zz00}},'{'b0}},'{'{'b11xx011xzz0xxxz10z01xx0z00x0xzzz0z1xz1x1zzz11001},'{'b1z}}};
  assign xehrty = xehrty;
  assign xehrty = '{'{'{'{'b11z1x1,'bxx,'bxx1}}},'{'{'bx11x}},'{'{'b0}},'{'{'{'bx,'bz0x0zx,'bz11}}},'{'{'bz}}};
endmodule: t



// Seed after: 5704607960138853302,14301837635999661855
