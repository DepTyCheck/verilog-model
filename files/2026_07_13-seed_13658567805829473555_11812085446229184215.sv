// Seed: 13658567805829473555,11812085446229184215

module vfq (inout triand logic bl [2:4], input integer mabukrf [3:0], output wire logic [4:4][1:3][0:1][3:2] zsnkooa);
  nand vjanxvcmb(jcmc, zsnkooa, opqhwbtoy);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   wire logic [4:4][1:3][0:1][3:2] zsnkooa -> logic zsnkooa
  
  and n(zsnkooa, e, myrvrko);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   wire logic [4:4][1:3][0:1][3:2] zsnkooa -> logic zsnkooa
  
  xnor a(uph, opqhwbtoy, ypn);
  
  xnor lqqwmvrng(kzgp, bcmn, opqhwbtoy);
  
  
  // Multi-driven assignments
  assign zsnkooa = '{'{'{'{'b1,'b10x1},'bx1zz0},'b0x010,'{'b00,'{'b1,'b1}}}};
  assign jcmc = kzgp;
endmodule: vfq

module kjofymsur (inout trireg logic [3:3][0:4] abi, inout supply1 logic o);
  // Unpacked net declarations
  integer jysc [3:0];
  triand logic vpuohn [2:4];
  
  nand yewzm(fpnktb, eqjrhrs, fpnktb);
  
  vfq sihprbofmg(.bl(vpuohn), .mabukrf(jysc), .zsnkooa(o));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   supply1 logic o -> wire logic [4:4][1:3][0:1][3:2] zsnkooa
  
  
  // Single-driven assignments
  assign jysc = jysc;
  
  // Multi-driven assignments
  assign o = 'bxxzzz;
endmodule: kjofymsur

module vj ();
  // Unpacked net declarations
  integer nzpkuv [3:0];
  integer wj [3:0];
  triand logic yxcn [2:4];
  
  vfq ra(.bl(yxcn), .mabukrf(wj), .zsnkooa(srunp));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic srunp -> wire logic [4:4][1:3][0:1][3:2] zsnkooa
  
  vfq nqwbvlnz(.bl(yxcn), .mabukrf(nzpkuv), .zsnkooa(dt));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic dt -> wire logic [4:4][1:3][0:1][3:2] zsnkooa
  
  xnor dlmhlgaxsm(edpnmmxm, srunp, kpbhifr);
  
  
  // Single-driven assignments
  assign nzpkuv = '{'b1zxzxzxxz1x110xxz00x1z00xx11xzxz,'b0xz0zz000zz10111z11zx0z10000x0zx,'bx0xx1xz0xxx00z1011z01xx1z0z0z1xx,'bx1zx};
  assign wj = '{'bzx011,'b0x,'b0z,'b011zxzx111100xxz1xxx10zxx111xx0x};
  
  // Multi-driven assignments
  assign yxcn = '{'bz,'b0,'bxxz1z};
  assign dt = kpbhifr;
  assign edpnmmxm = kpbhifr;
  assign dt = 'bz010;
endmodule: vj



// Seed after: 15011200230595464134,11812085446229184215
