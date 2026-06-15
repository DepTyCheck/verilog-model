// Seed: 8599103134394071760,6310664522635210649

module t (input reg [0:2][1:4][0:2][4:3] cnfmtgaxtg, inout wor logic [2:4][0:1][0:2][0:1] assiupxi [3:1][2:2][1:0][0:1], input shortreal ymktyn);
  
  or tsibis(shxupcruvs, cnfmtgaxtg, oufczix);
  // warning: implicit conversion of port connection truncates from 72 to 1 bits
  //   reg [0:2][1:4][0:2][4:3] cnfmtgaxtg -> logic cnfmtgaxtg
  
  not raynjx(phmid, ymktyn);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal ymktyn -> logic ymktyn
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign shxupcruvs = 'b000x0;
endmodule: t

module a (input wire logic [3:0][1:4] uwafpxyocz);
  // Unpacked net declarations
  wor logic [2:4][0:1][0:2][0:1] xjx [3:1][2:2][1:0][0:1];
  
  and sbfzm(ylwwlpw, nxouhbjcf, nxouhbjcf);
  
  xnor s(wc, ltfnqocjio, lplagtz);
  
  t uwowx(.cnfmtgaxtg(zwsx), .assiupxi(xjx), .ymktyn(lplagtz));
  // warning: implicit conversion of port connection expands from 1 to 72 bits
  //   wire logic zwsx -> reg [0:2][1:4][0:2][4:3] cnfmtgaxtg
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic lplagtz -> shortreal ymktyn
  
  nand h(scbvsuopg, uufw, scbvsuopg);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign uufw = ltfnqocjio;
  assign ltfnqocjio = 'bxxz1z;
  assign uwafpxyocz = uwafpxyocz;
  assign wc = lplagtz;
  assign zwsx = 'b1;
endmodule: a

module wfembu (inout triand logic [2:2][4:1] ksvfovzr [0:2][0:4][1:0]);
  
  a tfvuve(.uwafpxyocz(kpxegxo));
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic kpxegxo -> wire logic [3:0][1:4] uwafpxyocz
  
  not c(zlaruphfea, mu);
  
  xor l(pjkkmbynq, flux, vmjzn);
  
  xnor xkprax(mu, xbljwlw, xbljwlw);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign mu = vmjzn;
  assign zlaruphfea = 'bz;
  assign ksvfovzr = ksvfovzr;
  assign flux = 'bxz;
  assign pjkkmbynq = 'b0;
endmodule: wfembu



// Seed after: 12534001901697829936,6310664522635210649
