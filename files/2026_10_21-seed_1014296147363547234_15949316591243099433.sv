// Seed: 1014296147363547234,15949316591243099433

module wammehku (output supply0 logic hbmces [4:2][2:2][0:1], input bit [4:3][0:1][2:1] di);
  xnor ke(q, aqowunnsvq, di);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:3][0:1][2:1] di -> logic di
  
  xnor siunoalb(q, xyd, di);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:3][0:1][2:1] di -> logic di
  
  xnor lrcq(q, xyd, di);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:3][0:1][2:1] di -> logic di
  
  
  // Multi-driven assignments
  assign aqowunnsvq = q;
  assign xyd = aqowunnsvq;
  assign hbmces = '{'{'{'bz,'bx}},'{'{'b1,'bx00}},'{'{'b1z100,'b1}}};
endmodule: wammehku

module cgs (input shortreal rm);
  // Unpacked net declarations
  supply0 logic shyb [4:2][2:2][0:1];
  
  xor evarb(saqiyx, rm, h);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal rm -> logic rm
  
  wammehku kspdsvfy(.hbmces(shyb), .di(saqiyx));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic saqiyx -> bit [4:3][0:1][2:1] di
  
  
  // Multi-driven assignments
  assign shyb = '{'{'{'bxz1,'b0zz}},'{'{'bx,'bz1x1}},'{'{'bxz,'b1}}};
  assign saqiyx = 'bx;
endmodule: cgs

module cnau (output reg [0:2] cymo, output uwire logic p [0:1][3:2][1:4][3:3]);
  cgs gek(.rm(cymo));
  // warning: implicit conversion of port connection expands from 3 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   reg [0:2] cymo -> shortreal rm
  
  xnor uqrpgscmbq(yhyme, hgwnun, sdevxz);
  
  
  // Multi-driven assignments
  assign yhyme = sdevxz;
  assign hgwnun = hgwnun;
endmodule: cnau



// Seed after: 6204587084615884034,15949316591243099433
