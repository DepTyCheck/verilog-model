// Seed: 6903593206060436616,7477113606032372979

module mf (inout wor logic uny [0:2][3:1][3:1]);
  // Multi-driven assignments
  assign uny = '{'{'{'b1,'b0,'bx},'{'bx1,'bz0,'b1},'{'bzxx1z,'bxx0,'b1}},'{'{'bx0,'bx,'b0},'{'bz,'bx1x0,'b0},'{'b0,'b1z1zz,'bz10z1}},'{'{'bx,'b0x,'bx},'{'b1x11x,'bz1z0z,'bz},'{'b1,'b1,'bz}}};
endmodule: mf

module uv (output logic [2:3][4:1] yxfrcd, inout triand logic [2:4] lkvll [1:3][4:4][2:0]);
  xnor dtk(yxfrcd, vj, yxfrcd);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   logic [2:3][4:1] yxfrcd -> logic yxfrcd
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   logic [2:3][4:1] yxfrcd -> logic yxfrcd
  
  
  // Multi-driven assignments
  assign lkvll = lkvll;
  assign vj = 'bx;
endmodule: uv

module r ();
  // Unpacked net declarations
  triand logic [2:4] jabyuallrh [1:3][4:4][2:0];
  
  uv gchbq(.yxfrcd(icqy), .lkvll(jabyuallrh));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic icqy -> logic [2:3][4:1] yxfrcd
  
  xnor m(icqy, icqy, icqy);
  
  
  // Multi-driven assignments
  assign icqy = 'b1;
  assign icqy = 'b11z;
endmodule: r



// Seed after: 1065259006000628035,7477113606032372979
