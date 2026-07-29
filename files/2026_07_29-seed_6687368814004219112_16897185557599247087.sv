// Seed: 6687368814004219112,16897185557599247087

module fgmc (input reg [2:0][2:3] kbkf);
  nand acuh(km, kbkf, kbkf);
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  //   reg [2:0][2:3] kbkf -> logic kbkf
  //
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  //   reg [2:0][2:3] kbkf -> logic kbkf
  
  
  // Multi-driven assignments
  assign km = 'bz;
  assign km = 'bx;
  assign km = 'bz;
  assign km = 'b0z0;
endmodule: fgmc

module ps (input wire logic [0:1][4:1][0:1][1:4] izwdcevtx);
  nand byytlqli(izwdcevtx, upjuqoy, upjuqoy);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   wire logic [0:1][4:1][0:1][1:4] izwdcevtx -> logic izwdcevtx
  
  and ry(upjuqoy, upjuqoy, upjuqoy);
  
  fgmc xbpxu(.kbkf(izwdcevtx));
  // warning: implicit conversion of port connection truncates from 64 to 6 bits
  //   wire logic [0:1][4:1][0:1][1:4] izwdcevtx -> reg [2:0][2:3] kbkf
  
  
  // Multi-driven assignments
  assign izwdcevtx = 'b01zz1z0z011zxz1xxx10000xxz10z01x10xzz0zz0011x0zxz0x0z0x00xxxxz11;
endmodule: ps



// Seed after: 6855229378423651552,16897185557599247087
