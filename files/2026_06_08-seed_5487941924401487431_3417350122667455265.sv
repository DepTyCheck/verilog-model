// Seed: 5487941924401487431,3417350122667455265

module ec (output logic [2:4][1:3][1:0] m, output shortreal g, input tri1 logic [2:3][2:4][4:0] xr, inout tri1 logic wko [1:2][1:4][3:2]);
  
  nand o(osxpad, epdwbqdpcb, osxpad);
  
  and ssbpmpkhyn(uesmxjkg, g, njsaigfhc);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal g -> logic g
  
  xnor hk(ekkiqy, lgbbm, yblrznxc);
  
  
  // Single-driven assigns
  assign g = 'b10;
  
  // Multi-driven assigns
  assign wko = '{'{'{'b0xz,'b00},'{'b1z,'b1xz},'{'bz1,'b0010},'{'b0xxx1,'bx0x}},'{'{'b01z,'bxx1x0},'{'bz,'b1zz},'{'bzxxx,'b11},'{'b1xx,'bx1xz}}};
  assign xr = '{'{'{'bx0,'bx0x,'bz1,'bx1,'b00xzx},'{'b0000,'b11,'bx010z,'bzz10,'b1zxzx},'{'b00,'b1xxz,'bz0,'b010xx,'bz}},'{'{'b00zxz,'bz0,'bzx1z,'b0xx,'b1x100},'{'bz,'bxzxz,'b1z001,'b1,'b11111},'{'b1z,'bzxx0z,'b1,'b0,'b11x0}}};
  assign ekkiqy = 'b0zz;
endmodule: ec

module cyuaxhtp (input bit [4:1] hvwyamxtoa, input tri logic [0:4][4:3] wnn [1:1][1:3][0:0][3:0]);
  // Unpacked net declarations
  tri1 logic gbbtqihela [1:2][1:4][3:2];
  
  ec mbpfqqkh(.m(oanrmm), .g(kqqbunbtq), .xr(hvwyamxtoa), .wko(gbbtqihela));
  // warning: implicit conversion of port connection expands from 1 to 18 bits
  //   wire logic oanrmm -> logic [2:4][1:3][1:0] m
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic kqqbunbtq -> shortreal g
  //
  // warning: implicit conversion of port connection expands from 4 to 30 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:1] hvwyamxtoa -> tri1 logic [2:3][2:4][4:0] xr
  
  xnor snzxzjake(uir, hvwyamxtoa, hvwyamxtoa);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:1] hvwyamxtoa -> logic hvwyamxtoa
  //
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:1] hvwyamxtoa -> logic hvwyamxtoa
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign uir = 'b0xz;
  assign kqqbunbtq = oanrmm;
  assign oanrmm = oanrmm;
  assign wnn = wnn;
endmodule: cyuaxhtp

module r (input logic [1:1][0:0] xkzj [1:1][0:3], inout wor logic [0:3][0:2] ix [2:1][0:3][1:4], inout trior logic [0:1][4:4] z);
  // Unpacked net declarations
  tri logic [0:4][4:3] ytnosbqwni [1:1][1:3][0:0][3:0];
  
  cyuaxhtp ejmgw(.hvwyamxtoa(ogt), .wnn(ytnosbqwni));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic ogt -> bit [4:1] hvwyamxtoa
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign ix = ix;
  assign ytnosbqwni = ytnosbqwni;
endmodule: r



// Seed after: 2494653363962376007,3417350122667455265
