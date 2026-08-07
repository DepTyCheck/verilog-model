// Seed: 2901631759325899258,16789183692605617453

module pjfujwbicc ();
  nand wp(tip, tip, tip);
  
  
  // Multi-driven assignments
  assign tip = tip;
  assign tip = 'b1;
endmodule: pjfujwbicc

module obb (input trireg logic dukdym [2:2], output trior logic [1:2][2:0][1:0] cawtwiykir [3:1][2:1], input reg [1:4][3:2][2:4][4:1] jggcosty);
  pjfujwbicc yqq();
  
  xnor mtb(dtb, v, jggcosty);
  // warning: implicit conversion of port connection truncates from 96 to 1 bits
  //   reg [1:4][3:2][2:4][4:1] jggcosty -> logic jggcosty
  
  
  // Multi-driven assignments
  assign v = dtb;
  assign cawtwiykir = '{'{'{'{'bzz,'b0z,'b1xx},'{'b0z,'bx0,'bz10}},'{'bzz1110,'{'b0z,'b0x,'bx0zz}}},'{'{'b1xzz,'bzz1xz0},'{'b01z0,'{'bxx,'b001,'bz01z}}},'{'{'b00,'bx11xx1},'{'b11zz0z,'bx11x0}}};
endmodule: obb

module zldfbo (output tri1 logic [0:0][0:3] vdvsmomv, output bit [0:0][2:4] s [2:4]);
  // Unpacked net declarations
  trior logic [1:2][2:0][1:0] zq [3:1][2:1];
  trireg logic ft [2:2];
  
  obb gw(.dukdym(ft), .cawtwiykir(zq), .jggcosty(vdvsmomv));
  // warning: implicit conversion of port connection expands from 4 to 96 bits
  //   tri1 logic [0:0][0:3] vdvsmomv -> reg [1:4][3:2][2:4][4:1] jggcosty
  
  not b(jz, wxigs);
  
  
  // Single-driven assignments
  assign s = '{'{'{'b1,'b1,'b110}},'{'{'b0,'b0,'b1}},'b000};
  
  // Multi-driven assignments
  assign jz = jz;
endmodule: zldfbo

module aybtaoy (input logic krsyzxhz);
  // Unpacked net declarations
  bit [0:0][2:4] sv [2:4];
  
  zldfbo phsyksgj(.vdvsmomv(ornvk), .s(sv));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic ornvk -> tri1 logic [0:0][0:3] vdvsmomv
  
  
  // Multi-driven assignments
  assign ornvk = krsyzxhz;
endmodule: aybtaoy



// Seed after: 10818585642018627200,16789183692605617453
