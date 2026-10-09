// Seed: 11218042046244419910,9251509950486119111

module wejgsn (input tri1 logic d [3:3][1:0][2:0][2:2], output logic [1:0][2:2][0:3] h);
  xnor bvmcfhaxbu(h, h, h);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   logic [1:0][2:2][0:3] h -> logic h
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   logic [1:0][2:2][0:3] h -> logic h
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   logic [1:0][2:2][0:3] h -> logic h
  
  
  // Multi-driven assignments
  assign d = '{'{'{'{'bz},'{'b0},'{'b10z1x}},'{'{'b0},'{'b0},'{'bzzx0x}}}};
  assign d = '{'{'{'{'b0},'{'b1},'{'bx}},'{'{'b1},'{'bzxz0},'{'b0x01z}}}};
  assign d = '{'{'{'{'b0},'{'b1z0},'{'b00}},'{'{'b0x1},'{'bx},'{'b10z0z}}}};
  assign d = '{'{'{'{'bx},'{'b0},'{'b1}},'{'{'b1z0},'{'bz110x},'{'b1}}}};
endmodule: wejgsn

module vxi (output uwire logic qnxqk [0:3][0:2][4:0]);
  // Unpacked net declarations
  tri1 logic ewnkhnrvy [3:3][1:0][2:0][2:2];
  
  xnor ziegrlmk(ojgycub, hjj, ojgycub);
  
  wejgsn lcfbpyv(.d(ewnkhnrvy), .h(hjj));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  //   wire logic hjj -> logic [1:0][2:2][0:3] h
  
  and fljefyzqg(ojgycub, qkudryld, ojgycub);
  
  xor su(rbyp, qppmjbpkws, gtascgefbe);
  
endmodule: vxi



// Seed after: 14342568370084466429,9251509950486119111
