// Seed: 17189179814449107895,599996410845877199

module fqfeogib ( input int ofvwjcx
                , inout wire logic [1:2][1:2][0:4] utur
                , output tri1 logic [0:1][1:4] aelbeuqer [2:1]
                , output bit [4:1][3:1][2:2] nnmbsugm [0:1]
                );
  xor lozpcx(phqd, ocdxovzwe, ofvwjcx);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   int ofvwjcx -> logic ofvwjcx
  
  
  // Single-driven assignments
  assign nnmbsugm = '{'{'{'{'b1},'{'b1},'{'b00110}},'b00,'b1101,'{'{'b0},'b1,'{'b1}}},'b11};
  
  // Multi-driven assignments
  assign aelbeuqer = '{'bx1zxx000,'b0x11z0zx};
  assign aelbeuqer = aelbeuqer;
endmodule: fqfeogib

module eoxenqkwbo (input logic [4:2][1:1] edkdllsfv, inout tri1 logic [0:4][3:1] yfatkken [3:0][2:0][3:1][0:1], input triand logic [4:2] godxfhunt);
  // Unpacked net declarations
  bit [4:1][3:1][2:2] gsjjuxivnh [0:1];
  tri1 logic [0:1][1:4] lusqvxscl [2:1];
  
  xnor gd(godxfhunt, mqstx, mqstx);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   triand logic [4:2] godxfhunt -> logic godxfhunt
  
  fqfeogib ml(.ofvwjcx(enfzl), .utur(godxfhunt), .aelbeuqer(lusqvxscl), .nnmbsugm(gsjjuxivnh));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic enfzl -> int ofvwjcx
  //
  // warning: implicit conversion of port connection expands from 3 to 20 bits
  //   triand logic [4:2] godxfhunt -> wire logic [1:2][1:2][0:4] utur
  
endmodule: eoxenqkwbo

module v (output tri0 logic [3:4] yf [3:0]);
  // Unpacked net declarations
  tri1 logic [0:4][3:1] nhg [3:0][2:0][3:1][0:1];
  bit [4:1][3:1][2:2] fgwqnzxs [0:1];
  tri1 logic [0:1][1:4] ktsjnvunc [2:1];
  
  fqfeogib difi(.ofvwjcx(dcehy), .utur(fnlenutj), .aelbeuqer(ktsjnvunc), .nnmbsugm(fgwqnzxs));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic dcehy -> int ofvwjcx
  //
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic fnlenutj -> wire logic [1:2][1:2][0:4] utur
  
  eoxenqkwbo jpl(.edkdllsfv(py), .yfatkken(nhg), .godxfhunt(py));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic py -> logic [4:2][1:1] edkdllsfv
  //
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic py -> triand logic [4:2] godxfhunt
  
  or gxxqd(dcehy, dcehy, py);
  
  xor vfrqmuim(dcehy, py, jyhhsd);
  
endmodule: v



// Seed after: 12575419810562500732,599996410845877199
