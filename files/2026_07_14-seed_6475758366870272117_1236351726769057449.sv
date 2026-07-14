// Seed: 6475758366870272117,1236351726769057449

module njjp (output logic [3:1] ezy, inout trireg logic [1:0][2:4] vbanzdp, output bit [2:2][2:1] zgnkxjy [0:1][4:0], inout trior logic [2:0] g);
  // Single-driven assignments
  assign ezy = '{'bx,'bz,'bx};
  assign zgnkxjy = '{'{'{'b01},'b00101,'b00,'{'b11},'b110},'{'{'b10},'b10,'{'{'b101,'b0}},'b00,'{'{'b1,'b1}}}};
  
  // Multi-driven assignments
  assign g = '{'b1,'bx,'b0};
  assign g = g;
  assign g = 'b01x;
  assign g = 'bzx110;
endmodule: njjp

module zcuckjb (input supply0 logic [1:4][0:1][0:2][4:3] rn [1:0][4:4][0:3], output reg [4:4] bjolckoh, input logic [3:0][1:0] iqpkehurz);
  // Unpacked net declarations
  bit [2:2][2:1] zu [0:1][4:0];
  
  njjp fqo(.ezy(yjgpvxbi), .vbanzdp(fm), .zgnkxjy(zu), .g(yjgpvxbi));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic yjgpvxbi -> logic [3:1] ezy
  //
  // warning: implicit conversion of port connection expands from 1 to 6 bits
  //   wire logic fm -> trireg logic [1:0][2:4] vbanzdp
  //
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic yjgpvxbi -> trior logic [2:0] g
  
  
  // Single-driven assignments
  assign bjolckoh = bjolckoh;
  
  // Multi-driven assignments
  assign fm = 'bxzzz1;
endmodule: zcuckjb



// Seed after: 10636865640523731705,1236351726769057449
