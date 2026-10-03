// Seed: 6602758733804876038,12544753810326529141

module fu (output bit [3:4][4:2][0:4] wzoda [0:2], inout wire logic [1:3][3:2][0:0][2:4] u [0:2][0:1][2:0]);
  // Single-driven assignments
  assign wzoda = '{'b1101,'{'b10,'{'{'b1,'b1,'b00,'b1,'b010},'{'b10,'b101,'b1,'b1,'b1101},'b01001}},'{'b100,'b001}};
  
  // Multi-driven assignments
  assign u = '{'{'{'b0,'{'bzz1x,'b1xx11z,'bx1z},'b11000x0xz1zx0x0zx0},'{'b1z110,'{'b1z1,'b1zx,'bz},'{'b1z1xxx,'bzz11zx,'bxzxz0z}}},'{'{'{'b0,'b1,'b0z11x},'{'b1zxx1x,'bz,'bx001z1},'{'bx10,'b11xz0,'b1x1}},'{'b1z0zx1z11zzzzz0101,'b10,'{'b1x11xx,'bzx0x1,'bz0zz00}}},'{'{'{'b11010z,'b00zx00,'bx0zxx0},'{'bz01z00,'b111xx0,'b10},'{'b0010,'bzzz,'b11x1x0}},'{'{'bz0x0xz,'bz01x10,'b1},'{'bx01z,'bz000xz,'bx},'{'bxx01xz,'bx0,'bx1zxz1}}}};
endmodule: fu



// Seed after: 1203294321570671527,12544753810326529141
