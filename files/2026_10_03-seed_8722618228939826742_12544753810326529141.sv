// Seed: 8722618228939826742,12544753810326529141

module snrgetub (output reg [3:0][0:3][3:2] vrpxipbs, input supply0 logic qpfw [2:3][2:0][1:3][4:0]);
  // Single-driven assignments
  assign vrpxipbs = vrpxipbs;
  
  // Multi-driven assignments
  assign qpfw = '{'{'{'{'bz,'b1,'bz,'bx,'bz},'{'b0,'bz,'b0,'b1,'bz1},'{'b0,'bzx0,'bx1x1x,'bz,'b1x0}},'{'{'bx0,'b0,'b1,'b00zx,'b1},'{'bz1,'b01000,'bz,'b1,'b100z},'{'bz,'bzxz,'b0,'b1,'b1}},'{'{'b0,'bx,'b0,'bz,'bzx},'{'bz0001,'b100x,'bz,'b1,'bx},'{'b0,'b0,'b10,'bxz,'b0}}},'{'{'{'b11,'b000z,'b0,'bz,'b0},'{'b0,'bx,'b0,'b10z,'b1x0x},'{'bz,'b0,'bx,'bz,'b1}},'{'{'bz0xz,'bz,'bzx,'b1xz1,'bx1x},'{'bx,'b0,'b0,'bz,'b0z},'{'b0,'b0,'bz0,'bzxzz0,'bz}},'{'{'bz,'bx,'b0,'bx00x,'bx1},'{'bz,'b1,'b0,'b0z0,'bz},'{'bz,'b111zz,'b0,'b1,'b0}}}};
endmodule: snrgetub

module my ();
  // Unpacked net declarations
  supply0 logic vzbdii [2:3][2:0][1:3][4:0];
  
  snrgetub oakoaohqm(.vrpxipbs(yjzk), .qpfw(vzbdii));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  //   wire logic yjzk -> reg [3:0][0:3][3:2] vrpxipbs
  
  
  // Multi-driven assignments
  assign yjzk = 'b1;
endmodule: my



// Seed after: 11940686907353016791,12544753810326529141
