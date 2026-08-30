// Seed: 8671324208107552436,1567685517161240205

module gaz (output reg cfmnfab [2:4][3:4][4:0], output tri0 logic [3:3][4:4][3:3][0:1] bs [1:1][0:4][3:2][4:1]);
  // Single-driven assignments
  assign cfmnfab = '{'{'{'b10x,'b0,'bx1xx,'b1,'bz},'{'bx0zz,'b1,'bz,'b0,'bz}},'{'{'bzzx,'bx,'b0,'b1,'b1},'{'bz1,'bz,'b001,'b0,'bz}},'{'{'bz00zz,'b11,'b01,'b1,'b0x},'{'bzxz1,'bxz0,'b1,'b1xx,'b1}}};
  
  // Multi-driven assignments
  assign bs = bs;
  assign bs = bs;
  assign bs = bs;
endmodule: gaz



// Seed after: 4450379331079635391,1567685517161240205
