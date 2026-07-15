// Seed: 2809128614332643880,6650287718302477117

module rfanttit (input reg [1:1] ubtz, output tri1 logic [1:2][1:1] fz [4:2][1:4][3:1], output bit [3:0][1:3] flzhc);
  xor obc(flzhc, usbh, h);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:0][1:3] flzhc -> logic flzhc
  
  
  // Multi-driven assignments
  assign fz = '{'{'{'{'b1zxz,'b1},'b1z,'{'bzz1x1,'b0xz0}},'{'b1x,'{'b0,'bx},'b00},'{'{'bz,'bxxxz},'{'b1,'bzzxz},'bzx},'{'b1x,'bzx,'bxz}},'{'{'{'bx,'bz1x},'bx1,'{'b0110x,'b01z1}},'{'bxx,'{'b01x,'b1},'{'bz10,'b1}},'{'b11,'bxzx,'{'bz,'bz}},'{'bzx,'{'bz,'b0},'bx1z11}},'{'{'{'bx,'b1},'{'bxz01z,'b000xx},'{'bz01,'b1}},'{'bz,'{'bx01x,'bxx0z},'bzz},'{'{'bz1xxx,'b1xx1},'b0xz,'bz0z},'{'{'b1zzx0,'bzz},'{'b1x0,'b1xx1},'b01}}};
endmodule: rfanttit



// Seed after: 10585015456810709310,6650287718302477117
