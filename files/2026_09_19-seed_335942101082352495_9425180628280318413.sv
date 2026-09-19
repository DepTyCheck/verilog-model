// Seed: 335942101082352495,9425180628280318413

module fb (inout tri logic [0:0][4:0][1:0] rufoslv [0:4][0:2][2:1][3:0], inout triand logic [0:1][4:3][1:1][0:2] h [1:0][0:0]);
  // Multi-driven assignments
  assign h = '{'{'{'{'bz1x1,'bx1x},'{'bz11,'b0xx}}},'{'{'{'b1xzz,'b01xzx},'{'b1zz,'b100}}}};
  assign h = h;
  assign h = h;
endmodule: fb



// Seed after: 1323402796985702735,9425180628280318413
