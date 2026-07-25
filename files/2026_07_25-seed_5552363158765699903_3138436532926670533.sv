// Seed: 5552363158765699903,3138436532926670533

module lv (output bit [2:3][2:3][4:2][0:1] zjzbq);
  nand klsdac(celq, celq, zjzbq);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:3][2:3][4:2][0:1] zjzbq -> logic zjzbq
  
  
  // Single-driven assignments
  assign zjzbq = '{'{'{'b01,'b01,'{'b0,'b00}},'b110000},'b100001101010};
  
  // Multi-driven assignments
  assign celq = celq;
  assign celq = celq;
  assign celq = 'b01zx;
endmodule: lv



// Seed after: 4411180098138586831,3138436532926670533
