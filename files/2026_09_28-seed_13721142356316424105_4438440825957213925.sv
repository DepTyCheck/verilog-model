// Seed: 13721142356316424105,4438440825957213925

module sfp (input logic [2:0][4:3][1:4][0:3] rqc, inout triand logic [0:2] tzx [0:1]);
  or arcx(vsq, rqc, rqc);
  // warning: implicit conversion of port connection truncates from 96 to 1 bits
  //   logic [2:0][4:3][1:4][0:3] rqc -> logic rqc
  //
  // warning: implicit conversion of port connection truncates from 96 to 1 bits
  //   logic [2:0][4:3][1:4][0:3] rqc -> logic rqc
  
  
  // Multi-driven assignments
  assign vsq = 'bxx0;
  assign vsq = vsq;
  assign vsq = vsq;
  assign vsq = 'b0;
endmodule: sfp



// Seed after: 14517727680115705345,4438440825957213925
