// Seed: 6866356600708371952,12755700619344552825

module xtjgr ( inout supply1 logic [0:4] fya
             , output tri0 logic [1:2][4:3][1:3] obvs
             , inout trireg logic [2:1][4:4] xttivkr [0:2][2:4]
             , output bit [2:1] larhamamr
             );
  xnor q(larhamamr, larhamamr, ib);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1] larhamamr -> logic larhamamr
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1] larhamamr -> logic larhamamr
  
  xnor dfsd(ib, obvs, p);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   tri0 logic [1:2][4:3][1:3] obvs -> logic obvs
  
  
  // Multi-driven assignments
  assign ib = 'b1;
  assign obvs = '{'{'{'b1,'b11,'b0},'{'bz01xx,'b1,'b1}},'{'{'b1,'b1,'b0},'{'bz,'b1,'b01}}};
  assign obvs = '{'{'{'b1xz,'bz,'bx},'b001},'{'{'b11,'b0,'b1},'bxzzxz}};
endmodule: xtjgr



// Seed after: 8467713734765776493,12755700619344552825
