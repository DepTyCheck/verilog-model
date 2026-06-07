// Seed: 5906768364939250053,10774379589804174675

module eiut ( inout tri1 logic [0:2][1:1][1:3][0:4] jchc [4:0][4:4][1:1]
            , inout triand logic [2:1][3:0][3:3] dri [2:3][2:4][0:0]
            , inout trireg logic i [0:0][1:4][3:0][4:1]
            , input trireg logic [1:4][3:2] lwenlmt
            );
  
  xnor ohvco(lwenlmt, lwenlmt, lwenlmt);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [1:4][3:2] lwenlmt -> logic lwenlmt
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [1:4][3:2] lwenlmt -> logic lwenlmt
  //
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  //   trireg logic [1:4][3:2] lwenlmt -> logic lwenlmt
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: eiut



// Seed after: 12856972143971279096,10774379589804174675
