// Seed: 17058616911232690737,9165656362651396931

module f (input bit z, input logic [0:3][3:3][0:4] v);
  not uo(mpsi, v);
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   logic [0:3][3:3][0:4] v -> logic v
  
endmodule: f



// Seed after: 1941183261495737597,9165656362651396931
