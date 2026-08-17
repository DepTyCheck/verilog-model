// Seed: 12912282335391317755,9165656362651396931

module s (output logic epuy, output bit [2:1][4:2] injd, inout tri logic hioput [0:0][4:1]);
  nand aju(injd, epuy, injd);
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][4:2] injd -> logic injd
  //
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][4:2] injd -> logic injd
  
  
  // Single-driven assignments
  assign epuy = 'b0111;
  
  // Multi-driven assignments
  assign hioput = '{'{'b1,'bxxx0x,'b1,'bxx00z}};
  assign hioput = hioput;
endmodule: s



// Seed after: 9546574551771966499,9165656362651396931
