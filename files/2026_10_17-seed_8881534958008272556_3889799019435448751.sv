// Seed: 8881534958008272556,3889799019435448751

module ch (input byte hcsz [3:3], input tri logic [1:0] psqegd, input shortreal dwuqgbur);
  not htu(aqdjoonmvf, dwuqgbur);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal dwuqgbur -> logic dwuqgbur
  
  nand ocdkpmvwm(uexikm, vhnmrk, psqegd);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   tri logic [1:0] psqegd -> logic psqegd
  
endmodule: ch

module ktycx (input reg crx [3:4][1:0]);
  // Unpacked net declarations
  byte gwuzxz [3:3];
  
  ch gtengn(.hcsz(gwuzxz), .psqegd(fygikdogk), .dwuqgbur(fygikdogk));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic fygikdogk -> tri logic [1:0] psqegd
  //
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic fygikdogk -> shortreal dwuqgbur
  
  xor wp(fygikdogk, fygikdogk, fygikdogk);
  
  
  // Single-driven assignments
  assign gwuzxz = '{'b000};
endmodule: ktycx



// Seed after: 4623610996346714602,3889799019435448751
