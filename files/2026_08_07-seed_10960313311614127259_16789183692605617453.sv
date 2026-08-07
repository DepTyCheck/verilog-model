// Seed: 10960313311614127259,16789183692605617453

module l (input int ygakln [2:2][3:0], input tri logic [3:0][1:2][1:1][2:1] fg);
  nand hokowmmtui(fg, fg, fg);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  //
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  //
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  
  nand r(fg, fe, fg);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  //
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  
  not xe(fg, fg);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  //
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   tri logic [3:0][1:2][1:1][2:1] fg -> logic fg
  
  
  // Multi-driven assignments
  assign fe = 'bx;
  assign fg = '{'b0000,'{'{'{'bx,'bx}},'bz},'{'{'{'b1,'bx}},'b1z},'{'{'{'b0,'b11001}},'bx00x0}};
  assign fg = 'bz101x0z10zzx1x1z;
endmodule: l



// Seed after: 18370752795405478091,16789183692605617453
