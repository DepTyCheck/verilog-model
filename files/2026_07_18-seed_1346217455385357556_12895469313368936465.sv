// Seed: 1346217455385357556,12895469313368936465

module fndtaci (output int wxzgqc [3:4]);
  nand yzkwo(jutxbzqard, u, jutxbzqard);
  
  not hyqzgz(dcwit, rkgx);
  
  and du(mtyqxftbwu, mpqvvhxob, vwmzto);
  
  
  // Single-driven assignments
  assign wxzgqc = '{'b00011010111010111000011101001001,'b00};
  
  // Multi-driven assignments
  assign jutxbzqard = jutxbzqard;
  assign jutxbzqard = u;
  assign mtyqxftbwu = dcwit;
endmodule: fndtaci

module afqwndb (input bit [0:4] sr [0:0][2:4], input bit [0:0][3:1][4:0] cdn, input logic [2:0][4:2] mmjkxn, output bit [1:4] wzgjozzqv);
  // Single-driven assignments
  assign wzgjozzqv = 'b00;
endmodule: afqwndb

module cqjck (input uwire logic [0:2] k, input supply0 logic [2:1][4:2][0:1][2:1] thjwytf [2:2][0:3]);
  // Unpacked net declarations
  int abrxbhpnhk [3:4];
  
  and poubbsw(zjzppawye, k, zjzppawye);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   uwire logic [0:2] k -> logic k
  
  fndtaci xddkicqi(.wxzgqc(abrxbhpnhk));
  
  xnor bfbofhsgu(lpuucwa, k, zjzppawye);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   uwire logic [0:2] k -> logic k
  
  xnor zgfakpaefo(zjzppawye, zjzppawye, rxwmsqwz);
  
  
  // Multi-driven assignments
  assign lpuucwa = zjzppawye;
  assign lpuucwa = 'b0;
  assign zjzppawye = zjzppawye;
endmodule: cqjck

module amgsrb ( input tri0 logic [1:0][0:0] pnciauysgs
              , inout tri1 logic [0:3][0:3] u [0:4][3:3][4:0]
              , output trireg logic [1:4][3:2] venuwyk [3:1][1:1][2:0][0:2]
              , inout triand logic [0:2][4:0][2:1][3:4] hnmcbfo
              );
  // Unpacked net declarations
  bit [0:4] tkxlttucr [0:0][2:4];
  supply0 logic [2:1][4:2][0:1][2:1] dyhvzlstea [2:2][0:3];
  
  nand jdarrbeuuu(qeqgmyjsmx, pnciauysgs, vrkrqk);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   tri0 logic [1:0][0:0] pnciauysgs -> logic pnciauysgs
  
  cqjck nsliplv(.k(t), .thjwytf(dyhvzlstea));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic t -> uwire logic [0:2] k
  
  afqwndb d(.sr(tkxlttucr), .cdn(pnciauysgs), .mmjkxn(nwpcp), .wzgjozzqv(luww));
  // warning: implicit conversion of port connection expands from 2 to 15 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   tri0 logic [1:0][0:0] pnciauysgs -> bit [0:0][3:1][4:0] cdn
  //
  // warning: implicit conversion of port connection expands from 1 to 9 bits
  //   wire logic nwpcp -> logic [2:0][4:2] mmjkxn
  //
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic luww -> bit [1:4] wzgjozzqv
  
  
  // Single-driven assignments
  assign tkxlttucr = '{'{'{'b0,'b0,'b00,'b00,'b1},'{'b1100,'b1,'b1,'b1110,'b1011},'{'b1,'b0,'b1,'b0,'b01}}};
  
  // Multi-driven assignments
  assign qeqgmyjsmx = qeqgmyjsmx;
  assign qeqgmyjsmx = 'bz;
endmodule: amgsrb



// Seed after: 2558249666353081175,12895469313368936465
