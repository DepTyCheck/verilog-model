// Seed: 14433474275339355046,11986128939542592591

module xqtdwdjfmq ( input bit [4:3][1:3][1:1][1:3] ruqxhhst
                  , inout trior logic [2:3][2:1] puyqo [2:2][1:4][3:2]
                  , inout supply0 logic [2:1][0:0][0:0] kxrsd
                  , inout triand logic rldcrc [3:4][2:2][1:4][4:3]
                  );
  
  and yymnhdhjyu(npuayxvi, npuayxvi, ouslgnzdpx);
  
  and osnx(npuayxvi, oeplv, ruqxhhst);
  // warning: implicit conversion of port connection truncates from 18 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:3][1:3][1:1][1:3] ruqxhhst -> logic ruqxhhst
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign rldcrc = rldcrc;
endmodule: xqtdwdjfmq

module lcgb (input logic [0:1][0:4][0:0] o, input reg [1:4][1:3][0:1] ymc);
  // Unpacked net declarations
  triand logic dkxadze [3:4][2:2][1:4][4:3];
  trior logic [2:3][2:1] fclklziw [2:2][1:4][3:2];
  
  xqtdwdjfmq dojj(.ruqxhhst(f), .puyqo(fclklziw), .kxrsd(f), .rldcrc(dkxadze));
  // warning: implicit conversion of port connection expands from 1 to 18 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic f -> bit [4:3][1:3][1:1][1:3] ruqxhhst
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic f -> supply0 logic [2:1][0:0][0:0] kxrsd
  
  xnor uiwt(guebso, ymc, guebso);
  // warning: implicit conversion of port connection truncates from 24 to 1 bits
  //   reg [1:4][1:3][0:1] ymc -> logic ymc
  
  nand unlnsb(z, o, guebso);
  // warning: implicit conversion of port connection truncates from 10 to 1 bits
  //   logic [0:1][0:4][0:0] o -> logic o
  
  xor ehpe(uct, uct, z);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign fclklziw = fclklziw;
  assign uct = 'bxx0x;
  assign dkxadze = '{'{'{'{'b1,'b11},'{'b0001z,'b0z},'{'bz,'bx},'{'bx1z0x,'b0xx}}},'{'{'{'b0zz,'b01110},'{'bz,'b00x0x},'{'b0,'bz101x},'{'bx,'bx10}}}};
  assign f = guebso;
  assign guebso = 'b01;
endmodule: lcgb

module q (output supply0 logic [0:1][2:4][2:2] qvkl [1:2][2:3][0:1]);
  // Unpacked net declarations
  triand logic pp [3:4][2:2][1:4][4:3];
  trior logic [2:3][2:1] mwntgka [2:2][1:4][3:2];
  
  lcgb bbs(.o(xw), .ymc(avffrz));
  // warning: implicit conversion of port connection expands from 1 to 10 bits
  //   wire logic xw -> logic [0:1][0:4][0:0] o
  //
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic avffrz -> reg [1:4][1:3][0:1] ymc
  
  xqtdwdjfmq dtlk(.ruqxhhst(xw), .puyqo(mwntgka), .kxrsd(avffrz), .rldcrc(pp));
  // warning: implicit conversion of port connection expands from 1 to 18 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic xw -> bit [4:3][1:3][1:1][1:3] ruqxhhst
  //
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic avffrz -> supply0 logic [2:1][0:0][0:0] kxrsd
  
  xnor utwjjrai(xw, xftb, vspgdglch);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xw = 'b1x1;
endmodule: q

module pkc (input wire logic [1:2][3:0] jgiotgkg [3:1][2:3][1:1][0:4], inout tri logic [3:3][0:1][2:0][1:0] zqrmw [1:2][3:0]);
  
  or dfzqdixao(yulfnn, jget, fdzvtc);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign fdzvtc = 'b1zz;
  assign jget = 'b101;
  assign yulfnn = yulfnn;
  assign jgiotgkg = jgiotgkg;
  assign zqrmw = zqrmw;
endmodule: pkc



// Seed after: 4746810427149180518,11986128939542592591
