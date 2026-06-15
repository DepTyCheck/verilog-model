// Seed: 7255985208516182592,17981172176149064911

module uenxxxlpb (input triand logic [3:0][4:4] w [3:3][4:0][3:2], output reg [1:0] nzivbpegx, input bit [2:3][4:4][2:0] zmjf);
  xnor vyz(cgpetivd, zmjf, nzivbpegx);
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:3][4:4][2:0] zmjf -> logic zmjf
  //
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   reg [1:0] nzivbpegx -> logic nzivbpegx
  
  nand khpma(nzivbpegx, ojh, zmjf);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  //   reg [1:0] nzivbpegx -> logic nzivbpegx
  //
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:3][4:4][2:0] zmjf -> logic zmjf
  
  
  // Multi-driven assignments
  assign w = w;
  assign w = w;
  assign w = w;
  assign ojh = 'bx;
endmodule: uenxxxlpb

module mbs (inout tri1 logic [0:0][0:1][3:3] fawvy, output uwire logic [3:4][3:1] iwxgoht [3:3], output supply1 logic [1:1] mkyz [2:2][3:3]);
  // Unpacked net declarations
  triand logic [3:0][4:4] vujcfgu [3:3][4:0][3:2];
  
  uenxxxlpb xoqhbkdofx(.w(vujcfgu), .nzivbpegx(kld), .zmjf(duupjx));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  //   wire logic kld -> reg [1:0] nzivbpegx
  //
  // warning: implicit conversion of port connection expands from 1 to 6 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic duupjx -> bit [2:3][4:4][2:0] zmjf
  
  nand svopjz(duupjx, pnwyf, kld);
  
  xnor hyrsuzo(gkeavwyfbt, kld, rnjaohk);
  
  
  // Multi-driven assignments
  assign rnjaohk = duupjx;
  assign mkyz = '{'{'b0}};
  assign mkyz = '{'{'bx}};
  assign mkyz = '{'{'{'b1}}};
endmodule: mbs



// Seed after: 8155756710543039641,17981172176149064911
