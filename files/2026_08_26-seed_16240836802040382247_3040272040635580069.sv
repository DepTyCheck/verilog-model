// Seed: 16240836802040382247,3040272040635580069

module bg ( input bit zkc
          , output triand logic [2:2][4:4] mbi [4:3][1:2]
          , input tri logic [1:2][1:4][3:4] fhdobpjok [0:0]
          , input trior logic [4:4][0:2][1:0] y [4:4][0:2][4:3][2:1]
          );
  not btzyc(ikp, zkc);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit zkc -> logic zkc
  
  nand ezaxzww(we, esmb, zkc);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit zkc -> logic zkc
  
endmodule: bg

module fy (output trireg logic [4:4] r [0:2], output byte oamjpq [1:1][4:0], input bit [0:1] tpc);
  // Unpacked net declarations
  trior logic [4:4][0:2][1:0] xtawz [4:4][0:2][4:3][2:1];
  tri logic [1:2][1:4][3:4] ffjvt [0:0];
  triand logic [2:2][4:4] akkfg [4:3][1:2];
  
  xor zkzteystqx(vtdc, tpc, vtdc);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:1] tpc -> logic tpc
  
  nand cbyiiksrs(vtdc, tmslmzpug, vgdylcm);
  
  bg dqg(.zkc(f), .mbi(akkfg), .fhdobpjok(ffjvt), .y(xtawz));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic f -> bit zkc
  
  and suykln(dq, tpc, nigde);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:1] tpc -> logic tpc
  
  
  // Single-driven assignments
  assign oamjpq = '{'{'b1,'b00110010,'b010,'b0100,'b1000}};
  
  // Multi-driven assignments
  assign r = r;
  assign r = r;
endmodule: fy

module vo (input uwire logic zpwyb [0:3], output logic [2:3][0:4][3:2] zqfzzbshbd [0:2]);
  // Unpacked net declarations
  byte kinbdt [1:1][4:0];
  trireg logic [4:4] lbzhvlcb [0:2];
  
  and nigs(ohaogstv, ohaogstv, ohaogstv);
  
  not lxnalxsvzg(ohaogstv, ohaogstv);
  
  or ghrmbng(lnlccmeorj, ohaogstv, ohaogstv);
  
  fy a(.r(lbzhvlcb), .oamjpq(kinbdt), .tpc(zpp));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic zpp -> bit [0:1] tpc
  
  
  // Single-driven assignments
  assign zqfzzbshbd = '{'{'b0xz1z0z10z,'{'{'b1,'bx},'bxz10z,'b10,'{'bx00,'bz},'bzz}},'{'{'{'bx,'b0},'b1,'bxx1x0,'{'bx,'bx},'bx0},'b0},'{'{'bxz,'b0,'{'b0,'b10},'b1z,'{'bx1,'b0}},'{'{'bx,'bz},'{'bz0zz0,'bx},'{'bx,'b1},'{'b1z1,'bzz1x},'{'bzz10z,'b0}}}};
endmodule: vo



// Seed after: 18051278841054205755,3040272040635580069
