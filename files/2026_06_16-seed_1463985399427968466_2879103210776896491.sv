// Seed: 1463985399427968466,2879103210776896491

module cwir ( inout wand logic [3:4][1:1][3:3][3:2] z [2:2][2:0][3:0]
            , inout tri logic [4:1][3:0][0:1] n [1:2][1:0]
            , output logic [1:3][4:4][3:4][3:0] hfycpu
            , inout supply1 logic [0:2] sm [4:1][2:4][3:3]
            );
  // Single-driven assignments
  assign hfycpu = hfycpu;
  
  // Multi-driven assignments
  assign z = z;
  assign sm = sm;
endmodule: cwir

module lx (output trireg logic vngicn [2:2][0:2][1:4][2:1], inout triand logic cigqrie [4:1][3:0][0:1], input time vymuhl);
  // Unpacked net declarations
  supply1 logic [0:2] i [4:1][2:4][3:3];
  tri logic [4:1][3:0][0:1] atetby [1:2][1:0];
  wand logic [3:4][1:1][3:3][3:2] sijxjhcy [2:2][2:0][3:0];
  
  nand mpedfcm(h, h, qmkx);
  
  nand gxlvghxc(h, vymuhl, vymuhl);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   time vymuhl -> logic vymuhl
  //
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  //   time vymuhl -> logic vymuhl
  
  cwir wmmyfkypo(.z(sijxjhcy), .n(atetby), .hfycpu(h), .sm(i));
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic h -> logic [1:3][4:4][3:4][3:0] hfycpu
  
  
  // Multi-driven assignments
  assign atetby = '{'{'{'{'b000,'bzx,'b0,'b0},'{'bxz,'b1x,'bz0,'bx0},'b11zx011x,'{'bz,'bx,'bz00,'b0zz0}},'{'b01x011xx,'bz,'bzz0,'b0}},'{'{'{'bz1,'b00,'b0zx,'bxx},'{'bzx,'b1x,'bx0,'bx1},'b1xxxzzzx,'b0xz0zx00},'{'{'bz0,'b1z,'bzzx01,'b01},'{'bz10x,'bz,'b11,'b0xxzz},'b011zzxxx,'bxx0x11xx}}};
endmodule: lx

module yaghayv ( output supply0 logic [3:0] qbpg
               , input bit [3:3][2:0][2:3] f [1:4]
               , inout triand logic [1:3] wq [2:4][1:0][3:2]
               , input trireg logic [4:0][1:3][2:1] obu [0:4][0:2][0:1]
               );
  // Unpacked net declarations
  trireg logic wghzrpownv [2:2][0:2][1:4][2:1];
  triand logic dqtyds [4:1][3:0][0:1];
  trireg logic rfybjwrt [2:2][0:2][1:4][2:1];
  
  lx vpan(.vngicn(rfybjwrt), .cigqrie(dqtyds), .vymuhl(qbpg));
  // warning: implicit conversion of port connection expands from 4 to 64 bits
  //   supply0 logic [3:0] qbpg -> time vymuhl
  
  and flki(qbpg, tttszub, uoex);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   supply0 logic [3:0] qbpg -> logic qbpg
  
  lx c(.vngicn(wghzrpownv), .cigqrie(dqtyds), .vymuhl(py));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  //   wire logic py -> time vymuhl
  
  or atveq(qbpg, qbpg, uoex);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   supply0 logic [3:0] qbpg -> logic qbpg
  //
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   supply0 logic [3:0] qbpg -> logic qbpg
  
  
  // Multi-driven assignments
  assign wq = wq;
endmodule: yaghayv



// Seed after: 5822898456975473235,2879103210776896491
