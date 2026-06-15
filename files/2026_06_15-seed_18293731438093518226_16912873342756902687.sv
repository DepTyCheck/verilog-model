// Seed: 18293731438093518226,16912873342756902687

module sw (inout trior logic [0:2][2:3][4:2][3:3] zvhzjg [0:3][1:4][1:4], input trireg logic [3:0] mzqtkssbxg [4:2][4:4]);
  // Multi-driven assignments
  assign mzqtkssbxg = '{'{'{'bzzx01,'b1,'b0,'bz}},'{'{'b0,'b0,'b0,'b101z}},'{'{'b1,'bxx0,'b0,'b1}}};
  assign mzqtkssbxg = mzqtkssbxg;
endmodule: sw

module hcx (output uwire logic [3:3][3:0] zcxwgd, inout trior logic [3:2][3:2][3:4] h [4:4][0:0][4:4], inout tri1 logic [2:4][3:0] co [0:2]);
  // Unpacked net declarations
  trireg logic [3:0] ru [4:2][4:4];
  trior logic [0:2][2:3][4:2][3:3] ba [0:3][1:4][1:4];
  
  xnor sebuzxkj(wf, wf, zcxwgd);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   uwire logic [3:3][3:0] zcxwgd -> logic zcxwgd
  
  sw pz(.zvhzjg(ba), .mzqtkssbxg(ru));
  
  xnor m(zcxwgd, fylquu, wf);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   uwire logic [3:3][3:0] zcxwgd -> logic zcxwgd
  
  
  // Multi-driven assignments
  assign co = '{'b01z01z0z1101,'{'bzzzx,'b0,'{'b0,'b10,'b000,'bx}},'{'{'b0,'bx1010,'bx,'bx101},'{'bz,'b110,'bz,'bx},'{'bx,'b001,'b0,'b1z01}}};
  assign co = '{'{'{'b1,'b0,'bz,'bx},'{'b0,'bz1,'b0,'bxx},'b0xxz},'{'{'bz,'b1,'bx,'b1},'b00x0,'bx011},'{'{'bx,'bx,'bz,'b1},'b0zx,'bx}};
  assign ru = '{'{'b00xx},'{'bx},'{'{'b0,'bx,'bx0,'bx}}};
  assign h = h;
endmodule: hcx

module kf (output tri0 logic [1:1] koniini [1:2][4:4][3:2]);
  // Unpacked net declarations
  tri1 logic [2:4][3:0] rnwdotr [0:2];
  trior logic [3:2][3:2][3:4] ao [4:4][0:0][4:4];
  
  or jtxmzdzlj(xqikpoq, xqikpoq, xqikpoq);
  
  hcx jr(.zcxwgd(ipuvdsa), .h(ao), .co(rnwdotr));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic ipuvdsa -> uwire logic [3:3][3:0] zcxwgd
  
  
  // Multi-driven assignments
  assign koniini = '{'{'{'bz,'{'b1}}},'{'{'b0z,'bx}}};
  assign rnwdotr = rnwdotr;
  assign xqikpoq = 'bx1xx;
  assign rnwdotr = rnwdotr;
endmodule: kf

module kwioezs ( inout supply0 logic [0:3] oedgw [3:3]
               , input trior logic [2:4][3:2][1:0] cczimd
               , input supply1 logic [0:3][0:2][1:2][1:0] q
               , output bit [0:0][2:4][2:0][3:3] cfkpdkwlt
               );
  // Unpacked net declarations
  tri0 logic [1:1] l [1:2][4:4][3:2];
  
  and sddgaromr(adiipyh, okhspmk, cfkpdkwlt);
  // warning: implicit conversion of port connection truncates from 9 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [0:0][2:4][2:0][3:3] cfkpdkwlt -> logic cfkpdkwlt
  
  xnor tykqbhjmh(q, adiipyh, q);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   supply1 logic [0:3][0:2][1:2][1:0] q -> logic q
  //
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   supply1 logic [0:3][0:2][1:2][1:0] q -> logic q
  
  kf dyywww(.koniini(l));
  
  
  // Single-driven assignments
  assign cfkpdkwlt = cfkpdkwlt;
  
  // Multi-driven assignments
  assign adiipyh = adiipyh;
  assign oedgw = '{'{'b0,'bx11,'b0z0x,'b100}};
endmodule: kwioezs



// Seed after: 5097086910711298616,16912873342756902687
