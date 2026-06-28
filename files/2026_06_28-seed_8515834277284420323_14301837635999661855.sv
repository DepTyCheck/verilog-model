// Seed: 8515834277284420323,14301837635999661855

module m (input wand logic [1:1][4:2][1:0][4:3] lqysqblolo, inout tri logic [0:2] pjtgmrczfs [1:3][3:0][3:4]);
  xnor rfdrysta(lqysqblolo, shj, wnbfrvt);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   wand logic [1:1][4:2][1:0][4:3] lqysqblolo -> logic lqysqblolo
  
  or wgaikzfhv(uaezqdr, iis, lqysqblolo);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   wand logic [1:1][4:2][1:0][4:3] lqysqblolo -> logic lqysqblolo
  
  xnor mxv(nwngcstmwv, lqysqblolo, kt);
  // warning: implicit conversion of port connection truncates from 12 to 1 bits
  //   wand logic [1:1][4:2][1:0][4:3] lqysqblolo -> logic lqysqblolo
  
  
  // Multi-driven assignments
  assign iis = shj;
endmodule: m

module dfzfnokbi (output logic [4:3][1:2][4:0] oqlatsd [3:4], input logic [4:2] yafcixwao [0:4][0:1][4:3], output byte cmgqkwk);
  // Single-driven assignments
  assign oqlatsd = '{'{'{'bzz1z0,'{'bx0,'b0,'b0,'bx1z0,'bz}},'{'b1x0z0,'bx0xxx}},'{'{'b1x01x,'bz},'b10zzx01z1z}};
endmodule: dfzfnokbi

module ttxg ();
  // Unpacked net declarations
  logic [4:2] jjlho [0:4][0:1][4:3];
  logic [4:3][1:2][4:0] tpiaemem [3:4];
  tri logic [0:2] ijtz [1:3][3:0][3:4];
  
  m w(.lqysqblolo(jt), .pjtgmrczfs(ijtz));
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic jt -> wand logic [1:1][4:2][1:0][4:3] lqysqblolo
  
  dfzfnokbi oo(.oqlatsd(tpiaemem), .yafcixwao(jjlho), .cmgqkwk(quuyzbaw));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic quuyzbaw -> byte cmgqkwk
  
  
  // Single-driven assignments
  assign jjlho = jjlho;
  
  // Multi-driven assignments
  assign jt = jt;
  assign ijtz = ijtz;
endmodule: ttxg



// Seed after: 3192137054073034518,14301837635999661855
