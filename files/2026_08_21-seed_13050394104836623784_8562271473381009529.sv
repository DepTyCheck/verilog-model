// Seed: 13050394104836623784,8562271473381009529

module ocwohw ( output reg [4:2][4:4][4:4] insrqavpb
              , input tri0 logic [1:0][2:1][1:1] apczehsnyi [0:4]
              , inout wand logic [4:2] ilgkopeof
              , inout wire logic pbtdzukq [2:1][2:4][2:4]
              );
  xnor mdgrko(insrqavpb, jekzxbe, dwbxqyekb);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   reg [4:2][4:4][4:4] insrqavpb -> logic insrqavpb
  
  nand ytyfl(jekzxbe, icwm, insrqavpb);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   reg [4:2][4:4][4:4] insrqavpb -> logic insrqavpb
  
endmodule: ocwohw

module nknjjxvt ( output bit [3:3][1:2][1:1] aipelbfl
                , input tri1 logic [0:2][1:0][4:0] gtg [1:2][1:0][1:3]
                , output logic [0:3][0:0][1:1] rreiwr
                , inout tri1 logic [0:3][3:0] yansq [4:2][4:4][2:0][3:4]
                );
  xor njbytgns(iaymj, iaymj, obphj);
  
  and xkqfu(mquqtxs, hwfi, rreiwr);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   logic [0:3][0:0][1:1] rreiwr -> logic rreiwr
  
  or lycqiseui(elrjs, oguovkk, fmzpw);
  
  or yejefvuu(dk, rreiwr, coos);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   logic [0:3][0:0][1:1] rreiwr -> logic rreiwr
  
  
  // Single-driven assignments
  assign rreiwr = '{'{'{'b1}},'bx,'{'{'b1}},'{'{'b1}}};
  assign aipelbfl = aipelbfl;
endmodule: nknjjxvt

module hxymxphl (input real xstnczhtk [2:0][3:4], inout trior logic [4:1][1:4][2:2][1:4] jjqujt [2:0][2:0]);
  // Unpacked net declarations
  tri1 logic [0:3][3:0] wjggjv [4:2][4:4][2:0][3:4];
  tri1 logic [0:2][1:0][4:0] ogbtjshgy [1:2][1:0][1:3];
  wire logic b [2:1][2:4][2:4];
  tri0 logic [1:0][2:1][1:1] x [0:4];
  
  ocwohw xdeqqhwx(.insrqavpb(ainlzsr), .apczehsnyi(x), .ilgkopeof(jtnzlix), .pbtdzukq(b));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic ainlzsr -> reg [4:2][4:4][4:4] insrqavpb
  //
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic jtnzlix -> wand logic [4:2] ilgkopeof
  
  not qyy(bcutgehrvw, bcutgehrvw);
  
  xor sfjscxhjc(twqcbksm, bcutgehrvw, daz);
  
  nknjjxvt cpf(.aipelbfl(khdbuz), .gtg(ogbtjshgy), .rreiwr(jqtyz), .yansq(wjggjv));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic khdbuz -> bit [3:3][1:2][1:1] aipelbfl
  //
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic jqtyz -> logic [0:3][0:0][1:1] rreiwr
  
  
  // Multi-driven assignments
  assign twqcbksm = ainlzsr;
  assign jtnzlix = bcutgehrvw;
  assign jjqujt = jjqujt;
  assign ainlzsr = 'b1;
endmodule: hxymxphl

module jozv ();
  // Unpacked net declarations
  trior logic [4:1][1:4][2:2][1:4] xyp [2:0][2:0];
  real dluml [2:0][3:4];
  
  nand qgeezdc(dggehe, dggehe, dggehe);
  
  hxymxphl nsdzoevhwn(.xstnczhtk(dluml), .jjqujt(xyp));
  
  
  // Single-driven assignments
  assign dluml = dluml;
  
  // Multi-driven assignments
  assign dggehe = dggehe;
  assign dggehe = dggehe;
  assign dggehe = 'b0;
  assign dggehe = dggehe;
endmodule: jozv



// Seed after: 11052326038438182867,8562271473381009529
