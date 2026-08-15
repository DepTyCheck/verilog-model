// Seed: 1974346120871191176,15659934682226287687

module pbbysfni ( input supply0 logic v [1:2][4:1]
                , input tri logic [4:3][3:0][0:3] k [1:2]
                , input wand logic [4:1] cld
                , inout supply0 logic [1:2][2:4] t
                );
  xnor tp(cld, ol, i);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   wand logic [4:1] cld -> logic cld
  
  xnor pzcin(bjiluvdnr, cld, bjiluvdnr);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   wand logic [4:1] cld -> logic cld
  
endmodule: pbbysfni

module nzra (inout trireg logic mtlpr [3:4][0:1][3:3][3:1]);
  // Unpacked net declarations
  tri logic [4:3][3:0][0:3] ckjwqrzqsf [1:2];
  supply0 logic pdvr [1:2][4:1];
  
  not bpln(f, ghlstdim);
  
  pbbysfni zgdqv(.v(pdvr), .k(ckjwqrzqsf), .cld(f), .t(f));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic f -> wand logic [4:1] cld
  //
  // warning: implicit conversion of port connection expands from 1 to 6 bits
  //   wire logic f -> supply0 logic [1:2][2:4] t
  
  
  // Multi-driven assignments
  assign mtlpr = mtlpr;
endmodule: nzra



// Seed after: 7892895749919087747,15659934682226287687
