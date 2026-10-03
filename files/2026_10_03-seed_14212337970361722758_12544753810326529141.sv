// Seed: 14212337970361722758,12544753810326529141

module svdown ();
  
endmodule: svdown

module zl (output byte mvvp [4:1], output bit [2:1][0:4][1:0] mhqcxqt, output bit pcdqfa);
  
endmodule: zl

module wpx (input tri logic [1:3] jqhxwpk);
  
endmodule: wpx

module zlscq ();
  // Unpacked net declarations
  byte ntdmhhos [4:1];
  
  wpx irqlcyrhj(.jqhxwpk(tupkjdd));
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic tupkjdd -> tri logic [1:3] jqhxwpk
  
  zl gyjkduj(.mvvp(ntdmhhos), .mhqcxqt(iqqbk), .pcdqfa(qwcenhd));
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic iqqbk -> bit [2:1][0:4][1:0] mhqcxqt
  //
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic qwcenhd -> bit pcdqfa
  
  nand d(qwcenhd, tvt, kaoimx);
  
  
  // Multi-driven assignments
  assign kaoimx = qwcenhd;
  assign tvt = 'b1;
  assign tupkjdd = tupkjdd;
  assign iqqbk = kaoimx;
endmodule: zlscq



// Seed after: 10800658594994148896,12544753810326529141
