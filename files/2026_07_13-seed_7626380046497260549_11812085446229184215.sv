// Seed: 7626380046497260549,11812085446229184215

module wtozs (output shortreal pecjbt, input reg [0:0][3:1][2:3] tfp, output bit pbzutmgz, output reg zliffgca [0:1][3:3][2:3]);
  or xeszmnhki(unyw, tfp, unyw);
  // warning: implicit conversion of port connection truncates from 6 to 1 bits
  //   reg [0:0][3:1][2:3] tfp -> logic tfp
  
  not xujcgv(pbzutmgz, zdroljc);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit pbzutmgz -> logic pbzutmgz
  
  or ft(pecjbt, xauljgma, cfjpc);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   shortreal pecjbt -> logic pecjbt
  
  
  // Single-driven assignments
  assign zliffgca = '{'{'{'bzz,'bx}},'{'{'bz,'bx}}};
  
  // Multi-driven assignments
  assign cfjpc = unyw;
  assign xauljgma = unyw;
endmodule: wtozs

module pcwe ();
  // Unpacked net declarations
  reg jkg [0:1][3:3][2:3];
  
  wtozs p(.pecjbt(gsxavr), .tfp(dls), .pbzutmgz(cq), .zliffgca(jkg));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic gsxavr -> shortreal pecjbt
  //
  // warning: implicit conversion of port connection expands from 1 to 6 bits
  //   wire logic dls -> reg [0:0][3:1][2:3] tfp
  //
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic cq -> bit pbzutmgz
  
  xnor fpdorrb(gsxavr, gsxavr, dhli);
  
  
  // Multi-driven assignments
  assign dls = 'b0z;
  assign dhli = cq;
endmodule: pcwe

module n (input tri0 logic ypw [4:4][0:2][0:2], output integer fvkdunbi);
  nand qomm(fvkdunbi, fvkdunbi, u);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   integer fvkdunbi -> logic fvkdunbi
  //
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   integer fvkdunbi -> logic fvkdunbi
  
  
  // Multi-driven assignments
  assign ypw = '{'{'{'b1,'b0zzzx,'bx},'{'bz,'b0,'bx},'{'bxz0,'bx0xx,'bx}}};
  assign u = u;
  assign u = u;
  assign ypw = '{'{'{'b1011,'bx,'b0},'{'b10,'bz,'b0},'{'b0,'bz,'b1}}};
endmodule: n



// Seed after: 16688269572800757811,11812085446229184215
