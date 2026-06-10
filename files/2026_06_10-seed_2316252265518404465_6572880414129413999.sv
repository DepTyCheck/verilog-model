// Seed: 2316252265518404465,6572880414129413999

module alupdnv (output logic [2:2][4:0][1:4][3:1] tydldpcva);
  
  
  
  // Single-driven assigns
  assign tydldpcva = '{'{'{'{'b1z0x,'b01z1,'b1},'{'bx0,'b1zx,'bz0},'{'b1,'bx,'b0xx0},'{'b1zx0,'bz0,'bz}},'{'{'bx,'b0x0,'bz1},'{'b11z1,'b1000z,'b01x},'{'bx,'bxz0,'bx1x},'{'b1,'b10,'bz}},'{'{'bxx1,'bxxz00,'bxxxzx},'{'bx0x,'bx00,'bxz1},'{'bz1z,'bx,'b0x00},'{'bxzxz,'b1z00,'b10}},'{'{'b0,'bz,'b0x},'{'b0,'bzx1,'b1},'{'b1zxx,'bzz11,'bzzxzz},'{'bxz1,'b00xx1,'bz1xz}},'{'{'bz0z0,'b1z,'b01},'{'bz1,'bx0x11,'b1z1},'{'bx10x1,'b0z0,'b0x},'{'b0x0zz,'b1z00,'b11xx1}}}};
  
  // Multi-driven assigns
endmodule: alupdnv

module lgd (inout trireg logic [4:1][3:3][0:1][2:4] ma, input triand logic [1:2][0:0][3:2][4:4] ejpafgomyu, input logic [4:2] uqbonu);
  
  xnor pvxyfswogn(ejpafgomyu, uqbonu, obhwddpse);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   triand logic [1:2][0:0][3:2][4:4] ejpafgomyu -> logic ejpafgomyu
  //
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   logic [4:2] uqbonu -> logic uqbonu
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign ejpafgomyu = ejpafgomyu;
  assign obhwddpse = obhwddpse;
  assign ma = '{'{'{'{'b0z0x0,'b1xz,'bzx},'{'b01,'b0xx,'b0z}}},'{'{'{'b0z0,'bz00,'b1z1},'{'bx,'bz111,'b0011}}},'{'{'{'b0x0,'b001,'b01},'{'bz1xz,'b1z,'b0}}},'{'{'{'b0x1x0,'bx01z0,'b1z},'{'bzx,'b1,'b1z}}}};
endmodule: lgd

module tcjyp ();
  
  xor bgvhmeqhio(aqy, aotymze, aqy);
  
  or dpxepoy(aotymze, hodbwgeefg, aqy);
  
  lgd cpisv(.ma(x), .ejpafgomyu(rgox), .uqbonu(aqy));
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic x -> trireg logic [4:1][3:3][0:1][2:4] ma
  //
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic rgox -> triand logic [1:2][0:0][3:2][4:4] ejpafgomyu
  //
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic aqy -> logic [4:2] uqbonu
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: tcjyp

module prmvck ();
  
  lgd nicsvmajmd(.ma(mwtbbncf), .ejpafgomyu(w), .uqbonu(zvh));
  // warning: implicit conversion of port connection expands from 1 to 24 bits
  //   wire logic mwtbbncf -> trireg logic [4:1][3:3][0:1][2:4] ma
  //
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic w -> triand logic [1:2][0:0][3:2][4:4] ejpafgomyu
  //
  // warning: implicit conversion of port connection expands from 1 to 3 bits
  //   wire logic zvh -> logic [4:2] uqbonu
  
  alupdnv vf(.tydldpcva(wav));
  // warning: implicit conversion of port connection expands from 1 to 60 bits
  //   wire logic wav -> logic [2:2][4:0][1:4][3:1] tydldpcva
  
  and bfdhudzct(w, wav, uetoqppdn);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign uetoqppdn = 'bx;
endmodule: prmvck



// Seed after: 2981567375744992708,6572880414129413999
