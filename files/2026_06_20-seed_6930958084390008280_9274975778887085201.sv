// Seed: 6930958084390008280,9274975778887085201

module mrnp (input bit fwgeuwsftc [3:3][2:1]);
  
endmodule: mrnp

module uwkoxq ( inout trireg logic [3:4] lxmgffyr [3:2][0:4]
              , inout wor logic [0:3] fxzis
              , input shortint eekzypme
              , input supply1 logic [1:3][4:4] r [4:3][3:4][3:2][0:3]
              );
  // Multi-driven assignments
  assign r = '{'{'{'{'b0zx,'bx01,'bzxx,'b00x},'{'bxx1,'bz0z,'bx0,'bx10}},'{'{'bx,'b10,'bxxx10,'bz1},'{'bx10,'b1z0xx,'bxz100,'b1z}}},'{'{'{'bx1z,'bx11,'b01x,'b1zx},'{'b0z,'bzzx,'b1,'bz1x1}},'{'{'bz1111,'bzz1,'bz0x,'bz00xx},'{'b110,'bz,'bz00,'b0z11}}}};
  assign lxmgffyr = '{'{'{'bxxx1x,'b0z},'{'bxx1,'bx},'{'b1,'b1},'b0zx,'{'b110,'b0}},'{'bxx,'b10,'b0001,'{'bzzz,'bx},'b1z}};
  assign lxmgffyr = lxmgffyr;
  assign r = r;
endmodule: uwkoxq

module rzzurpj (output bit ffi, inout triand logic [3:4] qbicbfp [4:3][0:3], output shortint zcndeczbxe [3:0][4:1], input trireg logic [4:4][2:1] fturhvhl [1:1][1:3]);
  // Unpacked net declarations
  supply1 logic [1:3][4:4] xdu [4:3][3:4][3:2][0:3];
  trireg logic [3:4] agayz [3:2][0:4];
  
  not ltrjtf(ffi, krtomt);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit ffi -> logic ffi
  
  xnor wzxvk(krtomt, ffi, utj);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit ffi -> logic ffi
  
  and vnfk(krtomt, qge, ir);
  
  uwkoxq gxmdvqiram(.lxmgffyr(agayz), .fxzis(sfjkkjmddy), .eekzypme(utj), .r(xdu));
  // warning: implicit conversion of port connection expands from 1 to 4 bits
  //   wire logic sfjkkjmddy -> wor logic [0:3] fxzis
  //
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic utj -> shortint eekzypme
  
  
  // Single-driven assignments
  assign zcndeczbxe = '{'{'b01100,'b00000,'b1101110111110111,'b0001},'{'b1101,'b01,'b1110111010000110,'b0100101110101010},'{'b011,'b0,'b11,'b011},'{'b00110,'b0011111010011110,'b010,'b1010110011010001}};
endmodule: rzzurpj

module dn (output supply0 logic [4:3][1:2] iyfhhc);
  // Unpacked net declarations
  trireg logic [4:4][2:1] dffzdbovdo [1:1][1:3];
  shortint qmvessxb [3:0][4:1];
  triand logic [3:4] tnzmps [4:3][0:3];
  
  rzzurpj jmtxhyjdf(.ffi(iyfhhc), .qbicbfp(tnzmps), .zcndeczbxe(qmvessxb), .fturhvhl(dffzdbovdo));
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   supply0 logic [4:3][1:2] iyfhhc -> bit ffi
  
endmodule: dn



// Seed after: 15988403262782857948,9274975778887085201
