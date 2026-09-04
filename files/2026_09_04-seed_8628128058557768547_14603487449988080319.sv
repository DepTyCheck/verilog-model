// Seed: 8628128058557768547,14603487449988080319

module cgdch (output logic [3:0][4:3][2:2][4:3] onhn, inout tri logic [0:2][0:1][3:2][1:1] eejlqibaux, input shortint cgzpb);
  xnor mmctjpdmlw(coidiyoong, onhn, npxepsg);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  //   logic [3:0][4:3][2:2][4:3] onhn -> logic onhn
  
  xor lotiyghif(pwsovxro, cgzpb, iemhimh);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   shortint cgzpb -> logic cgzpb
  
  
  // Single-driven assignments
  assign onhn = onhn;
  
  // Multi-driven assignments
  assign eejlqibaux = '{'{'{'bz0zxz,'b1},'b1xz},'bzzxx,'{'{'{'b1},'{'b0}},'{'b0,'bz0}}};
endmodule: cgdch

module gske (output tri logic y [2:0][3:0][2:3][0:1], input tri0 logic [4:1][1:2][4:1] qlsw [1:3][1:1][0:1][1:3]);
  not xlxkz(hud, hud);
  
  xnor tdwpoxs(hud, hud, kjqnzw);
  
  cgdch f(.onhn(hud), .eejlqibaux(kjqnzw), .cgzpb(hud));
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic hud -> logic [3:0][4:3][2:2][4:3] onhn
  //
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic kjqnzw -> tri logic [0:2][0:1][3:2][1:1] eejlqibaux
  //
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic hud -> shortint cgzpb
  
  cgdch ypgugvfyk(.onhn(egwct), .eejlqibaux(p), .cgzpb(hud));
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  //   wire logic egwct -> logic [3:0][4:3][2:2][4:3] onhn
  //
  // warning: implicit conversion of port connection expands from 1 to 12 bits
  //   wire logic p -> tri logic [0:2][0:1][3:2][1:1] eejlqibaux
  //
  // warning: implicit conversion of port connection expands from 1 to 16 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic hud -> shortint cgzpb
  
  
  // Multi-driven assignments
  assign kjqnzw = p;
  assign qlsw = qlsw;
  assign p = egwct;
  assign y = y;
endmodule: gske



// Seed after: 13270877556004618131,14603487449988080319
