// Seed: 1903289396505964078,9274975778887085201

module ngi (output integer bpn);
  xnor nt(bpn, bpn, yweupr);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   integer bpn -> logic bpn
  //
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   integer bpn -> logic bpn
  
  
  // Multi-driven assignments
  assign yweupr = 'b0;
  assign yweupr = 'bxz01;
endmodule: ngi

module glesgv (input reg lmldibiyro, output real jpvco, output bit [4:0][0:0] vrvf);
  and navru(vfpydp, vrvf, vrvf);
  // warning: implicit conversion of port connection truncates from 5 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:0][0:0] vrvf -> logic vrvf
  //
  // warning: implicit conversion of port connection truncates from 5 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [4:0][0:0] vrvf -> logic vrvf
  
  
  // Single-driven assignments
  assign jpvco = 'b10zz0;
  assign vrvf = vrvf;
  
  // Multi-driven assignments
  assign vfpydp = 'b0;
  assign vfpydp = lmldibiyro;
endmodule: glesgv

module hecws (input real anlz);
  ngi rfyvgmhtck(.bpn(othhk));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic othhk -> integer bpn
  
  glesgv xlo(.lmldibiyro(othhk), .jpvco(ge), .vrvf(asukz));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic ge -> real jpvco
  //
  // warning: implicit conversion of port connection expands from 1 to 5 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic asukz -> bit [4:0][0:0] vrvf
  
  
  // Multi-driven assignments
  assign othhk = ge;
  assign othhk = othhk;
  assign ge = 'bz1;
  assign ge = othhk;
endmodule: hecws



// Seed after: 15476364628285138226,9274975778887085201
