// Seed: 15577985717778405823,3366587734033679655

module z (output logic [4:2][3:0][1:4][4:4] eqds, input supply0 logic [2:1][0:3][1:1] o [0:1], input uwire logic [1:4] i [2:3][3:4][3:0][4:0]);
  
  
  
  // Single-driven assigns
  assign eqds = '{'{'{'{'b0zxx0},'{'b011},'{'bz0z0z},'{'b0x0}},'{'{'b1},'{'b01001},'{'bzzzxx},'{'b111z0}},'{'{'bx11x},'{'bx},'{'bz},'{'bx}},'{'{'bz1z00},'{'b01},'{'bx},'{'bx000z}}},'{'{'{'b0z},'{'bzzz},'{'bx1},'{'bz01x0}},'{'{'bz},'{'b10},'{'b10xzx},'{'b0}},'{'{'b0},'{'b10},'{'b0z10},'{'b010z1}},'{'{'bx000},'{'bz00x0},'{'b101},'{'bxzzx}}},'{'{'{'bx11},'{'bx1zx1},'{'bx0zz},'{'b1xz}},'{'{'b0},'{'b0x10x},'{'b0z01},'{'bz}},'{'{'bxxx},'{'bz1x0},'{'b0zz},'{'bzz}},'{'{'bz10x},'{'bz},'{'bz10},'{'b00z00}}}};
  
  // Multi-driven assigns
  assign o = o;
endmodule: z



// Seed after: 18193451566276089497,3366587734033679655
