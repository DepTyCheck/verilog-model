// Seed: 14749952735702694708,15707698741854944441

module akwf (output bit [2:1][0:0][4:4] j);
  nand jcpdp(j, zhel, sdsjcqc);
  // warning: implicit conversion of port connection truncates from 2 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:1][0:0][4:4] j -> logic j
  
  
  // Multi-driven assignments
  assign zhel = 'bx1z;
  assign zhel = zhel;
  assign sdsjcqc = zhel;
  assign zhel = zhel;
endmodule: akwf

module vaa (output bit [1:0][4:0] dlgdbowg, input trireg logic [1:2][2:0] wrolawna [2:4][2:4][1:3][3:1]);
  xnor lyyznnpbm(ejkrte, dlgdbowg, updrlmw);
  // warning: implicit conversion of port connection truncates from 10 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [1:0][4:0] dlgdbowg -> logic dlgdbowg
  
  
  // Single-driven assignments
  assign dlgdbowg = dlgdbowg;
  
  // Multi-driven assignments
  assign wrolawna = wrolawna;
endmodule: vaa

module exuxb (input int utgvhvjf [1:3], input shortreal szbqcooato [4:1][1:3], output realtime qek);
  akwf sdstipfoj(.j(quwzncqvgm));
  // warning: implicit conversion of port connection expands from 1 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic quwzncqvgm -> bit [2:1][0:0][4:4] j
  
  
  // Single-driven assignments
  assign qek = 'b111xxzzxx01x01101xxzz0x0zzzx1z0zxxx10000z00xzxx1z0zzzxzx010zx000;
  
  // Multi-driven assignments
  assign quwzncqvgm = 'b0z101;
  assign quwzncqvgm = quwzncqvgm;
  assign quwzncqvgm = 'bz0z0z;
endmodule: exuxb

module rrpvjj (output logic [0:0][3:4][3:4] mg, inout trireg logic [4:0] ivdwf, inout wire logic [4:2][3:1][0:1][4:3] otdvznl);
  // Unpacked net declarations
  trireg logic [1:2][2:0] keahg [2:4][2:4][1:3][3:1];
  
  nand cs(nwk, rgka, mg);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   logic [0:0][3:4][3:4] mg -> logic mg
  
  akwf mb(.j(mg));
  // warning: implicit conversion of port connection truncates from 4 to 2 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   logic [0:0][3:4][3:4] mg -> bit [2:1][0:0][4:4] j
  
  vaa ievbu(.dlgdbowg(gn), .wrolawna(keahg));
  // warning: implicit conversion of port connection expands from 1 to 10 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic gn -> bit [1:0][4:0] dlgdbowg
  
  
  // Multi-driven assignments
  assign otdvznl = 'b1z1xzx101z11xz0xx0z1xzxz101110xx1z01;
  assign rgka = 'bx0z00;
endmodule: rrpvjj



// Seed after: 4315270130177129286,15707698741854944441
