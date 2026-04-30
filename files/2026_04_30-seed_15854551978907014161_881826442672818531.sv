// Seed: 15854551978907014161,881826442672818531

module rdei (input bit [3:1] sncxwdegvp [0:3], input logic [0:0] beeigj [1:3], output bit [2:2][2:1][1:4][0:0] y);
  
  xor lo(mxq, wtvywu, y);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:2][2:1][1:4][0:0] y -> logic y
  
  and le(smjli, y, lion);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:2][2:1][1:4][0:0] y -> logic y
  
  xnor ar(usddowwhoq, y, etxinx);
  // warning: implicit conversion of port connection truncates from 8 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:2][2:1][1:4][0:0] y -> logic y
  
  
  // Single-driven assigns
  assign y = y;
  
  // Multi-driven assigns
  assign mxq = 'b100z1;
  assign lion = usddowwhoq;
  assign usddowwhoq = wtvywu;
  assign wtvywu = etxinx;
endmodule: rdei

module jonqdll (input wire logic [4:1][2:4] xjsiaehl, output reg [2:1][3:3][0:1] epvxyu, output tri0 logic [0:3][0:1] wfjbeezico [4:2][3:2][2:1]);
  // Unpacked net declarations
  bit [3:1] zybf [0:3];
  logic [0:0] m [1:3];
  bit [3:1] oeabrhibv [0:3];
  
  rdei ailmmv(.sncxwdegvp(oeabrhibv), .beeigj(m), .y(vzskti));
  // warning: implicit conversion of port connection expands from 1 to 8 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic vzskti -> bit [2:2][2:1][1:4][0:0] y
  
  xor xqtdnaevzt(epvxyu, f, xzmyved);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   reg [2:1][3:3][0:1] epvxyu -> logic epvxyu
  
  rdei g(.sncxwdegvp(zybf), .beeigj(m), .y(xjsiaehl));
  // warning: implicit conversion of port connection truncates from 12 to 8 bits
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic [4:1][2:4] xjsiaehl -> bit [2:2][2:1][1:4][0:0] y
  
  xnor dhiigshq(f, epvxyu, epvxyu);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   reg [2:1][3:3][0:1] epvxyu -> logic epvxyu
  //
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  //   reg [2:1][3:3][0:1] epvxyu -> logic epvxyu
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
endmodule: jonqdll



// Seed after: 3755090394715030847,881826442672818531
