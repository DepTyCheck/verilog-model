// Seed: 3007056111798894923,10774379589804174675

module qp ( input wire logic [3:2][1:4][4:2][1:4] dlomxmx [0:1][0:0]
          , input supply0 logic [3:0][4:3][1:0][1:4] j [3:1]
          , input triand logic [0:3][2:0][1:0][4:1] y
          );
  
  xnor p(y, y, t);
  // warning: implicit conversion of port connection truncates from 96 to 1 bits
  //   triand logic [0:3][2:0][1:0][4:1] y -> logic y
  //
  // warning: implicit conversion of port connection truncates from 96 to 1 bits
  //   triand logic [0:3][2:0][1:0][4:1] y -> logic y
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign y = y;
  assign t = 'b0x;
  assign dlomxmx = dlomxmx;
  assign j = j;
endmodule: qp

module flvjsmmu (inout tri logic [4:2][1:0][4:4][1:1] ki, output reg [0:2] wslja, input reg xylqxjy, input trior logic qlnpibrtpe [0:0]);
  // Unpacked net declarations
  supply0 logic [3:0][4:3][1:0][1:4] aslbanwq [3:1];
  wire logic [3:2][1:4][4:2][1:4] lg [0:1][0:0];
  
  qp trlhmotngr(.dlomxmx(lg), .j(aslbanwq), .y(rtzmndvdcj));
  // warning: implicit conversion of port connection expands from 1 to 96 bits
  //   wire logic rtzmndvdcj -> triand logic [0:3][2:0][1:0][4:1] y
  
  not p(wslja, xylqxjy);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   reg [0:2] wslja -> logic wslja
  
  xnor orlviwr(rtzmndvdcj, wslja, injugrb);
  // warning: implicit conversion of port connection truncates from 3 to 1 bits
  //   reg [0:2] wslja -> logic wslja
  
  xnor fo(nmo, xylqxjy, nmo);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign rtzmndvdcj = xylqxjy;
  assign ki = '{'{'{'{'b1zx}},'{'{'b11}}},'{'{'{'bx10zx}},'{'{'b1xx}}},'{'{'{'b0x11}},'{'{'b0z}}}};
  assign qlnpibrtpe = qlnpibrtpe;
endmodule: flvjsmmu



// Seed after: 4226644363008873465,10774379589804174675
