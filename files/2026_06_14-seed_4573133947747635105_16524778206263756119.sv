// Seed: 4573133947747635105,16524778206263756119

module jk (inout wire logic [3:4] plrqabbn [0:0][4:2][4:2][0:2]);
  
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign plrqabbn = plrqabbn;
endmodule: jk

module ko ();
  // Unpacked net declarations
  wire logic [3:4] kz [0:0][4:2][4:2][0:2];
  wire logic [3:4] kvmxn [0:0][4:2][4:2][0:2];
  
  jk eyumyzrvxm(.plrqabbn(kvmxn));
  
  jk s(.plrqabbn(kz));
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign kz = kvmxn;
  assign kvmxn = kvmxn;
endmodule: ko



// Seed after: 15742573966397393761,16524778206263756119
