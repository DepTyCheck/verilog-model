// Seed: 12101935056771330253,3366587734033679655

module grbun ( input supply0 logic [4:1][4:2][0:3] z
             , input reg otyphoyw
             , input uwire logic [1:2] tnzdwgq [2:3][0:2]
             , output logic [0:3][2:3] iizhr [2:0]
             );
  
  not mpoa(z, wn);
  // warning: implicit conversion of port connection truncates from 48 to 1 bits
  //   supply0 logic [4:1][4:2][0:3] z -> logic z
  
  
  // Single-driven assigns
  assign iizhr = iizhr;
  
  // Multi-driven assigns
  assign z = z;
  assign wn = wn;
endmodule: grbun

module uatdvyvwg (output bit [3:2][3:4][2:1][2:4] bijpjr);
  // Unpacked net declarations
  logic [0:3][2:3] mnfjhwlze [2:0];
  uwire logic [1:2] veydxecs [2:3][0:2];
  
  grbun h(.z(bijpjr), .otyphoyw(hme), .tnzdwgq(veydxecs), .iizhr(mnfjhwlze));
  // warning: implicit conversion of port connection expands from 24 to 48 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:2][3:4][2:1][2:4] bijpjr -> supply0 logic [4:1][4:2][0:3] z
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign hme = hme;
endmodule: uatdvyvwg

module phczevtwor ();
  // Unpacked net declarations
  logic [0:3][2:3] diexavd [2:0];
  uwire logic [1:2] vcpmym [2:3][0:2];
  
  grbun asrxshwfwo(.z(xxq), .otyphoyw(qp), .tnzdwgq(vcpmym), .iizhr(diexavd));
  // warning: implicit conversion of port connection expands from 1 to 48 bits
  //   wire logic xxq -> supply0 logic [4:1][4:2][0:3] z
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xxq = 'b0z111;
  assign qp = 'bxx;
endmodule: phczevtwor



// Seed after: 1161351700250627107,3366587734033679655
