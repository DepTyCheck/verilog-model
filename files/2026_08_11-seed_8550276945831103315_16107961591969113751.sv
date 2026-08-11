// Seed: 8550276945831103315,16107961591969113751

module g ( input bit [3:0] nfysvtx
         , inout supply0 logic [2:4][1:3] n [1:1][3:1]
         , inout supply0 logic [3:2][3:0][0:2][1:2] c [0:0]
         , output triand logic [4:3][2:3][4:3][3:3] z [4:3][3:3]
         );
  nand ty(fluo, nfysvtx, fluo);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:0] nfysvtx -> logic nfysvtx
  
  or xaorj(fluo, nfysvtx, fluo);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:0] nfysvtx -> logic nfysvtx
  
  xnor tjomjwf(cuzszwglku, nfysvtx, hybv);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:0] nfysvtx -> logic nfysvtx
  
  nand q(fluo, esign, nfysvtx);
  // warning: implicit conversion of port connection truncates from 4 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [3:0] nfysvtx -> logic nfysvtx
  
endmodule: g



// Seed after: 5459561792230578433,16107961591969113751
