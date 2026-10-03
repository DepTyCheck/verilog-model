// Seed: 6403256887211732163,12544753810326529141

module mfpqoxln ( input longint k [2:2][4:0][3:4]
                , input wor logic [4:4][2:0][4:0][1:3] xou
                , inout trireg logic [0:1] flvctdqjz [4:0]
                , output reg xhhxqgnm
                );
  xor fdlql(xhhxqgnm, cdifjnjodf, dw);
  
  nand dhjrm(dw, xhhxqgnm, cdifjnjodf);
  
  xnor xgddbvonb(xou, dw, cgtdcu);
  // warning: implicit conversion of port connection truncates from 45 to 1 bits
  //   wor logic [4:4][2:0][4:0][1:3] xou -> logic xou
  
endmodule: mfpqoxln

module wybhde ();
  // Unpacked net declarations
  trireg logic [0:1] inmwl [4:0];
  longint kajsn [2:2][4:0][3:4];
  
  xnor ta(sng, sng, dlspht);
  
  mfpqoxln oakebs(.k(kajsn), .xou(sng), .flvctdqjz(inmwl), .xhhxqgnm(sng));
  // warning: implicit conversion of port connection expands from 1 to 45 bits
  //   wire logic sng -> wor logic [4:4][2:0][4:0][1:3] xou
  
  
  // Single-driven assignments
  assign kajsn = '{'{'{'b0110001101000100011000011011110110001000011010111111101110000101,'b1111110001011101111011001100100000011001110110101100100111010011},'{'b0000011000100001001111001100110001100011001100010000110111110011,'b11001},'{'b000,'b101},'{'b010,'b0000000011010010110001111010001100100100000111101011101000110001},'{'b0110011110001010110100011110110010101111111000001100010101111111,'b0010000001100110100010011000101100001011010001001000100011100100}}};
endmodule: wybhde



// Seed after: 8863781514157275574,12544753810326529141
