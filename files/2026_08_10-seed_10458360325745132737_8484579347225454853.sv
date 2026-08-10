// Seed: 10458360325745132737,8484579347225454853

module twxm (output realtime hjpilme, input realtime tuxynzt [0:3]);
  xnor fxmx(mcdtbtzp, ktqhsukbu, hjpilme);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime hjpilme -> logic hjpilme
  
  xor zfcdjxsg(ktqhsukbu, sidjrrolbf, hyxmfo);
  
  nand f(nzqplzx, jyd, hjpilme);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   realtime hjpilme -> logic hjpilme
  
  
  // Multi-driven assignments
  assign mcdtbtzp = 'bx0x;
endmodule: twxm

module eszit (output bit jn, input bit [3:2] fhcexlbwkx [3:1]);
  // Unpacked net declarations
  realtime lnmcoic [0:3];
  
  xnor cwqyieg(jn, zjzdxh, zjzdxh);
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit jn -> logic jn
  
  twxm qzxujs(.hjpilme(neaflsr), .tuxynzt(lnmcoic));
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic neaflsr -> realtime hjpilme
  
  not ksyhip(zjzdxh, neuq);
  
  
  // Single-driven assignments
  assign lnmcoic = lnmcoic;
  
  // Multi-driven assignments
  assign zjzdxh = 'b1;
endmodule: eszit

module xp ( inout tri0 logic [0:1][3:4] zmkohtaqta [0:1][0:2]
          , output wor logic ohutq [4:1][2:2]
          , input wor logic [0:2][3:2][4:0][3:0] haquea [1:1][2:2][0:2]
          , input supply1 logic fcd [4:2][4:2]
          );
  // Unpacked net declarations
  bit [3:2] svwjbxyfa [3:1];
  
  and vgdvcud(zsahfps, zsahfps, l);
  
  eszit id(.jn(zsahfps), .fhcexlbwkx(svwjbxyfa));
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic zsahfps -> bit jn
  
  
  // Single-driven assignments
  assign svwjbxyfa = '{'{'b000,'b11},'{'b0,'b1},'{'b0111,'b11110}};
  
  // Multi-driven assignments
  assign fcd = '{'{'bz1x1,'b11,'b0},'{'bx,'b00x,'b1xx},'{'b01,'bz1z,'b00zz}};
endmodule: xp



// Seed after: 14690228133482938141,8484579347225454853
