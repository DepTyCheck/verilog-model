// Seed: 11266970013324271777,16416852235058217195

module tlje (inout tri0 logic [2:4] os [2:2][3:0][2:3]);
  
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign os = '{'{'{'{'b0,'bx0z,'bxz10x},'{'bz,'bxz0,'bx100}},'{'{'b00,'bxz0z,'b1z},'{'bz,'bxz,'b1}},'{'{'bzx0,'b0zzx1,'bxz},'{'bxx0z0,'b0110,'bz0x}},'{'{'bx0,'b00,'b10},'{'b0,'b1,'b000x}}}};
endmodule: tlje

module keccndzz ( inout supply0 logic [2:1][2:3][0:3][2:3] zhtsrnppws
                , output supply0 logic hmllqgn [0:2][0:3][0:3]
                , output supply1 logic ayq [4:3][3:4]
                , input tri logic aqrbiscbjo [0:4][4:0]
                );
  
  nand sbojrpvikv(azxylp, azxylp, yyey);
  
  xnor kfntizc(ucujauiey, olaad, azxylp);
  
  nand lmwblgdsv(q, q, ucujauiey);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign q = 'bzx1;
endmodule: keccndzz

module av (output real hyuywk [0:0][3:3], output time dcowjsufc [3:2]);
  // Unpacked net declarations
  tri0 logic [2:4] ysnp [2:2][3:0][2:3];
  tri logic ulazcxv [0:4][4:0];
  supply1 logic kvw [4:3][3:4];
  supply0 logic gnwlg [0:2][0:3][0:3];
  tri logic ii [0:4][4:0];
  supply1 logic uorlxxp [4:3][3:4];
  supply0 logic fxcp [0:2][0:3][0:3];
  
  xor igacsgzwpz(cql, s, aqdhypjc);
  
  keccndzz fhigyayi(.zhtsrnppws(steuh), .hmllqgn(fxcp), .ayq(uorlxxp), .aqrbiscbjo(ii));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  //   wire logic steuh -> supply0 logic [2:1][2:3][0:3][2:3] zhtsrnppws
  
  keccndzz zveikdkif(.zhtsrnppws(steuh), .hmllqgn(gnwlg), .ayq(kvw), .aqrbiscbjo(ulazcxv));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  //   wire logic steuh -> supply0 logic [2:1][2:3][0:3][2:3] zhtsrnppws
  
  tlje tdejji(.os(ysnp));
  
  
  // Single-driven assigns
  assign dcowjsufc = dcowjsufc;
  assign hyuywk = hyuywk;
  
  // Multi-driven assigns
endmodule: av



// Seed after: 12807856353305907048,16416852235058217195
