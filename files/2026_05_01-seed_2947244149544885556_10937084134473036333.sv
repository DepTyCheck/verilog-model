// Seed: 2947244149544885556,10937084134473036333

module nerh ( output logic [4:1][4:2][4:2][1:0] x
            , input tri logic n [0:1][2:1]
            , input trireg logic [4:3][0:2] hfwt [2:3]
            , input tri1 logic [0:4][1:3][4:0][2:1] blhkkjec [4:0][3:0][0:2]
            );
  
  
  
  // Single-driven assigns
  assign x = '{'{'{'{'b11x1,'bx0x0},'{'bzx01,'bx1x01},'{'bz010,'b00z}},'{'{'b1x1,'bxzzz1},'{'bx00,'b1z001},'{'bx0,'b00}},'{'{'b11zx,'b0},'{'bzx0z1,'b0z},'{'bz,'bx01xz}}},'{'{'{'bzz1,'bz},'{'b0,'b1},'{'bzx,'bxz00}},'{'{'b10z1,'bx1z},'{'b10x0,'bx01x},'{'bz0zx,'b0zx}},'{'{'bzxxxz,'bz0},'{'bxz,'b0z},'{'bzz1,'bzx1xz}}},'{'{'{'bxz,'b01},'{'bzx1z1,'bxzzzx},'{'bz1z,'b10}},'{'{'bz1xx,'b0},'{'b1z1,'bz0zx},'{'bzz1,'bx1}},'{'{'b101,'b11},'{'b1x1,'bzx10},'{'bz10z,'bz1x}}},'{'{'{'bxxz,'bz0xzz},'{'bz1z0z,'b1x},'{'b0zz,'b0z1}},'{'{'bzzz,'bxx1},'{'b1,'bz0},'{'b0z01,'bxx1}},'{'{'bxzz0z,'bx001x},'{'b10z,'b0zx0x},'{'bz,'bx10}}}};
  
  // Multi-driven assigns
  assign blhkkjec = blhkkjec;
  assign hfwt = '{'{'{'bxx1,'bxxzz,'b1x01},'{'bz0,'b1zzz1,'bx0x}},'{'{'b010,'b1xz0,'bz},'{'b0,'bx,'bx}}};
  assign n = '{'{'bxx,'bx},'{'b1,'bxxx}};
endmodule: nerh

module huqgdai (input int tixqsp);
  // Unpacked net declarations
  tri1 logic [0:4][1:3][4:0][2:1] edk [4:0][3:0][0:2];
  trireg logic [4:3][0:2] uhdgnyhrtl [2:3];
  tri logic wiunzhqxh [0:1][2:1];
  
  and t(iqud, gavzz, ghupteprsl);
  
  xnor buiyf(embcweck, gavzz, xwnriepu);
  
  nerh aisv(.x(iqud), .n(wiunzhqxh), .hfwt(uhdgnyhrtl), .blhkkjec(edk));
  // warning: implicit conversion of port connection expands from 1 to 72 bits
  //   wire logic iqud -> logic [4:1][4:2][4:2][1:0] x
  
  xor qupdt(fwhvfnuey, nljzx, tgiw);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xwnriepu = iqud;
  assign uhdgnyhrtl = '{'{'{'bzz0x,'b1zxz1,'b0010z},'{'b00zz,'b0zx,'b0z1}},'{'{'bz,'b10x0,'b1},'{'bx,'b1,'b1x0}}};
  assign ghupteprsl = 'bx01;
  assign tgiw = iqud;
  assign iqud = ghupteprsl;
endmodule: huqgdai

module my ( output bit [0:1][2:3] ehftgzlq [0:4]
          , output tri1 logic [1:4][3:1][4:1][2:3] hqnucobse [3:4][2:3]
          , output logic [2:4] gjxugya
          , inout triand logic [1:2][2:1][0:2][1:1] oui [4:3]
          );
  
  huqgdai zg(.tixqsp(q));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  // warning: implicit conversion changes possible bit states from 4-state to 2-state
  //   wire logic q -> int tixqsp
  
  
  // Single-driven assigns
  assign ehftgzlq = '{'{'{'b10011,'b11000},'{'b100,'b0}},'{'{'b1100,'b1},'{'b011,'b01010}},'{'{'b10,'b11},'{'b011,'b0011}},'{'{'b0100,'b0},'{'b010,'b10100}},'{'{'b100,'b10010},'{'b000,'b010}}};
  assign gjxugya = '{'b11xxx,'b1,'b1xx};
  
  // Multi-driven assigns
  assign oui = oui;
  assign q = q;
  assign hqnucobse = hqnucobse;
endmodule: my



// Seed after: 5938201571386207315,10937084134473036333
