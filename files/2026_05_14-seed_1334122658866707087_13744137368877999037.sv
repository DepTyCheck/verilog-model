// Seed: 1334122658866707087,13744137368877999037

module ewlysalbx ();
  
  xnor usjdhmk(xqb, xqb, wilsb);
  
  xor nnenuphrlj(e, emne, xqb);
  
  not eqmkpw(xqb, emne);
  
  xnor iodppizlvx(xqb, mm, mm);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign xqb = wilsb;
  assign e = mm;
  assign wilsb = e;
endmodule: ewlysalbx

module g ( output tri1 logic [4:3][1:4] joozqrzec [3:0][2:1][0:3][4:3]
         , input integer lfrlva
         , inout wire logic [4:1][2:3][3:2] ibsw [2:3][3:4][2:3]
         , output logic bzbxxppygl
         );
  
  nand su(us, mlqxvarp, elvoqzj);
  
  
  // Single-driven assigns
  assign bzbxxppygl = bzbxxppygl;
  
  // Multi-driven assigns
  assign ibsw = ibsw;
  assign mlqxvarp = us;
  assign joozqrzec = joozqrzec;
endmodule: g

module rbfeaf (inout wand logic [4:3][1:1][2:0][1:3] ep, input wand logic [1:0][4:0][1:2] oeem, inout trireg logic [4:0] vatgs);
  // Unpacked net declarations
  wire logic [4:1][2:3][3:2] igmjfisoar [2:3][3:4][2:3];
  tri1 logic [4:3][1:4] tmsdo [3:0][2:1][0:3][4:3];
  
  g fhstqp(.joozqrzec(tmsdo), .lfrlva(wazrjue), .ibsw(igmjfisoar), .bzbxxppygl(oeem));
  // warning: implicit conversion of port connection expands from 1 to 32 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic wazrjue -> integer lfrlva
  //
  // warning: implicit conversion of port connection truncates from 20 to 1 bits
  //   wand logic [1:0][4:0][1:2] oeem -> logic bzbxxppygl
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign wazrjue = 'b0x010;
endmodule: rbfeaf

module dol (input bit [2:3][4:1][1:0] f);
  
  xnor mxcndumhf(dpgwx, dpgwx, f);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:3][4:1][1:0] f -> logic f
  
  xor zkyexs(hzojp, pppy, blnm);
  
  nand yb(eyyvn, tjcy, f);
  // warning: implicit conversion of port connection truncates from 16 to 1 bits
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   bit [2:3][4:1][1:0] f -> logic f
  
  rbfeaf c(.ep(hzojp), .oeem(ez), .vatgs(eyyvn));
  // warning: implicit conversion of port connection expands from 1 to 18 bits
  //   wire logic hzojp -> wand logic [4:3][1:1][2:0][1:3] ep
  //
  // warning: implicit conversion of port connection expands from 1 to 20 bits
  //   wire logic ez -> wand logic [1:0][4:0][1:2] oeem
  //
  // warning: implicit conversion of port connection expands from 1 to 5 bits
  //   wire logic eyyvn -> trireg logic [4:0] vatgs
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign pppy = 'bzxz;
  assign hzojp = pppy;
  assign blnm = 'b1110;
  assign dpgwx = 'b0;
endmodule: dol



// Seed after: 12997907566729463657,13744137368877999037
