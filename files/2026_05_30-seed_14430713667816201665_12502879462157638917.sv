// Seed: 14430713667816201665,12502879462157638917

module l ( output wand logic [0:4][1:0][0:4] fpobk
         , inout supply0 logic [3:1][2:2][1:4] frbvvrkoka [2:0][3:0][2:0][0:3]
         , output real xcprsbb
         , input logic [4:3][1:2][2:2] yllfwdbbb [1:3]
         );
  
  nand ea(jpeimf, xcprsbb, qgfhjts);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real xcprsbb -> logic xcprsbb
  
  and mbtedl(qgfhjts, igcbgjoiob, xcprsbb);
  // warning: implicit conversion of port connection truncates from 64 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  //   real xcprsbb -> logic xcprsbb
  
  xnor bvytnj(fpobk, o, qgfhjts);
  // warning: implicit conversion of port connection truncates from 50 to 1 bits
  //   wand logic [0:4][1:0][0:4] fpobk -> logic fpobk
  
  
  // Single-driven assigns
  assign xcprsbb = 'bz0;
  
  // Multi-driven assigns
  assign fpobk = fpobk;
  assign qgfhjts = 'b01x0;
  assign jpeimf = jpeimf;
  assign frbvvrkoka = frbvvrkoka;
endmodule: l

module flphpi ( output uwire logic [2:2][3:1][3:3] grixahscre [3:0][3:0]
              , output wor logic [0:2][0:1][3:1][3:1] tnkz [0:3][3:4][2:3][2:2]
              , output reg epzxyelukf [1:4]
              , output reg xf
              );
  // Unpacked net declarations
  logic [4:3][1:2][2:2] wl [1:3];
  supply0 logic [3:1][2:2][1:4] ppwdwmxofx [2:0][3:0][2:0][0:3];
  
  l wkobytwo(.fpobk(gwanbziddv), .frbvvrkoka(ppwdwmxofx), .xcprsbb(pb), .yllfwdbbb(wl));
  // warning: implicit conversion of port connection expands from 1 to 50 bits
  //   wire logic gwanbziddv -> wand logic [0:4][1:0][0:4] fpobk
  //
  // warning: implicit conversion of port connection expands from 1 to 64 bits
  // warning: implicit conversion changes signedness from unsigned to signed
  //   wire logic pb -> real xcprsbb
  
  xnor xhthzxvr(j, xf, iviqzy);
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign tnkz = tnkz;
  assign ppwdwmxofx = ppwdwmxofx;
  assign gwanbziddv = 'b1101z;
  assign iviqzy = 'bx00;
  assign pb = 'b000;
endmodule: flphpi



// Seed after: 4834863471340160736,12502879462157638917
