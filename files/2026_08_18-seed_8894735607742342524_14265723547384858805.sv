// Seed: 8894735607742342524,14265723547384858805

module ttm (input uwire logic [3:1][4:0] yvl);
  not qf(zd, yvl);
  // warning: implicit conversion of port connection truncates from 15 to 1 bits
  //   uwire logic [3:1][4:0] yvl -> logic yvl
  
  xnor gasdi(joosc, gwqmvv, yvl);
  // warning: implicit conversion of port connection truncates from 15 to 1 bits
  //   uwire logic [3:1][4:0] yvl -> logic yvl
  
  
  // Multi-driven assignments
  assign zd = zd;
  assign gwqmvv = zd;
  assign zd = 'bz;
  assign joosc = zd;
endmodule: ttm



// Seed after: 1417805599362055785,14265723547384858805
