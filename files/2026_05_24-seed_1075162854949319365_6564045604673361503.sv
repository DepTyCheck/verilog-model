// Seed: 1075162854949319365,6564045604673361503

module m (output int sgbithh);
  
  xnor dl(sgbithh, mmel, sgbithh);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   int sgbithh -> logic sgbithh
  //
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   int sgbithh -> logic sgbithh
  
  xor ykp(dtk, f, dtk);
  
  or en(mmel, sgbithh, f);
  // warning: implicit conversion of port connection truncates from 32 to 1 bits
  // warning: implicit conversion changes signedness from signed to unsigned
  // warning: implicit conversion changes possible bit states from 2-state to 4-state
  //   int sgbithh -> logic sgbithh
  
  
  // Single-driven assigns
  
  // Multi-driven assigns
  assign mmel = f;
  assign dtk = mmel;
endmodule: m



// Seed after: 12352671901321925803,6564045604673361503
