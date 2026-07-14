-- Seed: 7066181247128935804,7726014785203345639

use std.reflection.all;

entity qsci is
  port (jojitjnlj : inout access_subtype_mirror; ybwg : inout enumeration_value_mirror; yvojdsutb : inout record_subtype_mirror; ahrfpmyzp : inout bit);
end qsci;

architecture dbxqjor of qsci is
  
begin
  -- Single-driven assignments
  ahrfpmyzp <= '1';
end dbxqjor;

use std.reflection.all;

entity cbmtgubt is
  port (cvmpiqoi : inout record_subtype_mirror);
end cbmtgubt;

use std.reflection.all;

architecture vvzg of cbmtgubt is
  signal ny : bit;
  shared variable glkv : record_subtype_mirror;
  shared variable qrygf : enumeration_value_mirror;
  shared variable fwzcjdivu : access_subtype_mirror;
begin
  kttqsewg : entity work.qsci
    port map (jojitjnlj => fwzcjdivu, ybwg => qrygf, yvojdsutb => glkv, ahrfpmyzp => ny);
end vvzg;

use std.reflection.all;

entity bxpvsuj is
  port (xr : inout integer_value_mirror; hidror : inout enumeration_subtype_mirror);
end bxpvsuj;

architecture itct of bxpvsuj is
  
begin
  
end itct;

use std.reflection.all;

entity jokv is
  port (agwuitedrv : out time_vector(2 downto 2); n : inout enumeration_subtype_mirror; h : inout record_subtype_mirror);
end jokv;

use std.reflection.all;

architecture lgcnv of jokv is
  signal tqqvx : bit;
  shared variable urxikylay : record_subtype_mirror;
  shared variable hmannfqezk : enumeration_value_mirror;
  shared variable qeecbsh : access_subtype_mirror;
  signal hhnkbakzsl : bit;
  shared variable q : record_subtype_mirror;
  shared variable vypdgsoh : enumeration_value_mirror;
  shared variable euizjvgj : access_subtype_mirror;
begin
  dqstmogumy : entity work.qsci
    port map (jojitjnlj => euizjvgj, ybwg => vypdgsoh, yvojdsutb => q, ahrfpmyzp => hhnkbakzsl);
  ywfvbgipp : entity work.qsci
    port map (jojitjnlj => qeecbsh, ybwg => hmannfqezk, yvojdsutb => urxikylay, ahrfpmyzp => tqqvx);
  
  -- Single-driven assignments
  agwuitedrv <= agwuitedrv;
end lgcnv;



-- Seed after: 8720484992809580622,7726014785203345639
