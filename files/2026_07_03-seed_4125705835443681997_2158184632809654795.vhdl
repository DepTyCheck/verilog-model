-- Seed: 4125705835443681997,2158184632809654795

entity dgjtxpd is
  port (xp : in integer_vector(0 to 3); cwo : linkage severity_level);
end dgjtxpd;

architecture mszficlxm of dgjtxpd is
  
begin
  
end mszficlxm;

use std.reflection.all;

entity tyo is
  port (rdwe : inout floating_subtype_mirror; dz : inout physical_value_mirror; fhaz : inout record_subtype_mirror);
end tyo;

architecture bjl of tyo is
  
begin
  
end bjl;

use std.reflection.all;

entity qastrry is
  port (rbdkhgyef : inout access_subtype_mirror; hvkvvx : inout integer_subtype_mirror);
end qastrry;

use std.reflection.all;

architecture zzllohaqf of qastrry is
  signal emtwfgnx : severity_level;
  signal gu : integer_vector(0 to 3);
  shared variable mrpisslfsj : record_subtype_mirror;
  shared variable hnpebq : physical_value_mirror;
  shared variable bvysfw : floating_subtype_mirror;
  signal eyvwb : severity_level;
  signal borkuzhylv : severity_level;
  signal spbv : integer_vector(0 to 3);
begin
  vzeujg : entity work.dgjtxpd
    port map (xp => spbv, cwo => borkuzhylv);
  bjbkfxeria : entity work.dgjtxpd
    port map (xp => spbv, cwo => eyvwb);
  qlptxddc : entity work.tyo
    port map (rdwe => bvysfw, dz => hnpebq, fhaz => mrpisslfsj);
  iw : entity work.dgjtxpd
    port map (xp => gu, cwo => emtwfgnx);
  
  -- Single-driven assignments
  spbv <= (2444, 2_3, 22, 3_1_2_1_3);
end zzllohaqf;



-- Seed after: 2423333533234580816,2158184632809654795
