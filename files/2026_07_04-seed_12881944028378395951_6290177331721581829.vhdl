-- Seed: 12881944028378395951,6290177331721581829

use std.reflection.all;

entity pukxhcs is
  port (oqpdzp : inout physical_value_mirror; ytja : inout floating_subtype_mirror);
end pukxhcs;

architecture ks of pukxhcs is
  
begin
  
end ks;

use std.reflection.all;

entity k is
  port (gz : linkage integer_vector(4 to 4); g : inout physical_value_mirror);
end k;

use std.reflection.all;

architecture cvnpoetd of k is
  shared variable rtorphc : floating_subtype_mirror;
  shared variable vlcmkzjcr : floating_subtype_mirror;
  shared variable fpoeywexg : physical_value_mirror;
  shared variable fephfq : floating_subtype_mirror;
  shared variable mk : physical_value_mirror;
  shared variable sutr : floating_subtype_mirror;
  shared variable pukpqkay : physical_value_mirror;
begin
  wazau : entity work.pukxhcs
    port map (oqpdzp => pukpqkay, ytja => sutr);
  poqllcrwtv : entity work.pukxhcs
    port map (oqpdzp => mk, ytja => fephfq);
  attgej : entity work.pukxhcs
    port map (oqpdzp => fpoeywexg, ytja => vlcmkzjcr);
  pqhvmenrm : entity work.pukxhcs
    port map (oqpdzp => g, ytja => rtorphc);
end cvnpoetd;

entity t is
  port (aawmprykeg : in time);
end t;

use std.reflection.all;

architecture czzjvujqkl of t is
  shared variable vrtc : physical_value_mirror;
  signal vjnu : integer_vector(4 to 4);
begin
  uvchxnfoex : entity work.k
    port map (gz => vjnu, g => vrtc);
end czzjvujqkl;



-- Seed after: 2165612807002687711,6290177331721581829
