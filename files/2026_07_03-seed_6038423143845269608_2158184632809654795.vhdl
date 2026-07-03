-- Seed: 6038423143845269608,2158184632809654795

use std.reflection.all;

entity fxpl is
  port (rrbfx : buffer integer; gp : inout physical_value_mirror);
end fxpl;

architecture mofzhuo of fxpl is
  
begin
  -- Single-driven assignments
  rrbfx <= rrbfx;
end mofzhuo;

entity knuy is
  port (fjzu : linkage time_vector(0 downto 4));
end knuy;

use std.reflection.all;

architecture kf of knuy is
  shared variable qup : physical_value_mirror;
  signal rgccns : integer;
  shared variable gzzjds : physical_value_mirror;
  signal xllyjzgfpa : integer;
  shared variable nkvrdwdzu : physical_value_mirror;
  signal bxibzwc : integer;
begin
  aoqzwjl : entity work.fxpl
    port map (rrbfx => bxibzwc, gp => nkvrdwdzu);
  ldjmeogva : entity work.fxpl
    port map (rrbfx => xllyjzgfpa, gp => gzzjds);
  e : entity work.fxpl
    port map (rrbfx => rgccns, gp => qup);
end kf;

use std.reflection.all;

entity rahfbrsb is
  port (okksakbmgj : linkage real; joyq : inout subtype_mirror);
end rahfbrsb;

use std.reflection.all;

architecture tnvfktxmko of rahfbrsb is
  shared variable mriewn : physical_value_mirror;
  signal kojzyuoq : integer;
begin
  gyxnxet : entity work.fxpl
    port map (rrbfx => kojzyuoq, gp => mriewn);
end tnvfktxmko;

use std.reflection.all;

entity lpveseurj is
  port (ztafadzzpn : inout time; cew : inout physical_value_mirror; pulhk : inout protected_value_mirror);
end lpveseurj;

use std.reflection.all;

architecture kthrdimbmy of lpveseurj is
  shared variable gjmjyf : subtype_mirror;
  signal ffv : real;
  shared variable tibfxo : subtype_mirror;
  signal w : real;
begin
  pcbwc : entity work.rahfbrsb
    port map (okksakbmgj => w, joyq => tibfxo);
  sgguzkace : entity work.rahfbrsb
    port map (okksakbmgj => ffv, joyq => gjmjyf);
  
  -- Single-driven assignments
  ztafadzzpn <= 8#1.6_4_5# ms;
end kthrdimbmy;



-- Seed after: 13262797415924793416,2158184632809654795
