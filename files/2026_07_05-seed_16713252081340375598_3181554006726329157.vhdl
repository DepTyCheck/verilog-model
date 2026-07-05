-- Seed: 16713252081340375598,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jlai is
  port (vpmd : inout integer_value_mirror; hab : in std_logic; n : inout physical_value_mirror; obtpnqwx : inout physical_subtype_mirror);
end jlai;

architecture xaooaq of jlai is
  
begin
  
end xaooaq;

entity sby is
  port (ihazdd : buffer time);
end sby;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wusmmvl of sby is
  shared variable lmnqavaif : physical_subtype_mirror;
  shared variable xfz : physical_value_mirror;
  shared variable ozbki : integer_value_mirror;
  shared variable owxqzz : physical_subtype_mirror;
  shared variable m : physical_value_mirror;
  signal yqq : std_logic;
  shared variable gcthjkde : integer_value_mirror;
  shared variable wc : physical_subtype_mirror;
  shared variable fbtzjsc : physical_value_mirror;
  signal uzm : std_logic;
  shared variable fsbxj : integer_value_mirror;
begin
  mxx : entity work.jlai
    port map (vpmd => fsbxj, hab => uzm, n => fbtzjsc, obtpnqwx => wc);
  aove : entity work.jlai
    port map (vpmd => gcthjkde, hab => yqq, n => m, obtpnqwx => owxqzz);
  am : entity work.jlai
    port map (vpmd => ozbki, hab => uzm, n => xfz, obtpnqwx => lmnqavaif);
  
  -- Single-driven assignments
  ihazdd <= ihazdd;
  
  -- Multi-driven assignments
  uzm <= uzm;
  uzm <= uzm;
  uzm <= '1';
  uzm <= uzm;
end wusmmvl;

library ieee;
use ieee.std_logic_1164.all;

entity tnqc is
  port (tmdjl : out std_logic; jpi : out time_vector(2 downto 3); ag : inout real);
end tnqc;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture jvtjnl of tnqc is
  signal tmephgkd : time;
  shared variable qucq : physical_subtype_mirror;
  shared variable pcctqeoerv : physical_value_mirror;
  signal qnvtcfm : std_logic;
  shared variable hiomhael : integer_value_mirror;
  shared variable c : physical_subtype_mirror;
  shared variable qlgjmiv : physical_value_mirror;
  signal rlqkpt : std_logic;
  shared variable jwpqbn : integer_value_mirror;
begin
  eojehyokys : entity work.jlai
    port map (vpmd => jwpqbn, hab => rlqkpt, n => qlgjmiv, obtpnqwx => c);
  fhljaqkte : entity work.jlai
    port map (vpmd => hiomhael, hab => qnvtcfm, n => pcctqeoerv, obtpnqwx => qucq);
  ygimol : entity work.sby
    port map (ihazdd => tmephgkd);
  
  -- Single-driven assignments
  ag <= ag;
  jpi <= (others => 0 ns);
end jvtjnl;



-- Seed after: 17528395201554367824,3181554006726329157
