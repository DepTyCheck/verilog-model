-- Seed: 6345166771153151697,14426950258250697445

use std.reflection.all;

entity g is
  port (upk : in real; qgray : inout enumeration_value_mirror);
end g;

architecture bplcey of g is
  
begin
  
end bplcey;

library ieee;
use ieee.std_logic_1164.all;

entity emrtctdjhi is
  port (vdl : out std_logic_vector(1 downto 2); ug : out integer_vector(0 to 1));
end emrtctdjhi;

use std.reflection.all;

architecture ojgcgaffdq of emrtctdjhi is
  shared variable vxkgpocbu : enumeration_value_mirror;
  shared variable cke : enumeration_value_mirror;
  shared variable vcwr : enumeration_value_mirror;
  signal mi : real;
  shared variable pdbkme : enumeration_value_mirror;
  signal t : real;
begin
  yh : entity work.g
    port map (upk => t, qgray => pdbkme);
  mntimgbbqv : entity work.g
    port map (upk => mi, qgray => vcwr);
  qjlrrsne : entity work.g
    port map (upk => mi, qgray => cke);
  zdkmcwdovd : entity work.g
    port map (upk => t, qgray => vxkgpocbu);
  
  -- Single-driven assignments
  mi <= 0_0_0_1.4_4;
  t <= 16#4732.3_C_E_4#;
  ug <= (0_2_3_1, 2#011#);
end ojgcgaffdq;

use std.reflection.all;

entity fnb is
  port (yhwkdhllw : buffer time; tqtstcjywg : out character; drwg : linkage integer; neqf : inout physical_subtype_mirror);
end fnb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pe of fnb is
  shared variable ykuhrdk : enumeration_value_mirror;
  signal hajozdenwu : integer_vector(0 to 1);
  signal hxi : std_logic_vector(1 downto 2);
  shared variable o : enumeration_value_mirror;
  signal bo : real;
begin
  mqdm : entity work.g
    port map (upk => bo, qgray => o);
  trolkzjw : entity work.emrtctdjhi
    port map (vdl => hxi, ug => hajozdenwu);
  vtbcrl : entity work.g
    port map (upk => bo, qgray => ykuhrdk);
  
  -- Single-driven assignments
  tqtstcjywg <= tqtstcjywg;
  bo <= 2_0_0.1;
  yhwkdhllw <= yhwkdhllw;
end pe;



-- Seed after: 5479144391492037517,14426950258250697445
