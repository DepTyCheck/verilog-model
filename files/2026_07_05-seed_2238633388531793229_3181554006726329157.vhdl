-- Seed: 2238633388531793229,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity bnjbhd is
  port (j : inout enumeration_value_mirror; ohram : in bit_vector(1 downto 2); p : in std_logic; zezqqcdv : inout access_subtype_mirror);
end bnjbhd;

architecture tebscobf of bnjbhd is
  
begin
  
end tebscobf;

use std.reflection.all;

entity rlmaphcbrs is
  port (ehlwmx : inout subtype_mirror);
end rlmaphcbrs;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture heerohsoa of rlmaphcbrs is
  shared variable sqkpvsbkrz : access_subtype_mirror;
  signal tgl : std_logic;
  signal ziex : bit_vector(1 downto 2);
  shared variable wltpklv : enumeration_value_mirror;
begin
  nhxmukn : entity work.bnjbhd
    port map (j => wltpklv, ohram => ziex, p => tgl, zezqqcdv => sqkpvsbkrz);
  
  -- Single-driven assignments
  ziex <= ziex;
  
  -- Multi-driven assignments
  tgl <= 'U';
  tgl <= 'L';
end heerohsoa;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity oerxlt is
  port (pvwk : inout std_logic; vfayidq : inout value_mirror);
end oerxlt;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture fxrwque of oerxlt is
  shared variable uimqbvmr : access_subtype_mirror;
  shared variable pqb : enumeration_value_mirror;
  shared variable cpexprsok : access_subtype_mirror;
  signal rrllfq : std_logic;
  signal vuewmjcxyj : bit_vector(1 downto 2);
  shared variable gwxgxmm : enumeration_value_mirror;
begin
  do : entity work.bnjbhd
    port map (j => gwxgxmm, ohram => vuewmjcxyj, p => rrllfq, zezqqcdv => cpexprsok);
  asbo : entity work.bnjbhd
    port map (j => pqb, ohram => vuewmjcxyj, p => pvwk, zezqqcdv => uimqbvmr);
  
  -- Single-driven assignments
  vuewmjcxyj <= vuewmjcxyj;
  
  -- Multi-driven assignments
  pvwk <= rrllfq;
end fxrwque;



-- Seed after: 11799053614295653271,3181554006726329157
