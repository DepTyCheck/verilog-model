-- Seed: 17318139051764209282,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;

entity iuxfhddap is
  port (rm : buffer std_logic);
end iuxfhddap;

architecture ubscuyi of iuxfhddap is
  
begin
  -- Multi-driven assignments
  rm <= 'X';
  rm <= rm;
  rm <= 'U';
  rm <= rm;
end ubscuyi;

use std.reflection.all;

entity nyn is
  port (variable wbgmmdlg : inout subtype_mirror_pt; variable tu : inout physical_value_mirror_pt);
end nyn;

library ieee;
use ieee.std_logic_1164.all;

architecture f of nyn is
  signal tankgapy : std_logic;
  signal vb : std_logic;
begin
  kepibjxxfp : entity work.iuxfhddap
    port map (rm => vb);
  tjfef : entity work.iuxfhddap
    port map (rm => vb);
  pzvftaoij : entity work.iuxfhddap
    port map (rm => tankgapy);
end f;



-- Seed after: 5570309894678522147,2983771601630957889
