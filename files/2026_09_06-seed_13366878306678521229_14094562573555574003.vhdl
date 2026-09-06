-- Seed: 13366878306678521229,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity blvn is
  port (om : in std_logic_vector(4 downto 4); xvoojd : inout integer; gcioy : linkage std_logic_vector(3 to 2));
end blvn;

architecture siqciyt of blvn is
  
begin
  -- Single-driven assignments
  xvoojd <= xvoojd;
end siqciyt;

entity ybpk is
  port (tjgpoxsii : buffer time_vector(2 downto 0); miya : in time; voivimxh : buffer integer; ohiadgj : buffer integer);
end ybpk;

library ieee;
use ieee.std_logic_1164.all;

architecture h of ybpk is
  signal pqrd : std_logic_vector(3 to 2);
  signal sy : std_logic_vector(4 downto 4);
begin
  tamwcabqfh : entity work.blvn
    port map (om => sy, xvoojd => ohiadgj, gcioy => pqrd);
  iyxesie : entity work.blvn
    port map (om => sy, xvoojd => voivimxh, gcioy => pqrd);
  
  -- Single-driven assignments
  tjgpoxsii <= (3 min, 16#C1A3B# fs, 0_3_4_2_0.0_0 ns);
  
  -- Multi-driven assignments
  sy <= sy;
end h;



-- Seed after: 8325125925610587311,14094562573555574003
