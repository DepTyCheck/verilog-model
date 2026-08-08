-- Seed: 10630640075131102186,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity co is
  port (gkmhyrnbh : out integer; eknimiuzjq : in std_logic; tdw : inout time);
end co;

architecture d of co is
  
begin
  -- Single-driven assignments
  tdw <= 8#2_7# ns;
  gkmhyrnbh <= 8#451#;
end d;

library ieee;
use ieee.std_logic_1164.all;

entity yimxxsyo is
  port (i : in bit; liwdp : linkage real; hqrwuvot : in time; oqagfu : buffer std_logic);
end yimxxsyo;

library ieee;
use ieee.std_logic_1164.all;

architecture sbjwbbv of yimxxsyo is
  signal o : time;
  signal rnirhwme : std_logic;
  signal kzowyt : integer;
  signal iujrqtyjoj : time;
  signal vn : integer;
  signal rktpqcitoh : time;
  signal fctsyroc : std_logic;
  signal shakdsxah : integer;
begin
  vtlhx : entity work.co
    port map (gkmhyrnbh => shakdsxah, eknimiuzjq => fctsyroc, tdw => rktpqcitoh);
  ftausia : entity work.co
    port map (gkmhyrnbh => vn, eknimiuzjq => fctsyroc, tdw => iujrqtyjoj);
  z : entity work.co
    port map (gkmhyrnbh => kzowyt, eknimiuzjq => rnirhwme, tdw => o);
  
  -- Multi-driven assignments
  rnirhwme <= oqagfu;
  oqagfu <= 'W';
  fctsyroc <= 'U';
end sbjwbbv;



-- Seed after: 492838424738544404,8927267689619684183
