-- Seed: 3745188869689855094,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity efrdda is
  port (vcxwvnx : inout time_vector(0 to 1); l : out time; mn : buffer real; cwdyh : linkage std_logic_vector(4 downto 2));
end efrdda;

architecture dps of efrdda is
  
begin
  -- Single-driven assignments
  mn <= 3044.3;
end dps;

library ieee;
use ieee.std_logic_1164.all;

entity bkkfaic is
  port (x : inout time; bcr : in std_logic_vector(4 downto 2));
end bkkfaic;

library ieee;
use ieee.std_logic_1164.all;

architecture kwownkvqw of bkkfaic is
  signal dasdhs : std_logic_vector(4 downto 2);
  signal ao : real;
  signal cjgem : time_vector(0 to 1);
  signal kwnmxwpgb : std_logic_vector(4 downto 2);
  signal vmgyjcozyw : real;
  signal d : time;
  signal an : time_vector(0 to 1);
begin
  zdz : entity work.efrdda
    port map (vcxwvnx => an, l => d, mn => vmgyjcozyw, cwdyh => kwnmxwpgb);
  xurtpc : entity work.efrdda
    port map (vcxwvnx => cjgem, l => x, mn => ao, cwdyh => dasdhs);
  
  -- Multi-driven assignments
  dasdhs <= kwnmxwpgb;
  kwnmxwpgb <= kwnmxwpgb;
  kwnmxwpgb <= "01U";
end kwownkvqw;

entity qroqjl is
  port (lafpyhiyt : buffer real; deqopzkg : linkage time);
end qroqjl;

library ieee;
use ieee.std_logic_1164.all;

architecture delrpigj of qroqjl is
  signal f : std_logic_vector(4 downto 2);
  signal ypekrw : time;
begin
  u : entity work.bkkfaic
    port map (x => ypekrw, bcr => f);
  
  -- Single-driven assignments
  lafpyhiyt <= lafpyhiyt;
  
  -- Multi-driven assignments
  f <= f;
end delrpigj;



-- Seed after: 15348989253258662953,9900838946149614537
