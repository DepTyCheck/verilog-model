-- Seed: 11205723903248140976,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity aq is
  port (t : inout real; zj : buffer std_logic; ynz : out std_logic);
end aq;

architecture blyx of aq is
  
begin
  -- Multi-driven assignments
  ynz <= 'Z';
  ynz <= '0';
  ynz <= 'Z';
  ynz <= ynz;
end blyx;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (lbxqvt : in std_logic_vector(4 downto 4));
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture wzrstgzwq of w is
  signal jpdpkj : real;
  signal aecxli : std_logic;
  signal izhtas : real;
  signal jxuetfllg : std_logic;
  signal yh : real;
begin
  ilp : entity work.aq
    port map (t => yh, zj => jxuetfllg, ynz => jxuetfllg);
  gltrqj : entity work.aq
    port map (t => izhtas, zj => jxuetfllg, ynz => aecxli);
  p : entity work.aq
    port map (t => jpdpkj, zj => jxuetfllg, ynz => aecxli);
  
  -- Multi-driven assignments
  jxuetfllg <= '-';
end wzrstgzwq;



-- Seed after: 2539030743876010479,7304262412290825129
