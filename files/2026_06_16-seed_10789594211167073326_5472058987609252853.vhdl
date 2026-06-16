-- Seed: 10789594211167073326,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity owuixxn is
  port (uwfllbi : buffer time_vector(2 to 4); h : in std_logic);
end owuixxn;

architecture xb of owuixxn is
  
begin
  -- Single-driven assignments
  uwfllbi <= (3240 ns, 34240 us, 2#0# ps);
end xb;

library ieee;
use ieee.std_logic_1164.all;

entity yiccnlimag is
  port (osej : inout std_logic);
end yiccnlimag;

library ieee;
use ieee.std_logic_1164.all;

architecture ogrxpucr of yiccnlimag is
  signal ausa : std_logic;
  signal ejpzhlp : time_vector(2 to 4);
begin
  ea : entity work.owuixxn
    port map (uwfllbi => ejpzhlp, h => ausa);
  
  -- Multi-driven assignments
  ausa <= 'X';
  osej <= '1';
  osej <= 'U';
  osej <= 'L';
end ogrxpucr;

library ieee;
use ieee.std_logic_1164.all;

entity ejqwqj is
  port (kjauowj : linkage std_logic; yrb : in real; vnhidwdmq : inout integer);
end ejqwqj;

library ieee;
use ieee.std_logic_1164.all;

architecture axntw of ejqwqj is
  signal f : std_logic;
  signal hreuw : time_vector(2 to 4);
  signal dvw : time_vector(2 to 4);
  signal owh : std_logic;
begin
  bqh : entity work.yiccnlimag
    port map (osej => owh);
  kvjwoie : entity work.owuixxn
    port map (uwfllbi => dvw, h => owh);
  lw : entity work.yiccnlimag
    port map (osej => owh);
  ua : entity work.owuixxn
    port map (uwfllbi => hreuw, h => f);
  
  -- Single-driven assignments
  vnhidwdmq <= 1_3_0_3;
  
  -- Multi-driven assignments
  owh <= '0';
  owh <= 'Z';
  f <= '-';
end axntw;



-- Seed after: 11331023304870600020,5472058987609252853
