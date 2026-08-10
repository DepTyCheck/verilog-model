-- Seed: 16021740717228525541,2338584220606314193

library ieee;
use ieee.std_logic_1164.all;

entity nr is
  port (zhgama : inout std_logic);
end nr;

architecture tid of nr is
  
begin
  
end tid;

library ieee;
use ieee.std_logic_1164.all;

entity ahrt is
  port (uh : linkage time; hwaeaeh : inout integer; mi : in std_logic_vector(0 downto 2));
end ahrt;

library ieee;
use ieee.std_logic_1164.all;

architecture zmuckgyumq of ahrt is
  signal berqmdpmtw : std_logic;
begin
  xxxoxcfq : entity work.nr
    port map (zhgama => berqmdpmtw);
  bse : entity work.nr
    port map (zhgama => berqmdpmtw);
  kq : entity work.nr
    port map (zhgama => berqmdpmtw);
  
  -- Multi-driven assignments
  berqmdpmtw <= '0';
end zmuckgyumq;

entity vzdgr is
  port (bzlhjauyg : buffer time);
end vzdgr;

library ieee;
use ieee.std_logic_1164.all;

architecture liexayjzo of vzdgr is
  signal ibcfqcmb : std_logic;
  signal y : std_logic_vector(0 downto 2);
  signal yhaswsq : integer;
  signal omzv : time;
begin
  alrm : entity work.ahrt
    port map (uh => omzv, hwaeaeh => yhaswsq, mi => y);
  prdtq : entity work.nr
    port map (zhgama => ibcfqcmb);
  
  -- Single-driven assignments
  bzlhjauyg <= 1_1.40110 ps;
  
  -- Multi-driven assignments
  ibcfqcmb <= ibcfqcmb;
end liexayjzo;



-- Seed after: 11555314488829041096,2338584220606314193
