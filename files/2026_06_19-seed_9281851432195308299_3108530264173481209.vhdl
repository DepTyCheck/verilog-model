-- Seed: 9281851432195308299,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity eh is
  port (xzwjg : buffer real_vector(0 to 4); pzcoh : buffer std_logic);
end eh;

architecture jftlph of eh is
  
begin
  -- Single-driven assignments
  xzwjg <= (2#0_1_0_1.01#, 2#1_0.0_1_1_0_1#, 8#2711.27066#, 101.1_2_2, 2_0_2.1_3);
  
  -- Multi-driven assignments
  pzcoh <= 'U';
end jftlph;

library ieee;
use ieee.std_logic_1164.all;

entity vebxf is
  port (yawl : out std_logic_vector(3 to 0));
end vebxf;

library ieee;
use ieee.std_logic_1164.all;

architecture ykiasmoju of vebxf is
  signal w : std_logic;
  signal crcvhnw : real_vector(0 to 4);
  signal endlqud : std_logic;
  signal bpckgjihmw : real_vector(0 to 4);
  signal kjdcuqac : std_logic;
  signal oon : real_vector(0 to 4);
begin
  bntiot : entity work.eh
    port map (xzwjg => oon, pzcoh => kjdcuqac);
  pox : entity work.eh
    port map (xzwjg => bpckgjihmw, pzcoh => endlqud);
  zu : entity work.eh
    port map (xzwjg => crcvhnw, pzcoh => w);
  
  -- Multi-driven assignments
  w <= '1';
end ykiasmoju;

entity mwhc is
  port (kkyghjdiat : buffer severity_level);
end mwhc;

library ieee;
use ieee.std_logic_1164.all;

architecture z of mwhc is
  signal n : std_logic;
  signal cipyylxm : real_vector(0 to 4);
begin
  u : entity work.eh
    port map (xzwjg => cipyylxm, pzcoh => n);
  
  -- Single-driven assignments
  kkyghjdiat <= ERROR;
end z;

library ieee;
use ieee.std_logic_1164.all;

entity hpgrs is
  port (ikiqgw : inout bit; vtsj : out boolean_vector(1 downto 3); ynylv : out std_logic_vector(2 to 2); akivlhq : buffer std_logic);
end hpgrs;

architecture exnvfy of hpgrs is
  
begin
  -- Single-driven assignments
  vtsj <= (others => TRUE);
  ikiqgw <= '1';
  
  -- Multi-driven assignments
  akivlhq <= 'L';
end exnvfy;



-- Seed after: 15837581007288273752,3108530264173481209
