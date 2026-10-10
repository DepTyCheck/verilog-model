-- Seed: 12704549532148794233,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity qqchtejffv is
  port (cnqqvpvb : in time; vsnkfzbdag : in integer_vector(0 downto 0); atd : in std_logic_vector(1 downto 3));
end qqchtejffv;

architecture tksuqmt of qqchtejffv is
  
begin
  
end tksuqmt;

entity v is
  port (yupm : out boolean; dvusezevm : in integer; mslcablen : inout real);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture ry of v is
  signal zt : std_logic_vector(1 downto 3);
  signal abkeaps : integer_vector(0 downto 0);
  signal mqcigka : integer_vector(0 downto 0);
  signal puqfzis : time;
  signal spsgcf : std_logic_vector(1 downto 3);
  signal pctwcewllc : std_logic_vector(1 downto 3);
  signal drx : integer_vector(0 downto 0);
  signal qpgvn : time;
begin
  xhf : entity work.qqchtejffv
    port map (cnqqvpvb => qpgvn, vsnkfzbdag => drx, atd => pctwcewllc);
  cjl : entity work.qqchtejffv
    port map (cnqqvpvb => qpgvn, vsnkfzbdag => drx, atd => spsgcf);
  agi : entity work.qqchtejffv
    port map (cnqqvpvb => puqfzis, vsnkfzbdag => mqcigka, atd => spsgcf);
  orso : entity work.qqchtejffv
    port map (cnqqvpvb => qpgvn, vsnkfzbdag => abkeaps, atd => zt);
  
  -- Single-driven assignments
  qpgvn <= qpgvn;
  mqcigka <= drx;
  drx <= drx;
  yupm <= yupm;
  
  -- Multi-driven assignments
  spsgcf <= (others => '0');
end ry;



-- Seed after: 16581463169117459260,511364357853360275
