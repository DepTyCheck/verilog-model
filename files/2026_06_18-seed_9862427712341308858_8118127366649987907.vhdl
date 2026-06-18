-- Seed: 9862427712341308858,8118127366649987907

entity ispphsvjjy is
  port (heckb : linkage time_vector(2 downto 0); rqkcou : out severity_level);
end ispphsvjjy;

architecture cbptqsx of ispphsvjjy is
  
begin
  
end cbptqsx;

library ieee;
use ieee.std_logic_1164.all;

entity tpcb is
  port (yngw : in std_logic_vector(3 downto 4); i : buffer severity_level);
end tpcb;

architecture sjddscex of tpcb is
  signal pxf : time_vector(2 downto 0);
  signal nhsuyxdbvv : severity_level;
  signal yeenecyabl : time_vector(2 downto 0);
begin
  ektzryhx : entity work.ispphsvjjy
    port map (heckb => yeenecyabl, rqkcou => nhsuyxdbvv);
  oralhcmou : entity work.ispphsvjjy
    port map (heckb => pxf, rqkcou => i);
end sjddscex;

entity kov is
  port (nub : inout time);
end kov;

library ieee;
use ieee.std_logic_1164.all;

architecture rc of kov is
  signal kbccbkrar : severity_level;
  signal qpishsghy : std_logic_vector(3 downto 4);
  signal kmhiefjglr : severity_level;
  signal d : time_vector(2 downto 0);
begin
  mqr : entity work.ispphsvjjy
    port map (heckb => d, rqkcou => kmhiefjglr);
  jfgiw : entity work.tpcb
    port map (yngw => qpishsghy, i => kbccbkrar);
  
  -- Single-driven assignments
  nub <= 16#D59.C60# ns;
  
  -- Multi-driven assignments
  qpishsghy <= "";
  qpishsghy <= (others => '0');
  qpishsghy <= (others => '0');
  qpishsghy <= (others => '0');
end rc;

entity xtxlt is
  port (ngbohpdz : linkage real);
end xtxlt;

architecture xgncjinst of xtxlt is
  signal sgxq : severity_level;
  signal ztf : time_vector(2 downto 0);
  signal hsyzkdy : severity_level;
  signal ihcxex : time_vector(2 downto 0);
  signal qkzxandx : time;
  signal wdgmryuj : time;
begin
  cysru : entity work.kov
    port map (nub => wdgmryuj);
  n : entity work.kov
    port map (nub => qkzxandx);
  r : entity work.ispphsvjjy
    port map (heckb => ihcxex, rqkcou => hsyzkdy);
  qugy : entity work.ispphsvjjy
    port map (heckb => ztf, rqkcou => sgxq);
end xgncjinst;



-- Seed after: 4935986057984003754,8118127366649987907
