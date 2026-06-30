-- Seed: 7735414929393510499,14629254427735353553

entity zocbsrr is
  port (yahyswz : inout bit_vector(2 downto 0));
end zocbsrr;

architecture txgx of zocbsrr is
  
begin
  -- Single-driven assignments
  yahyswz <= ('0', '0', '1');
end txgx;

library ieee;
use ieee.std_logic_1164.all;

entity efezz is
  port (jggmyyw : inout std_logic_vector(0 downto 3); a : linkage std_logic; akyz : out time);
end efezz;

architecture enphql of efezz is
  
begin
  -- Single-driven assignments
  akyz <= 0 hr;
  
  -- Multi-driven assignments
  jggmyyw <= "";
end enphql;

entity ibq is
  port (xbfck : inout real; anstsjtcx : linkage time);
end ibq;

library ieee;
use ieee.std_logic_1164.all;

architecture l of ibq is
  signal dxrgz : time;
  signal qcfyvv : std_logic;
  signal zdwiii : std_logic_vector(0 downto 3);
begin
  f : entity work.efezz
    port map (jggmyyw => zdwiii, a => qcfyvv, akyz => dxrgz);
  
  -- Single-driven assignments
  xbfck <= 2#0110.1#;
  
  -- Multi-driven assignments
  zdwiii <= (others => '0');
  qcfyvv <= 'H';
  zdwiii <= "";
end l;

entity oawgalkpj is
  port (zc : linkage time);
end oawgalkpj;

architecture vei of oawgalkpj is
  signal xr : bit_vector(2 downto 0);
  signal zosfyo : bit_vector(2 downto 0);
  signal wguvxefux : bit_vector(2 downto 0);
begin
  p : entity work.zocbsrr
    port map (yahyswz => wguvxefux);
  bhg : entity work.zocbsrr
    port map (yahyswz => zosfyo);
  xjouok : entity work.zocbsrr
    port map (yahyswz => xr);
end vei;



-- Seed after: 2461019981850143132,14629254427735353553
