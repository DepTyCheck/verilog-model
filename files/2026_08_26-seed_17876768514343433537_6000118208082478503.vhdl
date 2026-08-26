-- Seed: 17876768514343433537,6000118208082478503

entity dpo is
  port (rmxdo : inout time);
end dpo;

architecture i of dpo is
  
begin
  -- Single-driven assignments
  rmxdo <= 16#5.0_E# fs;
end i;

entity hmhjv is
  port (ycic : buffer integer_vector(0 downto 2));
end hmhjv;

architecture gxgmcw of hmhjv is
  
begin
  
end gxgmcw;

library ieee;
use ieee.std_logic_1164.all;

entity fe is
  port (ddaug : inout bit; mw : buffer time; cjn : inout std_logic_vector(0 downto 3));
end fe;

architecture st of fe is
  signal zngnlew : integer_vector(0 downto 2);
  signal gssqnfgpx : integer_vector(0 downto 2);
begin
  dfzzyhfgt : entity work.dpo
    port map (rmxdo => mw);
  kdpoecuoqh : entity work.hmhjv
    port map (ycic => gssqnfgpx);
  vvqhjwypt : entity work.hmhjv
    port map (ycic => zngnlew);
  
  -- Single-driven assignments
  ddaug <= '1';
end st;



-- Seed after: 18266901073554045747,6000118208082478503
