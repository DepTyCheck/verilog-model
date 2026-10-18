-- Seed: 11477896626154029549,14108379317686958507

entity mvvpgaulb is
  port (sdlerw : inout integer);
end mvvpgaulb;

architecture bqkmx of mvvpgaulb is
  
begin
  -- Single-driven assignments
  sdlerw <= 322;
end bqkmx;

library ieee;
use ieee.std_logic_1164.all;

entity zse is
  port (jystyhhzpr : inout std_logic_vector(3 to 0));
end zse;

architecture g of zse is
  signal czgusao : integer;
  signal j : integer;
begin
  xydhfl : entity work.mvvpgaulb
    port map (sdlerw => j);
  hrbaquoegk : entity work.mvvpgaulb
    port map (sdlerw => czgusao);
  
  -- Multi-driven assignments
  jystyhhzpr <= "";
end g;

entity vsgjo is
  port (sehjrhudoo : inout integer_vector(1 downto 1); fkhqa : in integer_vector(0 downto 4));
end vsgjo;

architecture ai of vsgjo is
  
begin
  -- Single-driven assignments
  sehjrhudoo <= (others => 8#6#);
end ai;

library ieee;
use ieee.std_logic_1164.all;

entity xxscj is
  port (gewiqkfk : inout integer; lyjrney : inout time; fldjaswaf : inout std_logic_vector(2 downto 1));
end xxscj;

library ieee;
use ieee.std_logic_1164.all;

architecture ul of xxscj is
  signal fwoeyh : std_logic_vector(3 to 0);
begin
  pckyiil : entity work.zse
    port map (jystyhhzpr => fwoeyh);
  foqrogoiq : entity work.mvvpgaulb
    port map (sdlerw => gewiqkfk);
  mjzqr : entity work.zse
    port map (jystyhhzpr => fwoeyh);
  
  -- Multi-driven assignments
  fldjaswaf <= "01";
  fldjaswaf <= "HW";
end ul;



-- Seed after: 3116030864140325123,14108379317686958507
