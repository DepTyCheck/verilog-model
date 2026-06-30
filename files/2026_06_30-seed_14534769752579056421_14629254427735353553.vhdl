-- Seed: 14534769752579056421,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity tniuj is
  port (dpsdlswri : in std_logic_vector(2 downto 3); xal : in real; arbcerbvi : out character; pmogzmi : inout std_logic_vector(4 downto 0));
end tniuj;

architecture xpmuthezt of tniuj is
  
begin
  -- Single-driven assignments
  arbcerbvi <= 'd';
  
  -- Multi-driven assignments
  pmogzmi <= ('X', '1', 'X', 'X', '0');
  pmogzmi <= ('Z', 'U', '0', 'L', 'L');
  pmogzmi <= "WWLZW";
end xpmuthezt;

library ieee;
use ieee.std_logic_1164.all;

entity fp is
  port (ozvx : inout std_logic_vector(2 downto 4); hxwcqkjdcx : inout real);
end fp;

library ieee;
use ieee.std_logic_1164.all;

architecture isp of fp is
  signal hfcwg : std_logic_vector(4 downto 0);
  signal xcypt : character;
  signal jytwl : real;
  signal b : std_logic_vector(4 downto 0);
  signal yuw : character;
  signal evcrsclemz : std_logic_vector(2 downto 3);
begin
  niqfrk : entity work.tniuj
    port map (dpsdlswri => evcrsclemz, xal => hxwcqkjdcx, arbcerbvi => yuw, pmogzmi => b);
  peidqgjssm : entity work.tniuj
    port map (dpsdlswri => ozvx, xal => jytwl, arbcerbvi => xcypt, pmogzmi => hfcwg);
  
  -- Single-driven assignments
  hxwcqkjdcx <= 0_3_3.1;
  jytwl <= 3_3.0_1;
  
  -- Multi-driven assignments
  b <= ('L', 'X', '0', '1', '0');
  ozvx <= "";
  ozvx <= (others => '0');
  hfcwg <= ('L', 'Z', 'X', 'Z', 'W');
end isp;



-- Seed after: 5906813383016675668,14629254427735353553
