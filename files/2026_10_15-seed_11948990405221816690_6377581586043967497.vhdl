-- Seed: 11948990405221816690,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (qywdbouizj : inout std_logic_vector(4 downto 4));
end r;

architecture uhwcfzzst of r is
  
begin
  -- Multi-driven assignments
  qywdbouizj <= (others => '1');
  qywdbouizj <= qywdbouizj;
  qywdbouizj <= qywdbouizj;
end uhwcfzzst;

entity msunqgywg is
  port (amvcly : inout integer);
end msunqgywg;

library ieee;
use ieee.std_logic_1164.all;

architecture kibdc of msunqgywg is
  signal pfr : std_logic_vector(4 downto 4);
begin
  bdveoqg : entity work.r
    port map (qywdbouizj => pfr);
  vkkyolpvwe : entity work.r
    port map (qywdbouizj => pfr);
  gzqhlzta : entity work.r
    port map (qywdbouizj => pfr);
  
  -- Single-driven assignments
  amvcly <= 0_1_0;
  
  -- Multi-driven assignments
  pfr <= "1";
  pfr <= pfr;
  pfr <= pfr;
  pfr <= (others => 'Z');
end kibdc;

entity iztolfun is
  port (gm : out time; s : buffer real; du : out integer);
end iztolfun;

library ieee;
use ieee.std_logic_1164.all;

architecture spv of iztolfun is
  signal okxd : std_logic_vector(4 downto 4);
  signal xugk : std_logic_vector(4 downto 4);
begin
  szhjwsbdpd : entity work.r
    port map (qywdbouizj => xugk);
  nggkbppo : entity work.r
    port map (qywdbouizj => okxd);
  
  -- Single-driven assignments
  du <= 8#4#;
  s <= 1_0.3_1_2;
  gm <= gm;
end spv;



-- Seed after: 9689253341504603283,6377581586043967497
