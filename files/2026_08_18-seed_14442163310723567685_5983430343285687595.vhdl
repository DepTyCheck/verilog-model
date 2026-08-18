-- Seed: 14442163310723567685,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity kozxsbdti is
  port (evoqoer : linkage bit_vector(3 to 3); qdb : in std_logic_vector(2 to 1));
end kozxsbdti;

architecture jphjuczs of kozxsbdti is
  
begin
  
end jphjuczs;

library ieee;
use ieee.std_logic_1164.all;

entity acdcpoxaxz is
  port (xylihvfic : buffer std_logic_vector(0 downto 0); q : buffer real);
end acdcpoxaxz;

library ieee;
use ieee.std_logic_1164.all;

architecture ergkm of acdcpoxaxz is
  signal puqyi : std_logic_vector(2 to 1);
  signal mgu : bit_vector(3 to 3);
  signal vlm : std_logic_vector(2 to 1);
  signal mcb : bit_vector(3 to 3);
  signal hektv : std_logic_vector(2 to 1);
  signal gin : bit_vector(3 to 3);
begin
  pykcpovv : entity work.kozxsbdti
    port map (evoqoer => gin, qdb => hektv);
  alzsd : entity work.kozxsbdti
    port map (evoqoer => mcb, qdb => vlm);
  yeclxecb : entity work.kozxsbdti
    port map (evoqoer => mgu, qdb => puqyi);
  
  -- Single-driven assignments
  q <= 44.1_3_4_4_0;
  
  -- Multi-driven assignments
  xylihvfic <= xylihvfic;
  hektv <= (others => '0');
  puqyi <= puqyi;
  hektv <= hektv;
end ergkm;

library ieee;
use ieee.std_logic_1164.all;

entity njslugstn is
  port (hmot : inout string(5 to 3); otd : linkage std_logic_vector(2 downto 2); tgbojnmsxj : buffer time);
end njslugstn;

library ieee;
use ieee.std_logic_1164.all;

architecture vbd of njslugstn is
  signal grh : std_logic_vector(2 to 1);
  signal lb : bit_vector(3 to 3);
  signal cgigj : real;
  signal wterbudzz : std_logic_vector(0 downto 0);
begin
  zxpg : entity work.acdcpoxaxz
    port map (xylihvfic => wterbudzz, q => cgigj);
  n : entity work.kozxsbdti
    port map (evoqoer => lb, qdb => grh);
  
  -- Single-driven assignments
  hmot <= hmot;
  tgbojnmsxj <= 1 hr;
  
  -- Multi-driven assignments
  wterbudzz <= wterbudzz;
end vbd;



-- Seed after: 9194560023513561199,5983430343285687595
