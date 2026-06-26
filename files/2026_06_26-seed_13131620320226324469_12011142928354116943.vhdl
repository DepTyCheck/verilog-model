-- Seed: 13131620320226324469,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity uutm is
  port (qdhpfu : out boolean_vector(2 downto 3); hfdaeh : linkage std_logic_vector(4 to 1));
end uutm;

architecture qbmjwktyo of uutm is
  
begin
  
end qbmjwktyo;

entity ebawbzd is
  port (bkox : linkage integer);
end ebawbzd;

library ieee;
use ieee.std_logic_1164.all;

architecture eql of ebawbzd is
  signal yq : boolean_vector(2 downto 3);
  signal arwrivmztz : std_logic_vector(4 to 1);
  signal bksdih : boolean_vector(2 downto 3);
begin
  s : entity work.uutm
    port map (qdhpfu => bksdih, hfdaeh => arwrivmztz);
  yeingiwn : entity work.uutm
    port map (qdhpfu => yq, hfdaeh => arwrivmztz);
  
  -- Multi-driven assignments
  arwrivmztz <= "";
  arwrivmztz <= (others => '0');
  arwrivmztz <= (others => '0');
  arwrivmztz <= "";
end eql;



-- Seed after: 10756120332885322615,12011142928354116943
