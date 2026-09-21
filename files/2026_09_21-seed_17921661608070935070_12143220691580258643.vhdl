-- Seed: 17921661608070935070,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity wpiexlvi is
  port (gubmzg : in std_logic_vector(3 downto 3));
end wpiexlvi;

architecture fyv of wpiexlvi is
  
begin
  
end fyv;

entity oatnh is
  port (mruasev : in bit_vector(1 to 3); kpzyb : out boolean; gopkb : out integer);
end oatnh;

library ieee;
use ieee.std_logic_1164.all;

architecture owqa of oatnh is
  signal lv : std_logic_vector(3 downto 3);
  signal plx : std_logic_vector(3 downto 3);
begin
  nrktphf : entity work.wpiexlvi
    port map (gubmzg => plx);
  jpptaixi : entity work.wpiexlvi
    port map (gubmzg => lv);
  jadnvotc : entity work.wpiexlvi
    port map (gubmzg => lv);
  
  -- Single-driven assignments
  gopkb <= gopkb;
  kpzyb <= FALSE;
  
  -- Multi-driven assignments
  plx <= (others => '1');
  plx <= (others => 'L');
end owqa;



-- Seed after: 10902569337695380781,12143220691580258643
