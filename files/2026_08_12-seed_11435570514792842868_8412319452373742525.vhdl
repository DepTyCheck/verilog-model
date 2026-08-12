-- Seed: 11435570514792842868,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity ocertcka is
  port (fin : in std_logic_vector(4 to 3); rxkv : inout string(1 downto 2));
end ocertcka;

architecture l of ocertcka is
  
begin
  -- Single-driven assignments
  rxkv <= "";
end l;

entity wfqqec is
  port (ufoxou : inout time; nwv : out character);
end wfqqec;

library ieee;
use ieee.std_logic_1164.all;

architecture tsmx of wfqqec is
  signal lrvxqea : string(1 downto 2);
  signal nzkowljwoy : std_logic_vector(4 to 3);
  signal wkmu : string(1 downto 2);
  signal esz : string(1 downto 2);
  signal fvrvuidpu : std_logic_vector(4 to 3);
begin
  tq : entity work.ocertcka
    port map (fin => fvrvuidpu, rxkv => esz);
  x : entity work.ocertcka
    port map (fin => fvrvuidpu, rxkv => wkmu);
  zclz : entity work.ocertcka
    port map (fin => nzkowljwoy, rxkv => lrvxqea);
  
  -- Single-driven assignments
  nwv <= nwv;
  ufoxou <= ufoxou;
  
  -- Multi-driven assignments
  fvrvuidpu <= (others => '0');
end tsmx;

library ieee;
use ieee.std_logic_1164.all;

entity uhjamtrqk is
  port (vhw : out std_logic_vector(4 to 3));
end uhjamtrqk;

architecture x of uhjamtrqk is
  
begin
  -- Multi-driven assignments
  vhw <= "";
  vhw <= (others => '0');
  vhw <= vhw;
  vhw <= (others => '0');
end x;



-- Seed after: 1818346820406136419,8412319452373742525
