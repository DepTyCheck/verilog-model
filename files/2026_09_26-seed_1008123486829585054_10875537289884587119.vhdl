-- Seed: 1008123486829585054,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity xeoyosja is
  port (iebqqti : inout std_logic_vector(4 to 1));
end xeoyosja;

architecture jlemyrjixk of xeoyosja is
  
begin
  
end jlemyrjixk;

entity qxr is
  port (q : inout time; zxs : out severity_level; bdrenf : linkage string(5 downto 4));
end qxr;

library ieee;
use ieee.std_logic_1164.all;

architecture npmjcxux of qxr is
  signal n : std_logic_vector(4 to 1);
  signal olgtkrkybd : std_logic_vector(4 to 1);
begin
  qtjs : entity work.xeoyosja
    port map (iebqqti => olgtkrkybd);
  mdpqkej : entity work.xeoyosja
    port map (iebqqti => n);
  
  -- Single-driven assignments
  q <= 2#01# ms;
  zxs <= zxs;
  
  -- Multi-driven assignments
  olgtkrkybd <= (others => '0');
  n <= "";
  olgtkrkybd <= "";
end npmjcxux;



-- Seed after: 12309586795015212004,10875537289884587119
