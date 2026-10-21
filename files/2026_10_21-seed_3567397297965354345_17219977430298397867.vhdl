-- Seed: 3567397297965354345,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity wjmd is
  port (tp : in std_logic_vector(0 downto 2); bkbipr : linkage std_logic_vector(3 to 4));
end wjmd;

architecture pl of wjmd is
  
begin
  
end pl;

entity ue is
  port (s : buffer real_vector(4 to 3); ol : inout integer);
end ue;

library ieee;
use ieee.std_logic_1164.all;

architecture cnmsskl of ue is
  signal hf : std_logic_vector(3 to 4);
  signal wezis : std_logic_vector(0 downto 2);
begin
  wiemgdmn : entity work.wjmd
    port map (tp => wezis, bkbipr => hf);
  
  -- Multi-driven assignments
  hf <= "U0";
end cnmsskl;



-- Seed after: 5899438331080000309,17219977430298397867
