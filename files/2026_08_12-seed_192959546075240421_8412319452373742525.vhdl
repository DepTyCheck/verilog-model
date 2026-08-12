-- Seed: 192959546075240421,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity lzrg is
  port (ghse : buffer std_logic_vector(4 to 3));
end lzrg;

architecture jmrkq of lzrg is
  
begin
  -- Multi-driven assignments
  ghse <= (others => '0');
  ghse <= ghse;
end jmrkq;

entity rs is
  port (y : inout string(3 to 2));
end rs;

library ieee;
use ieee.std_logic_1164.all;

architecture zpjignash of rs is
  signal ihcdw : std_logic_vector(4 to 3);
  signal c : std_logic_vector(4 to 3);
  signal ocxmak : std_logic_vector(4 to 3);
begin
  jmtyokt : entity work.lzrg
    port map (ghse => ocxmak);
  vk : entity work.lzrg
    port map (ghse => c);
  i : entity work.lzrg
    port map (ghse => ihcdw);
  
  -- Single-driven assignments
  y <= "";
  
  -- Multi-driven assignments
  ocxmak <= ocxmak;
  ocxmak <= "";
end zpjignash;



-- Seed after: 13887944111694032215,8412319452373742525
