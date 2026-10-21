-- Seed: 13630553499843833345,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (hjhjeodacm : inout std_logic_vector(3 downto 0));
end z;

architecture irmdvfx of z is
  
begin
  -- Multi-driven assignments
  hjhjeodacm <= ('X', 'U', 'H', 'L');
end irmdvfx;

entity eddxvi is
  port (i : in integer);
end eddxvi;

library ieee;
use ieee.std_logic_1164.all;

architecture rouvxdbn of eddxvi is
  signal dxdqvpcbz : std_logic_vector(3 downto 0);
  signal sifbdxzisz : std_logic_vector(3 downto 0);
  signal zvbaazrlkv : std_logic_vector(3 downto 0);
begin
  csvra : entity work.z
    port map (hjhjeodacm => zvbaazrlkv);
  psvfd : entity work.z
    port map (hjhjeodacm => sifbdxzisz);
  abbyicujik : entity work.z
    port map (hjhjeodacm => zvbaazrlkv);
  vbifb : entity work.z
    port map (hjhjeodacm => dxdqvpcbz);
end rouvxdbn;



-- Seed after: 1179387994778292777,17219977430298397867
