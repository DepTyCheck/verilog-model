-- Seed: 15034196346253861528,511364357853360275

entity u is
  port (btgfyidx : buffer integer_vector(0 to 4); qhoropixx : in integer; vekkeg : in bit_vector(1 to 3); ft : buffer time);
end u;

architecture oky of u is
  
begin
  -- Single-driven assignments
  btgfyidx <= (16#9E9#, 443, 1403, 2#0_1_0_0_1#, 34);
  ft <= 16#E_C# ms;
end oky;

entity mcel is
  port (rxdpg : out integer);
end mcel;

architecture xjvbvwkx of mcel is
  signal uprkkaklo : time;
  signal dtocsnuek : bit_vector(1 to 3);
  signal y : integer_vector(0 to 4);
begin
  dtvb : entity work.u
    port map (btgfyidx => y, qhoropixx => rxdpg, vekkeg => dtocsnuek, ft => uprkkaklo);
  
  -- Single-driven assignments
  rxdpg <= 8#147#;
  dtocsnuek <= ('1', '1', '0');
end xjvbvwkx;

library ieee;
use ieee.std_logic_1164.all;

entity pplgiaa is
  port (ly : out std_logic; brwvk : in integer; mvp : linkage std_logic_vector(4 downto 2); iyhfoiaxlm : inout std_logic);
end pplgiaa;

architecture nmlbkrbgh of pplgiaa is
  signal offbrgxzil : integer;
begin
  p : entity work.mcel
    port map (rxdpg => offbrgxzil);
  
  -- Multi-driven assignments
  ly <= 'H';
  iyhfoiaxlm <= 'Z';
  iyhfoiaxlm <= 'X';
  iyhfoiaxlm <= iyhfoiaxlm;
end nmlbkrbgh;



-- Seed after: 14895449502372807787,511364357853360275
