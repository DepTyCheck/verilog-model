-- Seed: 12025109448823369874,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity onwhzh is
  port (ntzdcnbsw : inout std_logic);
end onwhzh;

architecture qajxiyhaze of onwhzh is
  
begin
  -- Multi-driven assignments
  ntzdcnbsw <= '0';
end qajxiyhaze;

entity prfeqy is
  port (uwbkptx : out real; irwzdfr : out real; fzz : out bit; bdljlv : out time_vector(4 to 1));
end prfeqy;

library ieee;
use ieee.std_logic_1164.all;

architecture jzqbcidyjm of prfeqy is
  signal vjrvgt : std_logic;
  signal aur : std_logic;
begin
  nej : entity work.onwhzh
    port map (ntzdcnbsw => aur);
  xrphuckmeu : entity work.onwhzh
    port map (ntzdcnbsw => vjrvgt);
  b : entity work.onwhzh
    port map (ntzdcnbsw => vjrvgt);
  zrhhe : entity work.onwhzh
    port map (ntzdcnbsw => vjrvgt);
  
  -- Single-driven assignments
  bdljlv <= (others => 0 ns);
  fzz <= fzz;
  
  -- Multi-driven assignments
  aur <= 'X';
end jzqbcidyjm;



-- Seed after: 3559222412736954593,12260394286515585877
