-- Seed: 16864785090042092483,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity whm is
  port (qdwr : out std_logic_vector(4 downto 1); nwkklami : in time; yznrky : linkage real);
end whm;

architecture ww of whm is
  
begin
  -- Multi-driven assignments
  qdwr <= qdwr;
end ww;

library ieee;
use ieee.std_logic_1164.all;

entity trviyucu is
  port (fnttmdlvy : linkage time; qnuzwchg : linkage real; qrkc : inout std_logic);
end trviyucu;

library ieee;
use ieee.std_logic_1164.all;

architecture anyuidbao of trviyucu is
  signal qfwuzir : std_logic_vector(4 downto 1);
  signal dsaltqgpst : real;
  signal kmfyms : time;
  signal srgvawrdy : real;
  signal ttnasfh : time;
  signal tenvrxiv : std_logic_vector(4 downto 1);
begin
  d : entity work.whm
    port map (qdwr => tenvrxiv, nwkklami => ttnasfh, yznrky => srgvawrdy);
  nnhs : entity work.whm
    port map (qdwr => tenvrxiv, nwkklami => kmfyms, yznrky => dsaltqgpst);
  mxt : entity work.whm
    port map (qdwr => qfwuzir, nwkklami => kmfyms, yznrky => qnuzwchg);
  
  -- Single-driven assignments
  ttnasfh <= ttnasfh;
  kmfyms <= 24400 us;
  
  -- Multi-driven assignments
  tenvrxiv <= ('0', 'Z', 'H', '-');
end anyuidbao;



-- Seed after: 9117539849670424065,2230106469645304029
