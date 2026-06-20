-- Seed: 17309709358984473635,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity czms is
  port (m : inout std_logic; rvbfomo : buffer std_logic; ecwtfrirzl : buffer real);
end czms;

architecture abjjlaum of czms is
  
begin
  -- Single-driven assignments
  ecwtfrirzl <= 143.3_2;
  
  -- Multi-driven assignments
  rvbfomo <= '-';
  rvbfomo <= 'U';
  m <= 'W';
end abjjlaum;

library ieee;
use ieee.std_logic_1164.all;

entity nflijtrjie is
  port (twzdqnkqs : in std_logic_vector(4 downto 2); rupdx : buffer std_logic; ocoohpw : in std_logic_vector(4 to 3));
end nflijtrjie;

library ieee;
use ieee.std_logic_1164.all;

architecture ksnxgcfwcj of nflijtrjie is
  signal twdmtje : real;
  signal sghiw : std_logic;
  signal lbnpdt : real;
  signal xlcjzxnk : std_logic;
  signal jvhhlrsaws : std_logic;
  signal kgplvzygi : real;
  signal gfrd : std_logic;
begin
  ib : entity work.czms
    port map (m => rupdx, rvbfomo => gfrd, ecwtfrirzl => kgplvzygi);
  necxjviugn : entity work.czms
    port map (m => jvhhlrsaws, rvbfomo => xlcjzxnk, ecwtfrirzl => lbnpdt);
  zrtckiff : entity work.czms
    port map (m => sghiw, rvbfomo => jvhhlrsaws, ecwtfrirzl => twdmtje);
  
  -- Multi-driven assignments
  rupdx <= 'L';
  rupdx <= 'H';
  rupdx <= 'W';
  rupdx <= 'X';
end ksnxgcfwcj;



-- Seed after: 1203154490368952177,3924983747739634027
