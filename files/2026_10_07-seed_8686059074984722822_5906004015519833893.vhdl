-- Seed: 8686059074984722822,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity llhdximgp is
  port (nuqrdhddss : linkage std_logic; bbod : linkage std_logic);
end llhdximgp;

architecture otvte of llhdximgp is
  
begin
  
end otvte;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (gfzx : linkage std_logic);
end f;

architecture ynibu of f is
  
begin
  eenzk : entity work.llhdximgp
    port map (nuqrdhddss => gfzx, bbod => gfzx);
  pcuutdyi : entity work.llhdximgp
    port map (nuqrdhddss => gfzx, bbod => gfzx);
  o : entity work.llhdximgp
    port map (nuqrdhddss => gfzx, bbod => gfzx);
end ynibu;

library ieee;
use ieee.std_logic_1164.all;

entity owaiepexsd is
  port (gqroej : linkage integer; ty : in integer; uudbkwmbvu : buffer time; pcdalrna : in std_logic);
end owaiepexsd;

library ieee;
use ieee.std_logic_1164.all;

architecture zhyx of owaiepexsd is
  signal ggtoxmmui : std_logic;
  signal fgtailrn : std_logic;
  signal abfxudxxul : std_logic;
begin
  axmcepmap : entity work.llhdximgp
    port map (nuqrdhddss => pcdalrna, bbod => abfxudxxul);
  ypxqwguhh : entity work.f
    port map (gfzx => abfxudxxul);
  sdyll : entity work.llhdximgp
    port map (nuqrdhddss => fgtailrn, bbod => ggtoxmmui);
  lbfwjmofqo : entity work.llhdximgp
    port map (nuqrdhddss => ggtoxmmui, bbod => ggtoxmmui);
  
  -- Single-driven assignments
  uudbkwmbvu <= 2#10# ms;
  
  -- Multi-driven assignments
  ggtoxmmui <= abfxudxxul;
  abfxudxxul <= 'U';
  abfxudxxul <= pcdalrna;
  ggtoxmmui <= 'Z';
end zhyx;

entity fa is
  port (urzukka : in real);
end fa;

library ieee;
use ieee.std_logic_1164.all;

architecture vbdknciwe of fa is
  signal t : time;
  signal ooe : integer;
  signal wwlju : integer;
  signal omhxl : std_logic;
  signal kcphiuwmh : std_logic;
begin
  rqfqckreg : entity work.llhdximgp
    port map (nuqrdhddss => kcphiuwmh, bbod => omhxl);
  xvl : entity work.f
    port map (gfzx => kcphiuwmh);
  lvzjgo : entity work.owaiepexsd
    port map (gqroej => wwlju, ty => ooe, uudbkwmbvu => t, pcdalrna => omhxl);
  
  -- Single-driven assignments
  ooe <= wwlju;
  
  -- Multi-driven assignments
  omhxl <= 'Z';
end vbdknciwe;



-- Seed after: 4300916410492035978,5906004015519833893
