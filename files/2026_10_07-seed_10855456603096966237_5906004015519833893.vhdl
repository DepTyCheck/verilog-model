-- Seed: 10855456603096966237,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity ky is
  port (whb : linkage std_logic; b : inout std_logic_vector(4 downto 0); ks : buffer std_logic_vector(2 downto 4));
end ky;

architecture zwhontnyz of ky is
  
begin
  -- Multi-driven assignments
  ks <= ks;
end zwhontnyz;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (zmofckvst : inout std_logic_vector(4 to 4); lqyb : inout real; sjugxuz : linkage integer);
end e;

architecture lp of e is
  
begin
  -- Single-driven assignments
  lqyb <= lqyb;
end lp;

entity bs is
  port (djfk : out bit_vector(2 downto 4));
end bs;

library ieee;
use ieee.std_logic_1164.all;

architecture ztxihosyh of bs is
  signal jvy : std_logic_vector(2 downto 4);
  signal czhtcscir : std_logic_vector(2 downto 4);
  signal vlmrfxkoi : std_logic_vector(4 downto 0);
  signal zogsixt : std_logic;
  signal tgcdqvxpl : integer;
  signal qolrgubci : real;
  signal htndhqlvfv : std_logic_vector(4 to 4);
begin
  tcnbgavq : entity work.e
    port map (zmofckvst => htndhqlvfv, lqyb => qolrgubci, sjugxuz => tgcdqvxpl);
  gfupz : entity work.ky
    port map (whb => zogsixt, b => vlmrfxkoi, ks => czhtcscir);
  xhcx : entity work.ky
    port map (whb => zogsixt, b => vlmrfxkoi, ks => jvy);
  
  -- Single-driven assignments
  djfk <= djfk;
  
  -- Multi-driven assignments
  htndhqlvfv <= (others => '0');
  jvy <= czhtcscir;
  htndhqlvfv <= htndhqlvfv;
  jvy <= czhtcscir;
end ztxihosyh;



-- Seed after: 16094569293490287288,5906004015519833893
