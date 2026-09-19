-- Seed: 4691036531037490317,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity ivl is
  port (ykmlc : inout std_logic_vector(1 downto 0); krfjt : out bit);
end ivl;

architecture ymodpraor of ivl is
  
begin
  -- Multi-driven assignments
  ykmlc <= "1L";
  ykmlc <= "11";
  ykmlc <= "H1";
  ykmlc <= ('H', 'X');
end ymodpraor;

entity yi is
  port (qme : in time; pxjus : linkage real);
end yi;

library ieee;
use ieee.std_logic_1164.all;

architecture bxdbt of yi is
  signal lpzynavl : bit;
  signal xwfs : std_logic_vector(1 downto 0);
  signal ckekq : bit;
  signal poswqyq : std_logic_vector(1 downto 0);
begin
  sqytbe : entity work.ivl
    port map (ykmlc => poswqyq, krfjt => ckekq);
  yhmy : entity work.ivl
    port map (ykmlc => xwfs, krfjt => lpzynavl);
end bxdbt;

entity fmsee is
  port (ousiglntk : buffer time_vector(2 downto 3));
end fmsee;

library ieee;
use ieee.std_logic_1164.all;

architecture xfoezajhl of fmsee is
  signal nyfjrg : real;
  signal ec : time;
  signal x : bit;
  signal dwn : std_logic_vector(1 downto 0);
begin
  tasxnvl : entity work.ivl
    port map (ykmlc => dwn, krfjt => x);
  zqci : entity work.yi
    port map (qme => ec, pxjus => nyfjrg);
  
  -- Single-driven assignments
  ec <= 16#0_3_B_8.2# ns;
  ousiglntk <= (others => 0 ns);
  
  -- Multi-driven assignments
  dwn <= dwn;
  dwn <= "L-";
end xfoezajhl;



-- Seed after: 6656828527556259581,14141408946471626091
