-- Seed: 11148265895133107393,499459191852795575

entity sia is
  port (pwel : inout bit_vector(2 to 4); rknekzqqa : buffer boolean_vector(3 to 4));
end sia;

architecture aej of sia is
  
begin
  -- Single-driven assignments
  rknekzqqa <= (TRUE, FALSE);
  pwel <= pwel;
end aej;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (jmpqbnzr : buffer std_logic_vector(2 downto 0));
end h;

architecture fqggnzj of h is
  signal kwhwezua : boolean_vector(3 to 4);
  signal wi : bit_vector(2 to 4);
  signal xfcvhdaa : boolean_vector(3 to 4);
  signal afxmi : bit_vector(2 to 4);
  signal se : boolean_vector(3 to 4);
  signal oou : bit_vector(2 to 4);
  signal nvxrgth : boolean_vector(3 to 4);
  signal ewiphvbz : bit_vector(2 to 4);
begin
  nzrzj : entity work.sia
    port map (pwel => ewiphvbz, rknekzqqa => nvxrgth);
  yujpa : entity work.sia
    port map (pwel => oou, rknekzqqa => se);
  fyhaqg : entity work.sia
    port map (pwel => afxmi, rknekzqqa => xfcvhdaa);
  skbynmomge : entity work.sia
    port map (pwel => wi, rknekzqqa => kwhwezua);
end fqggnzj;



-- Seed after: 116154180804360424,499459191852795575
