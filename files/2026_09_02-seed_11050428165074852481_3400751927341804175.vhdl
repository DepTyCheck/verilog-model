-- Seed: 11050428165074852481,3400751927341804175

entity hgkpaf is
  port (odqjp : in integer; xqbrhwe : buffer integer; erp : buffer severity_level; napzxfzru : linkage real_vector(0 to 3));
end hgkpaf;

architecture urw of hgkpaf is
  
begin
  
end urw;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (vkt : inout std_logic; edlt : linkage severity_level);
end v;

architecture x of v is
  signal a : real_vector(0 to 3);
  signal gppduk : severity_level;
  signal wdewfsyhvy : real_vector(0 to 3);
  signal c : severity_level;
  signal w : integer;
  signal cgs : integer;
begin
  bivo : entity work.hgkpaf
    port map (odqjp => cgs, xqbrhwe => w, erp => c, napzxfzru => wdewfsyhvy);
  pwg : entity work.hgkpaf
    port map (odqjp => cgs, xqbrhwe => cgs, erp => gppduk, napzxfzru => a);
  
  -- Multi-driven assignments
  vkt <= vkt;
  vkt <= 'U';
end x;



-- Seed after: 8966945179256846899,3400751927341804175
