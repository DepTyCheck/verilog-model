-- Seed: 758541321083243658,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity pvlrcwt is
  port (zwxpobmrjv : out real; evlvlzosi : in std_logic_vector(1 to 1); mpizrpjg : inout boolean_vector(1 to 3));
end pvlrcwt;

architecture wunzoig of pvlrcwt is
  
begin
  -- Single-driven assignments
  mpizrpjg <= (FALSE, FALSE, FALSE);
  zwxpobmrjv <= 16#CF8.E9#;
end wunzoig;

entity ruq is
  port (ogqtgvgly : out real; lvg : buffer integer; uqidwwciv : buffer time; t : out integer);
end ruq;

architecture chmrxzmlqq of ruq is
  
begin
  -- Single-driven assignments
  t <= 2#01#;
  uqidwwciv <= 0_1_0_1 ms;
end chmrxzmlqq;

entity beqhdvauh is
  port (rkth : linkage time; ln : buffer real_vector(1 to 0); sjggbr : linkage integer);
end beqhdvauh;

library ieee;
use ieee.std_logic_1164.all;

architecture wyyvh of beqhdvauh is
  signal cdgddszw : integer;
  signal ggmqi : time;
  signal bmsfe : integer;
  signal lffmccjeyy : real;
  signal plsi : boolean_vector(1 to 3);
  signal pnqhht : std_logic_vector(1 to 1);
  signal qnhxmskzw : real;
begin
  qtkxrgtt : entity work.pvlrcwt
    port map (zwxpobmrjv => qnhxmskzw, evlvlzosi => pnqhht, mpizrpjg => plsi);
  wurkd : entity work.ruq
    port map (ogqtgvgly => lffmccjeyy, lvg => bmsfe, uqidwwciv => ggmqi, t => cdgddszw);
  
  -- Single-driven assignments
  ln <= (others => 0.0);
  
  -- Multi-driven assignments
  pnqhht <= "X";
  pnqhht <= pnqhht;
  pnqhht <= pnqhht;
  pnqhht <= pnqhht;
end wyyvh;



-- Seed after: 17948613116760476775,6176025545472508329
