-- Seed: 11946501960191267413,7808623373429384027

entity qi is
  port (ytpoc : linkage real; nfqdicsal : in boolean; xtopdriayx : inout bit_vector(2 to 2));
end qi;

architecture b of qi is
  
begin
  -- Single-driven assignments
  xtopdriayx <= (others => '0');
end b;

library ieee;
use ieee.std_logic_1164.all;

entity bwtwjuaxit is
  port (bifoqohay : linkage std_logic_vector(2 to 2));
end bwtwjuaxit;

architecture tssu of bwtwjuaxit is
  signal uoywrm : bit_vector(2 to 2);
  signal ww : real;
  signal jzuqmkej : bit_vector(2 to 2);
  signal lziwiui : boolean;
  signal wkbgukqj : real;
  signal uqbeyysbf : bit_vector(2 to 2);
  signal gazobkw : real;
  signal raubbvm : bit_vector(2 to 2);
  signal zpfzd : boolean;
  signal wrtjh : real;
begin
  sgzlc : entity work.qi
    port map (ytpoc => wrtjh, nfqdicsal => zpfzd, xtopdriayx => raubbvm);
  onhppvx : entity work.qi
    port map (ytpoc => gazobkw, nfqdicsal => zpfzd, xtopdriayx => uqbeyysbf);
  lcb : entity work.qi
    port map (ytpoc => wkbgukqj, nfqdicsal => lziwiui, xtopdriayx => jzuqmkej);
  iyudgb : entity work.qi
    port map (ytpoc => ww, nfqdicsal => zpfzd, xtopdriayx => uoywrm);
  
  -- Single-driven assignments
  lziwiui <= zpfzd;
end tssu;



-- Seed after: 14223525211897028091,7808623373429384027
