-- Seed: 6410553434572664331,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity fmg is
  port (axu : buffer string(5 to 5); d : inout std_logic; ox : linkage std_logic_vector(3 to 0); bo : out string(2 downto 5));
end fmg;

architecture wcbr of fmg is
  
begin
  -- Single-driven assignments
  bo <= (others => ' ');
  axu <= "s";
  
  -- Multi-driven assignments
  d <= 'X';
  d <= d;
  d <= d;
end wcbr;

library ieee;
use ieee.std_logic_1164.all;

entity bdgs is
  port (ynhtumaq : buffer std_logic; udt : linkage integer; q : buffer bit_vector(3 to 1));
end bdgs;

library ieee;
use ieee.std_logic_1164.all;

architecture ncc of bdgs is
  signal zmjetbafk : string(2 downto 5);
  signal qjagpprf : std_logic_vector(3 to 0);
  signal stufki : string(5 to 5);
begin
  mbdxu : entity work.fmg
    port map (axu => stufki, d => ynhtumaq, ox => qjagpprf, bo => zmjetbafk);
  
  -- Multi-driven assignments
  qjagpprf <= qjagpprf;
  ynhtumaq <= 'W';
  ynhtumaq <= ynhtumaq;
  ynhtumaq <= ynhtumaq;
end ncc;



-- Seed after: 8476795080535312463,662889661651915549
