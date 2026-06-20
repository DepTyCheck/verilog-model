-- Seed: 15012926457848535730,17924494779688682807

entity a is
  port (pstupwjhmc : inout bit; jv : in boolean_vector(0 to 4); rvvz : in real);
end a;

architecture olryv of a is
  
begin
  -- Single-driven assignments
  pstupwjhmc <= '1';
end olryv;

entity dmaavi is
  port (jizmackv : linkage real; zv : linkage time; zyygaoo : inout integer; eblgw : buffer real);
end dmaavi;

architecture rnm of dmaavi is
  signal dfnonivz : bit;
  signal apw : real;
  signal dt : boolean_vector(0 to 4);
  signal hfa : bit;
begin
  pnkikhsral : entity work.a
    port map (pstupwjhmc => hfa, jv => dt, rvvz => apw);
  snkrfmjk : entity work.a
    port map (pstupwjhmc => dfnonivz, jv => dt, rvvz => apw);
  
  -- Single-driven assignments
  zyygaoo <= 16#5#;
  eblgw <= 2#1001.0#;
end rnm;

entity juwabias is
  port (hx : buffer integer; wcr : linkage bit_vector(2 to 1); snqkje : linkage integer; wfchacspl : inout time);
end juwabias;

architecture alqs of juwabias is
  signal kev : boolean_vector(0 to 4);
  signal gk : bit;
  signal fxasxtv : real;
  signal hz : integer;
  signal czftb : time;
  signal slnymmilzn : real;
  signal tfvqkx : boolean_vector(0 to 4);
  signal suvhzh : bit;
begin
  rnrzhnfoov : entity work.a
    port map (pstupwjhmc => suvhzh, jv => tfvqkx, rvvz => slnymmilzn);
  frigm : entity work.dmaavi
    port map (jizmackv => slnymmilzn, zv => czftb, zyygaoo => hz, eblgw => fxasxtv);
  xpwost : entity work.a
    port map (pstupwjhmc => gk, jv => kev, rvvz => slnymmilzn);
  
  -- Single-driven assignments
  hx <= 0_4;
end alqs;

library ieee;
use ieee.std_logic_1164.all;

entity ueuflaz is
  port (kdnoyaeeml : in time_vector(1 to 3); k : out std_logic; kcujqpu : buffer integer_vector(1 downto 2));
end ueuflaz;

architecture vualik of ueuflaz is
  signal yckchtiidj : real;
  signal yrojro : boolean_vector(0 to 4);
  signal l : bit;
begin
  xxfhnfjw : entity work.a
    port map (pstupwjhmc => l, jv => yrojro, rvvz => yckchtiidj);
  
  -- Single-driven assignments
  kcujqpu <= (others => 0);
  yrojro <= (FALSE, TRUE, FALSE, FALSE, TRUE);
  
  -- Multi-driven assignments
  k <= 'X';
  k <= 'L';
  k <= '0';
  k <= '1';
end vualik;



-- Seed after: 9138705935693573853,17924494779688682807
