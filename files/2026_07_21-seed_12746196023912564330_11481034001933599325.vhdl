-- Seed: 12746196023912564330,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity pdvnggw is
  port (yzmpwhfw : linkage std_logic; fytlg : in integer_vector(2 downto 4); ds : out real);
end pdvnggw;

architecture mutifx of pdvnggw is
  
begin
  
end mutifx;

entity dnubxavm is
  port (cxqsevoeh : inout time);
end dnubxavm;

library ieee;
use ieee.std_logic_1164.all;

architecture yst of dnubxavm is
  signal cieawghv : real;
  signal lesmkvhcbh : integer_vector(2 downto 4);
  signal sylhbbc : std_logic;
  signal nt : real;
  signal dgqp : integer_vector(2 downto 4);
  signal qgogc : real;
  signal gycg : integer_vector(2 downto 4);
  signal nkte : std_logic;
begin
  qhegj : entity work.pdvnggw
    port map (yzmpwhfw => nkte, fytlg => gycg, ds => qgogc);
  vqzf : entity work.pdvnggw
    port map (yzmpwhfw => nkte, fytlg => dgqp, ds => nt);
  lnoxa : entity work.pdvnggw
    port map (yzmpwhfw => sylhbbc, fytlg => lesmkvhcbh, ds => cieawghv);
  
  -- Single-driven assignments
  cxqsevoeh <= 34.3_3_2 ns;
  
  -- Multi-driven assignments
  nkte <= nkte;
  nkte <= '-';
  sylhbbc <= nkte;
  nkte <= nkte;
end yst;



-- Seed after: 6196589373341328438,11481034001933599325
