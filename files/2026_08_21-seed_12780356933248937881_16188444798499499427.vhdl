-- Seed: 12780356933248937881,16188444798499499427

entity borozgwje is
  port (q : out integer_vector(2 downto 0));
end borozgwje;

architecture frkzsnr of borozgwje is
  
begin
  -- Single-driven assignments
  q <= (1_3_3_0, 2#0_0#, 2#1_0_0#);
end frkzsnr;

entity t is
  port (dxyk : in severity_level; lpev : out integer; mw : inout integer);
end t;

architecture wdwsiyeqx of t is
  signal vhmimkw : integer_vector(2 downto 0);
  signal oxnvv : integer_vector(2 downto 0);
  signal h : integer_vector(2 downto 0);
  signal borcmhzn : integer_vector(2 downto 0);
begin
  sxtcyzqfsb : entity work.borozgwje
    port map (q => borcmhzn);
  jzvhmhgvia : entity work.borozgwje
    port map (q => h);
  jjdeu : entity work.borozgwje
    port map (q => oxnvv);
  oxlldgrknq : entity work.borozgwje
    port map (q => vhmimkw);
  
  -- Single-driven assignments
  mw <= 3;
end wdwsiyeqx;

library ieee;
use ieee.std_logic_1164.all;

entity zty is
  port (lg : buffer integer; aukqykrwb : buffer std_logic_vector(2 to 1); iltzkyxws : inout integer; kc : inout boolean_vector(0 downto 0));
end zty;

architecture joghuvzgt of zty is
  signal cq : integer;
  signal wfwtqbmzq : severity_level;
begin
  shus : entity work.t
    port map (dxyk => wfwtqbmzq, lpev => cq, mw => lg);
  
  -- Single-driven assignments
  kc <= kc;
  iltzkyxws <= iltzkyxws;
  wfwtqbmzq <= WARNING;
  
  -- Multi-driven assignments
  aukqykrwb <= (others => '0');
  aukqykrwb <= aukqykrwb;
end joghuvzgt;



-- Seed after: 543154430305247520,16188444798499499427
