-- Seed: 16716730661915455655,1834764876137802293

entity d is
  port (cpb : linkage integer);
end d;

architecture cizyfqfqk of d is
  
begin
  
end cizyfqfqk;

library ieee;
use ieee.std_logic_1164.all;

entity kkd is
  port (incgsmlzr : buffer string(2 downto 4); jrto : in std_logic_vector(4 downto 2); xuqevonxg : in integer);
end kkd;

architecture wumoftwqtu of kkd is
  
begin
  -- Single-driven assignments
  incgsmlzr <= "";
end wumoftwqtu;

library ieee;
use ieee.std_logic_1164.all;

entity rqlagrcx is
  port (ytq : in time_vector(1 downto 2); gv : buffer real; uenjctgn : in std_logic);
end rqlagrcx;

architecture g of rqlagrcx is
  
begin
  -- Single-driven assignments
  gv <= 2#1_0_1_0.0110#;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity ibo is
  port (lgf : out real_vector(1 to 3); x : buffer std_logic; uzkglnoqj : out time);
end ibo;

architecture hibcohvkub of ibo is
  signal ntgxg : real;
  signal jhqumvru : time_vector(1 downto 2);
  signal xiujm : integer;
  signal wawsvpilsx : integer;
  signal efv : real;
  signal iktlvyl : time_vector(1 downto 2);
begin
  ccjwhy : entity work.rqlagrcx
    port map (ytq => iktlvyl, gv => efv, uenjctgn => x);
  yxkwuhsu : entity work.d
    port map (cpb => wawsvpilsx);
  fywvaljlrw : entity work.d
    port map (cpb => xiujm);
  jbhzucmwlc : entity work.rqlagrcx
    port map (ytq => jhqumvru, gv => ntgxg, uenjctgn => x);
  
  -- Single-driven assignments
  lgf <= (1_0_4_1.30, 421.0340, 2#101.0_1_0#);
  iktlvyl <= (others => 0 ns);
  jhqumvru <= (others => 0 ns);
  uzkglnoqj <= 2#111# us;
  
  -- Multi-driven assignments
  x <= 'X';
  x <= 'L';
  x <= 'W';
end hibcohvkub;



-- Seed after: 4621101243434318078,1834764876137802293
