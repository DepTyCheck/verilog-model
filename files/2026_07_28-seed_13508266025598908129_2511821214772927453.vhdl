-- Seed: 13508266025598908129,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity ppi is
  port (xxsofgnpk : buffer std_logic_vector(4 downto 1); grit : inout time; h : in integer);
end ppi;

architecture ltstsnxdda of ppi is
  
begin
  -- Single-driven assignments
  grit <= 4_0_4_2 us;
end ltstsnxdda;

library ieee;
use ieee.std_logic_1164.all;

entity xnjxkeffbm is
  port (xqhba : buffer std_logic; lbks : inout integer_vector(4 downto 2));
end xnjxkeffbm;

library ieee;
use ieee.std_logic_1164.all;

architecture qnkdeubm of xnjxkeffbm is
  signal uhie : integer;
  signal cscunsavhu : time;
  signal g : std_logic_vector(4 downto 1);
begin
  fz : entity work.ppi
    port map (xxsofgnpk => g, grit => cscunsavhu, h => uhie);
  
  -- Single-driven assignments
  uhie <= 1_3_0_2;
  
  -- Multi-driven assignments
  xqhba <= 'H';
  xqhba <= xqhba;
  g <= ('U', '0', '0', '1');
end qnkdeubm;

entity vcb is
  port (f : out real; jsg : buffer integer);
end vcb;

architecture rkbg of vcb is
  
begin
  -- Single-driven assignments
  jsg <= jsg;
end rkbg;

entity gihjhmhmos is
  port (popzz : linkage bit_vector(4 downto 1); cnmbyouf : linkage integer);
end gihjhmhmos;

library ieee;
use ieee.std_logic_1164.all;

architecture l of gihjhmhmos is
  signal kjdanpdcth : integer_vector(4 downto 2);
  signal aopk : std_logic;
  signal liezr : integer_vector(4 downto 2);
  signal ncutkcw : std_logic;
begin
  ggyepemwl : entity work.xnjxkeffbm
    port map (xqhba => ncutkcw, lbks => liezr);
  fo : entity work.xnjxkeffbm
    port map (xqhba => aopk, lbks => kjdanpdcth);
  
  -- Multi-driven assignments
  aopk <= '1';
end l;



-- Seed after: 13401130720471105489,2511821214772927453
