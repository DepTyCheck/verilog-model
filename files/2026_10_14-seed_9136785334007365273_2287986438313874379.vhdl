-- Seed: 9136785334007365273,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity exsd is
  port (yyjnmb : linkage boolean_vector(0 downto 2); ym : buffer std_logic_vector(0 downto 1); gqjvvi : inout real; yymqmcuvd : inout time);
end exsd;

architecture inhb of exsd is
  
begin
  -- Single-driven assignments
  gqjvvi <= 16#1.C#;
  
  -- Multi-driven assignments
  ym <= (others => '0');
  ym <= ym;
  ym <= ym;
  ym <= ym;
end inhb;

library ieee;
use ieee.std_logic_1164.all;

entity ikzpzqgf is
  port (ptqhbhwkno : in std_logic_vector(3 downto 0));
end ikzpzqgf;

library ieee;
use ieee.std_logic_1164.all;

architecture gazcynvrdi of ikzpzqgf is
  signal bthovez : time;
  signal fsqqvxjvp : real;
  signal ghblqq : boolean_vector(0 downto 2);
  signal djhpe : time;
  signal uezl : real;
  signal rork : std_logic_vector(0 downto 1);
  signal lvbmfmui : boolean_vector(0 downto 2);
begin
  xhcs : entity work.exsd
    port map (yyjnmb => lvbmfmui, ym => rork, gqjvvi => uezl, yymqmcuvd => djhpe);
  o : entity work.exsd
    port map (yyjnmb => ghblqq, ym => rork, gqjvvi => fsqqvxjvp, yymqmcuvd => bthovez);
  
  -- Multi-driven assignments
  rork <= rork;
  rork <= rork;
end gazcynvrdi;



-- Seed after: 6916814426665592909,2287986438313874379
