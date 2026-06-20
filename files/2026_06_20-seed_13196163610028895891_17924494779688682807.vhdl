-- Seed: 13196163610028895891,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity nhojsdg is
  port (fd : inout real; tex : in integer; jzy : inout std_logic);
end nhojsdg;

architecture uiwb of nhojsdg is
  
begin
  -- Single-driven assignments
  fd <= 31412.2;
  
  -- Multi-driven assignments
  jzy <= 'H';
end uiwb;

entity egsztmmj is
  port (ywp : buffer bit);
end egsztmmj;

library ieee;
use ieee.std_logic_1164.all;

architecture brclclxh of egsztmmj is
  signal qqywikd : std_logic;
  signal bexj : integer;
  signal ysfcjganny : real;
  signal ixqilfij : std_logic;
  signal dcp : real;
  signal hzqit : std_logic;
  signal dc : integer;
  signal whjyve : real;
begin
  i : entity work.nhojsdg
    port map (fd => whjyve, tex => dc, jzy => hzqit);
  j : entity work.nhojsdg
    port map (fd => dcp, tex => dc, jzy => ixqilfij);
  hlxrawuqv : entity work.nhojsdg
    port map (fd => ysfcjganny, tex => bexj, jzy => qqywikd);
  
  -- Single-driven assignments
  ywp <= '0';
  dc <= 1;
  
  -- Multi-driven assignments
  hzqit <= '-';
  hzqit <= 'H';
  ixqilfij <= 'Z';
  hzqit <= '1';
end brclclxh;



-- Seed after: 8867279583795574706,17924494779688682807
