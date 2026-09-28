-- Seed: 13587133123848890232,7311216359267151659

entity ehv is
  port (z : in integer);
end ehv;

architecture uiaanl of ehv is
  
begin
  
end uiaanl;

library ieee;
use ieee.std_logic_1164.all;

entity lq is
  port (kwjbn : in std_logic);
end lq;

architecture xsdzn of lq is
  signal ziepjx : integer;
begin
  iioijrj : entity work.ehv
    port map (z => ziepjx);
  
  -- Single-driven assignments
  ziepjx <= 16#DD500#;
end xsdzn;

entity kypndpzgf is
  port (ljedvdyfr : out integer; qyxqfyfar : linkage boolean);
end kypndpzgf;

library ieee;
use ieee.std_logic_1164.all;

architecture jathwjxk of kypndpzgf is
  signal fergbbvx : std_logic;
  signal mrlxbscuk : integer;
begin
  lsyaxocbai : entity work.ehv
    port map (z => mrlxbscuk);
  bsak : entity work.lq
    port map (kwjbn => fergbbvx);
  gcdle : entity work.ehv
    port map (z => ljedvdyfr);
  
  -- Single-driven assignments
  mrlxbscuk <= ljedvdyfr;
  
  -- Multi-driven assignments
  fergbbvx <= fergbbvx;
  fergbbvx <= fergbbvx;
end jathwjxk;



-- Seed after: 13386048968556357042,7311216359267151659
