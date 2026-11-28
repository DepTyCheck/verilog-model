-- Seed: 7001508166058281823,15306146742274979323

entity m is
  port (wofv : out integer; xpxjht : inout integer);
end m;

architecture oke of m is
  
begin
  -- Single-driven assignments
  xpxjht <= wofv;
  wofv <= wofv;
end oke;

library ieee;
use ieee.std_logic_1164.all;

entity jmy is
  port (bcoc : in time; pxm : buffer std_logic; yngbd : in std_logic_vector(4 downto 4));
end jmy;

architecture eniw of jmy is
  
begin
  
end eniw;

entity fzmjsfhct is
  port (xryhe : in time);
end fzmjsfhct;

architecture ohs of fzmjsfhct is
  signal yuotveghip : integer;
  signal xhaxvkag : integer;
begin
  q : entity work.m
    port map (wofv => xhaxvkag, xpxjht => yuotveghip);
end ohs;

library ieee;
use ieee.std_logic_1164.all;

entity ooisit is
  port (kqmrka : buffer integer_vector(3 downto 4); yikdxdd : inout integer; svfdm : in real; razhzuaak : inout std_logic);
end ooisit;

library ieee;
use ieee.std_logic_1164.all;

architecture cvox of ooisit is
  signal tsp : integer;
  signal h : std_logic_vector(4 downto 4);
  signal mtzod : time;
begin
  uqkdixzfvp : entity work.jmy
    port map (bcoc => mtzod, pxm => razhzuaak, yngbd => h);
  c : entity work.m
    port map (wofv => tsp, xpxjht => yikdxdd);
  
  -- Single-driven assignments
  kqmrka <= kqmrka;
  mtzod <= 16#2F0B2# us;
end cvox;



-- Seed after: 7306230687865289068,15306146742274979323
