-- Seed: 6976450857759974074,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity hycdcv is
  port (dlxozvuw : linkage real; ffqwyrr : in bit; fqvjpvwpr : in std_logic_vector(4 to 0); kqnzb : inout integer);
end hycdcv;

architecture kplbegxvjn of hycdcv is
  
begin
  
end kplbegxvjn;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (skvav : in time; rnhlnmv : buffer std_logic_vector(1 to 3));
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture xgqkvm of y is
  signal pnmxheqnvx : integer;
  signal uxzyzotif : std_logic_vector(4 to 0);
  signal zvwxcnu : bit;
  signal cryysc : real;
  signal pikn : integer;
  signal u : real;
  signal braqxfh : integer;
  signal fnod : std_logic_vector(4 to 0);
  signal zcizrxkaf : bit;
  signal nbns : real;
begin
  egwdvgo : entity work.hycdcv
    port map (dlxozvuw => nbns, ffqwyrr => zcizrxkaf, fqvjpvwpr => fnod, kqnzb => braqxfh);
  oxxgidvcb : entity work.hycdcv
    port map (dlxozvuw => u, ffqwyrr => zcizrxkaf, fqvjpvwpr => fnod, kqnzb => pikn);
  bnt : entity work.hycdcv
    port map (dlxozvuw => cryysc, ffqwyrr => zvwxcnu, fqvjpvwpr => uxzyzotif, kqnzb => pnmxheqnvx);
  
  -- Single-driven assignments
  zcizrxkaf <= '0';
  zvwxcnu <= '0';
  
  -- Multi-driven assignments
  uxzyzotif <= (others => '0');
  rnhlnmv <= ('0', 'U', 'H');
end xgqkvm;



-- Seed after: 11791938905886249048,5472058987609252853
