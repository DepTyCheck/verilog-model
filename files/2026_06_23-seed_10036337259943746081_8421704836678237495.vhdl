-- Seed: 10036337259943746081,8421704836678237495

entity w is
  port (esqwkn : linkage integer; ths : buffer time);
end w;

architecture awj of w is
  
begin
  -- Single-driven assignments
  ths <= 4 sec;
end awj;

entity odd is
  port (nzxe : inout integer);
end odd;

architecture vbi of odd is
  signal yr : time;
  signal cgzfsmsn : time;
  signal lzvjsdfpc : integer;
  signal fscru : time;
  signal evvekvbjux : integer;
begin
  qrzgob : entity work.w
    port map (esqwkn => evvekvbjux, ths => fscru);
  jia : entity work.w
    port map (esqwkn => lzvjsdfpc, ths => cgzfsmsn);
  gypytz : entity work.w
    port map (esqwkn => nzxe, ths => yr);
end vbi;

entity kmz is
  port (zyfwyaa : in time);
end kmz;

architecture jh of kmz is
  signal zmyjhngme : integer;
  signal tixr : time;
  signal xcfkvn : integer;
  signal ltxclcgdmg : time;
  signal ockfnnivh : integer;
begin
  nxwjjqfngv : entity work.w
    port map (esqwkn => ockfnnivh, ths => ltxclcgdmg);
  ubkl : entity work.w
    port map (esqwkn => xcfkvn, ths => tixr);
  tljsjwmo : entity work.odd
    port map (nzxe => zmyjhngme);
end jh;

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (rlu : buffer std_logic_vector(3 to 1); dzjfqeex : in integer_vector(0 downto 0); pmdn : in real);
end j;

architecture qdw of j is
  signal wns : time;
  signal knpbowmyn : integer;
  signal pmgpls : time;
  signal tfohdde : integer;
begin
  whyjze : entity work.w
    port map (esqwkn => tfohdde, ths => pmgpls);
  yjshb : entity work.kmz
    port map (zyfwyaa => pmgpls);
  l : entity work.w
    port map (esqwkn => knpbowmyn, ths => wns);
  
  -- Multi-driven assignments
  rlu <= "";
  rlu <= "";
end qdw;



-- Seed after: 9227818195394983393,8421704836678237495
