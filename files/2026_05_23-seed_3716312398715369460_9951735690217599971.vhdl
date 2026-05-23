-- Seed: 3716312398715369460,9951735690217599971



entity pgajdl is
  port (phkj : in boolean; tgwxleeq : buffer boolean);
end pgajdl;



architecture j of pgajdl is
  
begin
  
end j;

library ieee;
use ieee.std_logic_1164.all;

entity bh is
  port (g : buffer std_logic; vqahidjea : buffer boolean);
end bh;



architecture zrehuxgld of bh is
  signal kcba : boolean;
begin
  ud : entity work.pgajdl
    port map (phkj => vqahidjea, tgwxleeq => vqahidjea);
  kaxfefg : entity work.pgajdl
    port map (phkj => vqahidjea, tgwxleeq => kcba);
end zrehuxgld;



entity usd is
  port (nrbhq : linkage time; kmw : in integer);
end usd;



architecture nolz of usd is
  signal xvzn : boolean;
  signal phnzddiyb : boolean;
  signal rz : boolean;
begin
  lxntemotfl : entity work.pgajdl
    port map (phkj => rz, tgwxleeq => phnzddiyb);
  ouzs : entity work.pgajdl
    port map (phkj => xvzn, tgwxleeq => xvzn);
end nolz;

library ieee;
use ieee.std_logic_1164.all;

entity tb is
  port (avswrkdqe : buffer std_logic; yjc : inout time; neeaaqk : inout real; x : out std_logic);
end tb;



architecture dgf of tb is
  signal kwnyxbte : boolean;
  signal roxezibau : boolean;
  signal cst : boolean;
  signal mlhsancqiw : boolean;
  signal xdgcezqr : integer;
begin
  c : entity work.usd
    port map (nrbhq => yjc, kmw => xdgcezqr);
  vkit : entity work.pgajdl
    port map (phkj => mlhsancqiw, tgwxleeq => cst);
  eekku : entity work.pgajdl
    port map (phkj => mlhsancqiw, tgwxleeq => roxezibau);
  yy : entity work.pgajdl
    port map (phkj => kwnyxbte, tgwxleeq => mlhsancqiw);
end dgf;



-- Seed after: 10027579059085988478,9951735690217599971
