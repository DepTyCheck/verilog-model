-- Seed: 3571716894529071695,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (l : in std_logic_vector(0 to 2); yliybho : buffer integer);
end y;

architecture rcrkdpxxn of y is
  
begin
  -- Single-driven assignments
  yliybho <= 2#01111#;
end rcrkdpxxn;

entity mil is
  port (vzzk : inout bit; mpyz : out real; jxn : inout time; wplzfrvths : linkage integer);
end mil;

library ieee;
use ieee.std_logic_1164.all;

architecture oexgkui of mil is
  signal tkyviwv : integer;
  signal nbjiuvoujw : std_logic_vector(0 to 2);
  signal lwbvv : integer;
  signal wxaqd : std_logic_vector(0 to 2);
  signal sfh : integer;
  signal rrbhjtwzl : std_logic_vector(0 to 2);
begin
  prdiussj : entity work.y
    port map (l => rrbhjtwzl, yliybho => sfh);
  a : entity work.y
    port map (l => wxaqd, yliybho => lwbvv);
  uqtdhqbr : entity work.y
    port map (l => nbjiuvoujw, yliybho => tkyviwv);
  
  -- Multi-driven assignments
  rrbhjtwzl <= rrbhjtwzl;
  rrbhjtwzl <= "1L0";
  rrbhjtwzl <= ('0', 'L', 'X');
  rrbhjtwzl <= rrbhjtwzl;
end oexgkui;

entity engjk is
  port (zsdniqh : buffer integer; ocyisl : buffer bit);
end engjk;

library ieee;
use ieee.std_logic_1164.all;

architecture dqobah of engjk is
  signal tixsgolfo : integer;
  signal qzyfpptul : time;
  signal jaagb : real;
  signal qgtodvhgfb : integer;
  signal aypjed : integer;
  signal cfz : std_logic_vector(0 to 2);
  signal ddxpsr : std_logic_vector(0 to 2);
begin
  vrnagaikqa : entity work.y
    port map (l => ddxpsr, yliybho => zsdniqh);
  uik : entity work.y
    port map (l => cfz, yliybho => aypjed);
  d : entity work.y
    port map (l => ddxpsr, yliybho => qgtodvhgfb);
  cegshalvap : entity work.mil
    port map (vzzk => ocyisl, mpyz => jaagb, jxn => qzyfpptul, wplzfrvths => tixsgolfo);
end dqobah;



-- Seed after: 5977730481604722727,13196211255131729027
