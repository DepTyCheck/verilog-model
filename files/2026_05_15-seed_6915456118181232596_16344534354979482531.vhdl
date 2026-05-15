-- Seed: 6915456118181232596,16344534354979482531



entity ih is
  port (gkqb : out integer; nmygc : in real);
end ih;



architecture lv of ih is
  
begin
  
end lv;

library ieee;
use ieee.std_logic_1164.all;

entity pwojt is
  port (przkicdq : buffer integer; vjn : out std_logic; rgeey : in boolean);
end pwojt;



architecture qrqqc of pwojt is
  signal vjgrfojrt : integer;
  signal xqwltyh : integer;
  signal v : real;
begin
  x : entity work.ih
    port map (gkqb => przkicdq, nmygc => v);
  vjcsfkb : entity work.ih
    port map (gkqb => xqwltyh, nmygc => v);
  yrnrvf : entity work.ih
    port map (gkqb => vjgrfojrt, nmygc => v);
end qrqqc;



entity tx is
  port (f : inout time; soiqkwb : inout integer);
end tx;

library ieee;
use ieee.std_logic_1164.all;

architecture dvozezo of tx is
  signal zxpqw : boolean;
  signal ifhzokz : std_logic;
begin
  liu : entity work.pwojt
    port map (przkicdq => soiqkwb, vjn => ifhzokz, rgeey => zxpqw);
end dvozezo;



-- Seed after: 3485249653270222237,16344534354979482531
