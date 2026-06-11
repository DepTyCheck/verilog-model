-- Seed: 5854201041021570116,11181851762153539145



entity pu is
  port (xxac : linkage integer);
end pu;



architecture tzrmkoqo of pu is
  
begin
  
end tzrmkoqo;



entity avl is
  port (clj : inout integer);
end avl;



architecture ktkcquub of avl is
  
begin
  izfno : entity work.pu
    port map (xxac => clj);
end ktkcquub;

library ieee;
use ieee.std_logic_1164.all;

entity xifqp is
  port (kxbt : in time; gkslmfshk : buffer std_logic);
end xifqp;



architecture lmiff of xifqp is
  signal vbedm : integer;
  signal xdn : integer;
begin
  tnjwzdiktr : entity work.pu
    port map (xxac => xdn);
  gjztykcnv : entity work.avl
    port map (clj => xdn);
  pnisseizl : entity work.pu
    port map (xxac => xdn);
  bhr : entity work.pu
    port map (xxac => vbedm);
end lmiff;



entity nreiwu is
  port (fkugo : linkage time);
end nreiwu;

library ieee;
use ieee.std_logic_1164.all;

architecture jiddidrymm of nreiwu is
  signal owdatn : integer;
  signal wqnmdryvvp : std_logic;
  signal nnukfxlgni : time;
begin
  fagcmy : entity work.xifqp
    port map (kxbt => nnukfxlgni, gkslmfshk => wqnmdryvvp);
  qimzgtx : entity work.pu
    port map (xxac => owdatn);
end jiddidrymm;



-- Seed after: 15496282362639657698,11181851762153539145
