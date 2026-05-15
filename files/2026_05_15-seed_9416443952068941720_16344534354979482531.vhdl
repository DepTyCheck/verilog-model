-- Seed: 9416443952068941720,16344534354979482531



entity spvqmasw is
  port (pnsnsuon : in severity_level; atj : inout time; tbw : inout real; jxakfsg : in character);
end spvqmasw;



architecture bst of spvqmasw is
  
begin
  
end bst;



entity err is
  port (xjb : out real; smrdbs : out real);
end err;



architecture xuvkyrdcg of err is
  signal kzhpqirsn : time;
  signal qhahsh : severity_level;
  signal ul : real;
  signal vxxcdsme : time;
  signal efj : severity_level;
  signal kjgulyi : character;
  signal tmh : real;
  signal osefevqbw : time;
  signal f : severity_level;
begin
  wqzdgdmsxy : entity work.spvqmasw
    port map (pnsnsuon => f, atj => osefevqbw, tbw => tmh, jxakfsg => kjgulyi);
  hnary : entity work.spvqmasw
    port map (pnsnsuon => efj, atj => vxxcdsme, tbw => ul, jxakfsg => kjgulyi);
  zpiplquj : entity work.spvqmasw
    port map (pnsnsuon => qhahsh, atj => kzhpqirsn, tbw => smrdbs, jxakfsg => kjgulyi);
end xuvkyrdcg;



entity ungonu is
  port (iybhhmdbs : out real; qmc : out real; djyaymfoc : linkage boolean);
end ungonu;



architecture ia of ungonu is
  signal rkmmyu : character;
  signal meklwaebg : real;
  signal qu : time;
  signal rv : severity_level;
  signal kv : time;
  signal w : severity_level;
  signal arbdic : time;
  signal vu : character;
  signal wvzqjzbuje : real;
  signal oixzrxy : time;
  signal rkkrmh : severity_level;
begin
  pdrpbnajmz : entity work.spvqmasw
    port map (pnsnsuon => rkkrmh, atj => oixzrxy, tbw => wvzqjzbuje, jxakfsg => vu);
  ergk : entity work.spvqmasw
    port map (pnsnsuon => rkkrmh, atj => arbdic, tbw => iybhhmdbs, jxakfsg => vu);
  rmkv : entity work.spvqmasw
    port map (pnsnsuon => w, atj => kv, tbw => qmc, jxakfsg => vu);
  bxbhf : entity work.spvqmasw
    port map (pnsnsuon => rv, atj => qu, tbw => meklwaebg, jxakfsg => rkmmyu);
end ia;

library ieee;
use ieee.std_logic_1164.all;

entity cxfazqur is
  port (surgxn : linkage time; yjqmr : buffer real; oqzpbavj : out std_logic);
end cxfazqur;



architecture fkupdahg of cxfazqur is
  signal sbyoj : real;
  signal ayjbzeq : real;
  signal bcy : real;
  signal sjmwrgtotp : boolean;
  signal hdx : real;
  signal iscqdc : real;
  signal yxplnn : boolean;
  signal zpgymxzobc : real;
  signal jrmqihyex : real;
begin
  vjfj : entity work.ungonu
    port map (iybhhmdbs => jrmqihyex, qmc => zpgymxzobc, djyaymfoc => yxplnn);
  v : entity work.ungonu
    port map (iybhhmdbs => iscqdc, qmc => hdx, djyaymfoc => sjmwrgtotp);
  mrglw : entity work.err
    port map (xjb => bcy, smrdbs => ayjbzeq);
  hojyviz : entity work.ungonu
    port map (iybhhmdbs => yjqmr, qmc => sbyoj, djyaymfoc => yxplnn);
end fkupdahg;



-- Seed after: 5764001104014931951,16344534354979482531
