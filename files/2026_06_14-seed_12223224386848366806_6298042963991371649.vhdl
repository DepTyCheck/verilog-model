-- Seed: 12223224386848366806,6298042963991371649



entity ob is
  port (kj : buffer real; ev : in real);
end ob;



architecture g of ob is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity idyscdpxun is
  port (jjox : in boolean; vvj : buffer std_logic_vector(4 downto 4));
end idyscdpxun;



architecture wpsqins of idyscdpxun is
  signal hvsoppn : real;
  signal qzn : real;
  signal wrdccr : real;
  signal t : real;
begin
  rqmllb : entity work.ob
    port map (kj => t, ev => wrdccr);
  fnboq : entity work.ob
    port map (kj => qzn, ev => hvsoppn);
end wpsqins;



entity kerjnclgsw is
  port (hxh : inout severity_level; dnifwugcf : in real; uktxvvqi : buffer real);
end kerjnclgsw;

library ieee;
use ieee.std_logic_1164.all;

architecture fqdhoq of kerjnclgsw is
  signal faoov : real;
  signal ovk : real;
  signal v : std_logic_vector(4 downto 4);
  signal csjjorm : boolean;
  signal rhyu : real;
begin
  avdco : entity work.ob
    port map (kj => rhyu, ev => dnifwugcf);
  pxgansiqnt : entity work.idyscdpxun
    port map (jjox => csjjorm, vvj => v);
  ampogtsdse : entity work.ob
    port map (kj => uktxvvqi, ev => ovk);
  c : entity work.ob
    port map (kj => ovk, ev => faoov);
end fqdhoq;



-- Seed after: 285743940415163304,6298042963991371649
