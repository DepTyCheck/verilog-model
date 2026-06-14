-- Seed: 9443725508106386456,6298042963991371649



entity zjsx is
  port (cdmwstlo : inout bit; vslqwz : linkage boolean; rkkeoi : in real);
end zjsx;



architecture ydifgdgy of zjsx is
  
begin
  
end ydifgdgy;



entity dtpklzqg is
  port (drum : out integer_vector(3 to 2));
end dtpklzqg;



architecture ccfvgqon of dtpklzqg is
  signal eduxinf : bit;
  signal yakppjzoq : real;
  signal lbkirvk : boolean;
  signal bxhjcmkagz : bit;
begin
  pnyus : entity work.zjsx
    port map (cdmwstlo => bxhjcmkagz, vslqwz => lbkirvk, rkkeoi => yakppjzoq);
  uotsea : entity work.zjsx
    port map (cdmwstlo => eduxinf, vslqwz => lbkirvk, rkkeoi => yakppjzoq);
end ccfvgqon;

library ieee;
use ieee.std_logic_1164.all;

entity wbqifhly is
  port (qbtvpv : linkage real; tfsu : out std_logic; mwznrvfpl : buffer std_logic);
end wbqifhly;



architecture dmw of wbqifhly is
  signal ovgpxeflf : real;
  signal leksvtv : boolean;
  signal va : bit;
  signal gnic : integer_vector(3 to 2);
begin
  cjqfntclw : entity work.dtpklzqg
    port map (drum => gnic);
  knupbhgx : entity work.zjsx
    port map (cdmwstlo => va, vslqwz => leksvtv, rkkeoi => ovgpxeflf);
end dmw;



-- Seed after: 10070812270841183101,6298042963991371649
