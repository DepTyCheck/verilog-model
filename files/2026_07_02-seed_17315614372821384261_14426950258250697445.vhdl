-- Seed: 17315614372821384261,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity fulnnh is
  port (ozlbl : out time; ymovthey : buffer time; xgo : linkage std_logic_vector(2 to 2));
end fulnnh;

architecture endlmvygvq of fulnnh is
  
begin
  -- Single-driven assignments
  ymovthey <= ymovthey;
  ozlbl <= 104 ps;
end endlmvygvq;

library ieee;
use ieee.std_logic_1164.all;

entity lcwyq is
  port (pebcqpqtmm : inout time; y : linkage integer; obwodnbx : buffer std_logic_vector(4 to 0));
end lcwyq;

architecture qrvhqpq of lcwyq is
  
begin
  -- Single-driven assignments
  pebcqpqtmm <= 0 sec;
end qrvhqpq;

library ieee;
use ieee.std_logic_1164.all;

entity gd is
  port (lyyis : buffer std_logic);
end gd;

library ieee;
use ieee.std_logic_1164.all;

architecture nhcno of gd is
  signal clomz : time;
  signal ufwrssofbq : time;
  signal wqgpxr : std_logic_vector(2 to 2);
  signal u : time;
  signal yaynqlri : time;
begin
  zqfkuttigl : entity work.fulnnh
    port map (ozlbl => yaynqlri, ymovthey => u, xgo => wqgpxr);
  xddecnfm : entity work.fulnnh
    port map (ozlbl => ufwrssofbq, ymovthey => clomz, xgo => wqgpxr);
  
  -- Multi-driven assignments
  lyyis <= 'H';
  lyyis <= 'H';
  lyyis <= 'X';
end nhcno;

use std.reflection.all;

entity dpwk is
  port (jcwx : linkage integer; uhnmdrbv : linkage integer; vmvemmv : inout protected_subtype_mirror);
end dpwk;

library ieee;
use ieee.std_logic_1164.all;

architecture bs of dpwk is
  signal yspx : std_logic_vector(2 to 2);
  signal qztsru : time;
  signal hyiych : time;
  signal qjtw : std_logic_vector(2 to 2);
  signal hkyvzy : time;
  signal phjibbgdat : time;
  signal ngr : std_logic_vector(4 to 0);
  signal vv : time;
begin
  ughydffgqv : entity work.lcwyq
    port map (pebcqpqtmm => vv, y => uhnmdrbv, obwodnbx => ngr);
  bhsdsbd : entity work.fulnnh
    port map (ozlbl => phjibbgdat, ymovthey => hkyvzy, xgo => qjtw);
  cpssmrevd : entity work.fulnnh
    port map (ozlbl => hyiych, ymovthey => qztsru, xgo => yspx);
  
  -- Multi-driven assignments
  yspx <= "X";
  qjtw <= yspx;
end bs;



-- Seed after: 7537364865653003827,14426950258250697445
