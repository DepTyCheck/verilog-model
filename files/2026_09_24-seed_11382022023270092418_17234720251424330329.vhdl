-- Seed: 11382022023270092418,17234720251424330329

entity ywjrlnj is
  port (asygbwctqf : buffer real; dxyerxn : in character; jm : buffer integer; oxljqj : in integer);
end ywjrlnj;

architecture irieyu of ywjrlnj is
  
begin
  -- Single-driven assignments
  asygbwctqf <= asygbwctqf;
  jm <= oxljqj;
end irieyu;

entity qdhwavqpv is
  port (cjx : linkage real; brxu : inout integer_vector(0 to 1));
end qdhwavqpv;

architecture sja of qdhwavqpv is
  signal fgxvg : real;
  signal fh : integer;
  signal tixltwuuq : integer;
  signal gfedl : character;
  signal ibdh : real;
begin
  bqoslvxdn : entity work.ywjrlnj
    port map (asygbwctqf => ibdh, dxyerxn => gfedl, jm => tixltwuuq, oxljqj => fh);
  ildgr : entity work.ywjrlnj
    port map (asygbwctqf => fgxvg, dxyerxn => gfedl, jm => fh, oxljqj => tixltwuuq);
  
  -- Single-driven assignments
  gfedl <= 'k';
  brxu <= (3, 16#5#);
end sja;

library ieee;
use ieee.std_logic_1164.all;

entity bqisqrkkwj is
  port (mokmlsq : linkage std_logic_vector(0 downto 3); ap : buffer std_logic; znoevnw : inout integer);
end bqisqrkkwj;

architecture mald of bqisqrkkwj is
  signal fyzuehehqi : integer_vector(0 to 1);
  signal sax : real;
  signal edv : integer_vector(0 to 1);
  signal qtqropbo : real;
  signal ijvwkv : integer_vector(0 to 1);
  signal oqhw : real;
begin
  molgda : entity work.qdhwavqpv
    port map (cjx => oqhw, brxu => ijvwkv);
  nlnwrmjn : entity work.qdhwavqpv
    port map (cjx => qtqropbo, brxu => edv);
  oltbtbe : entity work.qdhwavqpv
    port map (cjx => sax, brxu => fyzuehehqi);
  
  -- Single-driven assignments
  znoevnw <= 4;
  
  -- Multi-driven assignments
  ap <= 'X';
  ap <= 'U';
  ap <= 'W';
end mald;



-- Seed after: 11079010590724547969,17234720251424330329
