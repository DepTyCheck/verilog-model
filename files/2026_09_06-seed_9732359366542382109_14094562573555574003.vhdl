-- Seed: 9732359366542382109,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity fwtpdw is
  port (wnki : linkage real; uvkots : buffer integer; rspjjsf : in std_logic_vector(3 to 2); jjjlqpyg : inout boolean);
end fwtpdw;

architecture idqdeae of fwtpdw is
  
begin
  
end idqdeae;

entity x is
  port (f : inout integer);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture hoq of x is
  signal ztq : boolean;
  signal ursioyrcgx : integer;
  signal bcxre : real;
  signal lciq : boolean;
  signal qoeolompdw : std_logic_vector(3 to 2);
  signal yv : real;
begin
  nz : entity work.fwtpdw
    port map (wnki => yv, uvkots => f, rspjjsf => qoeolompdw, jjjlqpyg => lciq);
  rwva : entity work.fwtpdw
    port map (wnki => bcxre, uvkots => ursioyrcgx, rspjjsf => qoeolompdw, jjjlqpyg => ztq);
  
  -- Multi-driven assignments
  qoeolompdw <= (others => '0');
  qoeolompdw <= (others => '0');
  qoeolompdw <= qoeolompdw;
  qoeolompdw <= qoeolompdw;
end hoq;

entity ea is
  port (znzb : inout real; wyy : linkage real_vector(4 to 4));
end ea;

library ieee;
use ieee.std_logic_1164.all;

architecture c of ea is
  signal fbcf : integer;
  signal xrlkgswudx : boolean;
  signal csgmak : std_logic_vector(3 to 2);
  signal odklyw : integer;
  signal qiox : boolean;
  signal hhgw : std_logic_vector(3 to 2);
  signal tbpb : integer;
  signal rmdps : real;
begin
  qjotjwekd : entity work.fwtpdw
    port map (wnki => rmdps, uvkots => tbpb, rspjjsf => hhgw, jjjlqpyg => qiox);
  iodmnvoia : entity work.fwtpdw
    port map (wnki => znzb, uvkots => odklyw, rspjjsf => csgmak, jjjlqpyg => xrlkgswudx);
  echrgnoxjt : entity work.x
    port map (f => fbcf);
  
  -- Multi-driven assignments
  hhgw <= "";
  hhgw <= hhgw;
  hhgw <= "";
  csgmak <= (others => '0');
end c;



-- Seed after: 11339781456030082371,14094562573555574003
