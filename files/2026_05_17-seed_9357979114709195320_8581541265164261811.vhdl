-- Seed: 9357979114709195320,8581541265164261811



entity gzfllsdiv is
  port (sjtaby : buffer real; ah : linkage real; uwwcd : linkage time);
end gzfllsdiv;



architecture g of gzfllsdiv is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (biaqwsnjpc : out time; zjopdcxukv : linkage real; ppww : linkage time; omwobloe : inout std_logic);
end u;



architecture ukxrcmvcbm of u is
  signal dwasdddt : time;
  signal ayybijjfz : real;
  signal nygyyfykfw : real;
begin
  djrclqv : entity work.gzfllsdiv
    port map (sjtaby => nygyyfykfw, ah => zjopdcxukv, uwwcd => ppww);
  aavhjimlmk : entity work.gzfllsdiv
    port map (sjtaby => ayybijjfz, ah => zjopdcxukv, uwwcd => dwasdddt);
end ukxrcmvcbm;



entity caql is
  port (plptoxhin : inout integer);
end caql;

library ieee;
use ieee.std_logic_1164.all;

architecture cluvf of caql is
  signal gn : std_logic;
  signal hqghraetod : time;
  signal nfl : time;
  signal dqwaeqmwp : real;
  signal dnd : time;
  signal zwlhnoebn : real;
  signal cfuovnawhc : time;
  signal hnezqel : real;
  signal ybkobevo : real;
begin
  loymyoxy : entity work.gzfllsdiv
    port map (sjtaby => ybkobevo, ah => hnezqel, uwwcd => cfuovnawhc);
  ywdkg : entity work.gzfllsdiv
    port map (sjtaby => zwlhnoebn, ah => zwlhnoebn, uwwcd => dnd);
  nprsrxfjzm : entity work.gzfllsdiv
    port map (sjtaby => hnezqel, ah => dqwaeqmwp, uwwcd => nfl);
  qbqacvxzlm : entity work.u
    port map (biaqwsnjpc => cfuovnawhc, zjopdcxukv => dqwaeqmwp, ppww => hqghraetod, omwobloe => gn);
end cluvf;



-- Seed after: 15090580719272718960,8581541265164261811
