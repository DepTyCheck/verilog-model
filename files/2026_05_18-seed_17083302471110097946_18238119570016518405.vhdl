-- Seed: 17083302471110097946,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity unuoie is
  port (jvzyu : buffer severity_level; ovuhsuze : buffer std_logic_vector(1 to 4); mww : in time; u : buffer std_logic_vector(2 to 3));
end unuoie;



architecture axbznfmpp of unuoie is
  
begin
  
end axbznfmpp;

library ieee;
use ieee.std_logic_1164.all;

entity irer is
  port (s : out integer; sfhomoyuio : inout std_logic);
end irer;

library ieee;
use ieee.std_logic_1164.all;

architecture e of irer is
  signal mnw : std_logic_vector(2 to 3);
  signal zxw : std_logic_vector(1 to 4);
  signal bfmuxyxu : severity_level;
  signal sykqqj : std_logic_vector(2 to 3);
  signal ojtdowb : time;
  signal bqyotupt : std_logic_vector(1 to 4);
  signal rqzszua : severity_level;
begin
  cybezgmk : entity work.unuoie
    port map (jvzyu => rqzszua, ovuhsuze => bqyotupt, mww => ojtdowb, u => sykqqj);
  ty : entity work.unuoie
    port map (jvzyu => bfmuxyxu, ovuhsuze => zxw, mww => ojtdowb, u => mnw);
end e;



entity omckskx is
  port (uvhlhqzdon : linkage bit; p : out integer_vector(0 to 4));
end omckskx;

library ieee;
use ieee.std_logic_1164.all;

architecture kcnqedocg of omckskx is
  signal y : std_logic_vector(2 to 3);
  signal jadjrohpu : time;
  signal osbxy : std_logic_vector(1 to 4);
  signal nx : severity_level;
begin
  zrno : entity work.unuoie
    port map (jvzyu => nx, ovuhsuze => osbxy, mww => jadjrohpu, u => y);
end kcnqedocg;



entity uddrnvlmxs is
  port (kcaxh : inout integer);
end uddrnvlmxs;

library ieee;
use ieee.std_logic_1164.all;

architecture duy of uddrnvlmxs is
  signal frutjb : std_logic_vector(2 to 3);
  signal vtt : std_logic_vector(1 to 4);
  signal htr : severity_level;
  signal yfsbscywc : std_logic_vector(2 to 3);
  signal uvze : time;
  signal ny : std_logic_vector(1 to 4);
  signal zyujnec : severity_level;
  signal bpminrbnxr : std_logic;
  signal hzldv : integer;
begin
  cdcti : entity work.irer
    port map (s => hzldv, sfhomoyuio => bpminrbnxr);
  pwkylydp : entity work.unuoie
    port map (jvzyu => zyujnec, ovuhsuze => ny, mww => uvze, u => yfsbscywc);
  d : entity work.unuoie
    port map (jvzyu => htr, ovuhsuze => vtt, mww => uvze, u => frutjb);
end duy;



-- Seed after: 5548043202632534752,18238119570016518405
