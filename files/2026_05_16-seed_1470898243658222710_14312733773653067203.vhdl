-- Seed: 1470898243658222710,14312733773653067203

library ieee;
use ieee.std_logic_1164.all;

entity tbk is
  port (swhng : buffer std_logic; vlyp : out real; kwumzb : buffer real; avuco : inout time);
end tbk;



architecture alvvxeaqta of tbk is
  
begin
  
end alvvxeaqta;

library ieee;
use ieee.std_logic_1164.all;

entity yfdd is
  port (mlewtjm : inout integer; mshhoagjr : inout integer; rdsocsw : inout std_logic);
end yfdd;

library ieee;
use ieee.std_logic_1164.all;

architecture rge of yfdd is
  signal xqbafkqt : time;
  signal mpljt : real;
  signal ida : real;
  signal zwepu : time;
  signal iccubf : real;
  signal hlefhwywyz : real;
  signal cqn : time;
  signal pcaywk : real;
  signal mvrddc : real;
  signal fdxvkz : time;
  signal j : real;
  signal xnxnw : real;
  signal wtb : std_logic;
begin
  bbfolqo : entity work.tbk
    port map (swhng => wtb, vlyp => xnxnw, kwumzb => j, avuco => fdxvkz);
  t : entity work.tbk
    port map (swhng => wtb, vlyp => mvrddc, kwumzb => pcaywk, avuco => cqn);
  mueignfzsp : entity work.tbk
    port map (swhng => rdsocsw, vlyp => hlefhwywyz, kwumzb => iccubf, avuco => zwepu);
  tuycg : entity work.tbk
    port map (swhng => wtb, vlyp => ida, kwumzb => mpljt, avuco => xqbafkqt);
end rge;



entity vypa is
  port (osuol : inout time; ylxb : in integer; nvt : inout integer);
end vypa;

library ieee;
use ieee.std_logic_1164.all;

architecture lwf of vypa is
  signal wx : time;
  signal a : real;
  signal tyzvdt : real;
  signal fnkvg : std_logic;
  signal am : time;
  signal pmbtw : real;
  signal bqpr : real;
  signal kddlcaqms : real;
  signal lonzfgl : real;
  signal tnrqdfssn : std_logic;
  signal evdjpecb : std_logic;
  signal axadlve : integer;
begin
  ney : entity work.yfdd
    port map (mlewtjm => axadlve, mshhoagjr => nvt, rdsocsw => evdjpecb);
  x : entity work.tbk
    port map (swhng => tnrqdfssn, vlyp => lonzfgl, kwumzb => kddlcaqms, avuco => osuol);
  hqmmrpof : entity work.tbk
    port map (swhng => tnrqdfssn, vlyp => bqpr, kwumzb => pmbtw, avuco => am);
  rnah : entity work.tbk
    port map (swhng => fnkvg, vlyp => tyzvdt, kwumzb => a, avuco => wx);
end lwf;



-- Seed after: 1953313731903503378,14312733773653067203
