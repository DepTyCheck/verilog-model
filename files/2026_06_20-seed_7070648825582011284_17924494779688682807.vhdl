-- Seed: 7070648825582011284,17924494779688682807

entity qmvoj is
  port (kxqnqbegnc : inout severity_level; olw : in time; uhal : linkage bit_vector(3 to 1));
end qmvoj;

architecture yeps of qmvoj is
  
begin
  -- Single-driven assignments
  kxqnqbegnc <= ERROR;
end yeps;

library ieee;
use ieee.std_logic_1164.all;

entity soszoyua is
  port (s : linkage std_logic; itcdp : linkage time);
end soszoyua;

architecture ievmqo of soszoyua is
  signal qc : bit_vector(3 to 1);
  signal xncqmutsv : time;
  signal zqnqynekw : severity_level;
begin
  jotkvv : entity work.qmvoj
    port map (kxqnqbegnc => zqnqynekw, olw => xncqmutsv, uhal => qc);
end ievmqo;

library ieee;
use ieee.std_logic_1164.all;

entity pjb is
  port (bmmeiipg : buffer std_logic);
end pjb;

library ieee;
use ieee.std_logic_1164.all;

architecture ycq of pjb is
  signal wrcmaozymo : bit_vector(3 to 1);
  signal cqngwen : time;
  signal jcgwisggh : severity_level;
  signal mrv : bit_vector(3 to 1);
  signal yjxhwlu : time;
  signal giiiz : severity_level;
  signal dxntqz : time;
  signal tkrqk : std_logic;
begin
  gvj : entity work.soszoyua
    port map (s => tkrqk, itcdp => dxntqz);
  iwdnymx : entity work.qmvoj
    port map (kxqnqbegnc => giiiz, olw => yjxhwlu, uhal => mrv);
  p : entity work.qmvoj
    port map (kxqnqbegnc => jcgwisggh, olw => cqngwen, uhal => wrcmaozymo);
  
  -- Single-driven assignments
  yjxhwlu <= 03020.2_4 ms;
  cqngwen <= 8#4# ns;
  
  -- Multi-driven assignments
  bmmeiipg <= 'X';
  tkrqk <= 'U';
  bmmeiipg <= 'L';
  bmmeiipg <= 'Z';
end ycq;

entity xftgq is
  port (yi : out boolean; hnchhf : out time);
end xftgq;

architecture dbdexyyk of xftgq is
  
begin
  -- Single-driven assignments
  hnchhf <= 4 sec;
  yi <= TRUE;
end dbdexyyk;



-- Seed after: 3985321947137589365,17924494779688682807
