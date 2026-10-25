-- Seed: 3395291986443259583,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity sj is
  port (qrhjzzrcwm : in boolean; daqdo : inout std_logic; p : in time);
end sj;

architecture lcueltprkk of sj is
  
begin
  
end lcueltprkk;

library ieee;
use ieee.std_logic_1164.all;

entity lisdbqb is
  port (eqshw : inout std_logic_vector(3 downto 3));
end lisdbqb;

library ieee;
use ieee.std_logic_1164.all;

architecture gxejv of lisdbqb is
  signal feaytfdjct : time;
  signal uixirgl : std_logic;
  signal ozabwht : boolean;
  signal wjsexjph : time;
  signal rtbn : std_logic;
  signal pl : boolean;
  signal moomueri : time;
  signal rjacz : std_logic;
  signal cqtqcqdgt : boolean;
begin
  myacfhmm : entity work.sj
    port map (qrhjzzrcwm => cqtqcqdgt, daqdo => rjacz, p => moomueri);
  l : entity work.sj
    port map (qrhjzzrcwm => pl, daqdo => rtbn, p => wjsexjph);
  ox : entity work.sj
    port map (qrhjzzrcwm => ozabwht, daqdo => uixirgl, p => feaytfdjct);
  
  -- Single-driven assignments
  cqtqcqdgt <= cqtqcqdgt;
  moomueri <= 1 hr;
  feaytfdjct <= 2_1 fs;
  
  -- Multi-driven assignments
  uixirgl <= '1';
end gxejv;

entity lw is
  port (opdvwvnyu : out integer_vector(1 downto 0); qdy : linkage time);
end lw;

library ieee;
use ieee.std_logic_1164.all;

architecture nnrkpbo of lw is
  signal albtdmjuzj : std_logic_vector(3 downto 3);
  signal qnk : boolean;
  signal vyyaoho : time;
  signal ykxstmlj : std_logic;
  signal dlbp : boolean;
begin
  sihaysq : entity work.sj
    port map (qrhjzzrcwm => dlbp, daqdo => ykxstmlj, p => vyyaoho);
  l : entity work.sj
    port map (qrhjzzrcwm => qnk, daqdo => ykxstmlj, p => vyyaoho);
  wsnqg : entity work.lisdbqb
    port map (eqshw => albtdmjuzj);
  t : entity work.lisdbqb
    port map (eqshw => albtdmjuzj);
  
  -- Single-driven assignments
  opdvwvnyu <= (8#311#, 2#10001#);
  vyyaoho <= vyyaoho;
  dlbp <= dlbp;
  qnk <= FALSE;
  
  -- Multi-driven assignments
  ykxstmlj <= ykxstmlj;
  ykxstmlj <= '0';
end nnrkpbo;



-- Seed after: 14398517843013442580,13788294582928374589
