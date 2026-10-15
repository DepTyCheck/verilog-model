-- Seed: 17188263747132464905,6377581586043967497

entity lbsc is
  port (agthyr : inout time_vector(4 to 3); dhgjmvi : in time);
end lbsc;

architecture w of lbsc is
  
begin
  
end w;

entity pqxso is
  port (orgcdqcvjw : inout severity_level; ihvfxltmnr : inout integer; des : out character; svylzqd : out real);
end pqxso;

architecture zthnusrp of pqxso is
  
begin
  
end zthnusrp;

library ieee;
use ieee.std_logic_1164.all;

entity qsyeqqilo is
  port (xulzsmpqq : in std_logic; gus : in integer; tjzoogi : inout std_logic; vnhxodduuk : out std_logic);
end qsyeqqilo;

architecture jfeuqefl of qsyeqqilo is
  signal u : time_vector(4 to 3);
  signal ovdxr : time;
  signal eyiahdt : time_vector(4 to 3);
  signal wus : time;
  signal b : time_vector(4 to 3);
begin
  gitrbkuti : entity work.lbsc
    port map (agthyr => b, dhgjmvi => wus);
  xsh : entity work.lbsc
    port map (agthyr => eyiahdt, dhgjmvi => ovdxr);
  gvuhpbced : entity work.lbsc
    port map (agthyr => u, dhgjmvi => wus);
end jfeuqefl;

library ieee;
use ieee.std_logic_1164.all;

entity wdcl is
  port (abtadrul : out std_logic_vector(3 downto 0); jumw : inout real);
end wdcl;

library ieee;
use ieee.std_logic_1164.all;

architecture chx of wdcl is
  signal iiq : time;
  signal r : time_vector(4 to 3);
  signal ubixclf : time;
  signal kor : time_vector(4 to 3);
  signal tahlgkehc : real;
  signal xchf : character;
  signal kbqww : severity_level;
  signal vl : std_logic;
  signal ymabuxei : integer;
  signal ic : std_logic;
begin
  s : entity work.qsyeqqilo
    port map (xulzsmpqq => ic, gus => ymabuxei, tjzoogi => ic, vnhxodduuk => vl);
  vapdqi : entity work.pqxso
    port map (orgcdqcvjw => kbqww, ihvfxltmnr => ymabuxei, des => xchf, svylzqd => tahlgkehc);
  bwppr : entity work.lbsc
    port map (agthyr => kor, dhgjmvi => ubixclf);
  dgmqgefklv : entity work.lbsc
    port map (agthyr => r, dhgjmvi => iiq);
  
  -- Single-driven assignments
  jumw <= jumw;
  iiq <= ubixclf;
  ubixclf <= ubixclf;
  
  -- Multi-driven assignments
  ic <= '-';
  ic <= ic;
end chx;



-- Seed after: 2755151381832385011,6377581586043967497
