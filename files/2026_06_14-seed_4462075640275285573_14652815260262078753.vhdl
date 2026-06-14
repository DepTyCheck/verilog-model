-- Seed: 4462075640275285573,14652815260262078753

entity bhbubaeq is
  port (m : in severity_level);
end bhbubaeq;

architecture l of bhbubaeq is
  
begin
  
end l;

library ieee;
use ieee.std_logic_1164.all;

entity zlxlkxuphc is
  port (nogve : inout time_vector(0 to 0); fbsfrnzh : out integer; tc : in character; aqpuidw : out std_logic_vector(1 to 1));
end zlxlkxuphc;

architecture z of zlxlkxuphc is
  signal zdpsjjbpri : severity_level;
begin
  ndixzt : entity work.bhbubaeq
    port map (m => zdpsjjbpri);
  
  -- Single-driven assignments
  fbsfrnzh <= 1;
  zdpsjjbpri <= ERROR;
  nogve <= (others => 2#01.11101# ps);
  
  -- Multi-driven assignments
  aqpuidw <= "-";
end z;

library ieee;
use ieee.std_logic_1164.all;

entity pdbu is
  port (y : out std_logic);
end pdbu;

library ieee;
use ieee.std_logic_1164.all;

architecture ahqhdk of pdbu is
  signal sb : std_logic_vector(1 to 1);
  signal oc : character;
  signal ymhjozdhiw : integer;
  signal fvvlz : time_vector(0 to 0);
  signal esewljkv : severity_level;
begin
  f : entity work.bhbubaeq
    port map (m => esewljkv);
  w : entity work.zlxlkxuphc
    port map (nogve => fvvlz, fbsfrnzh => ymhjozdhiw, tc => oc, aqpuidw => sb);
  
  -- Single-driven assignments
  oc <= 'y';
  esewljkv <= WARNING;
  
  -- Multi-driven assignments
  y <= 'U';
end ahqhdk;

entity tsc is
  port (zcgca : out time; adxqnh : linkage time; hifnywlzte : in time);
end tsc;

library ieee;
use ieee.std_logic_1164.all;

architecture ujotgtq of tsc is
  signal v : std_logic_vector(1 to 1);
  signal wuvq : integer;
  signal nshoo : time_vector(0 to 0);
  signal wdwywongd : severity_level;
  signal dfmpzw : std_logic_vector(1 to 1);
  signal ylolavbtk : character;
  signal fwogocgvv : integer;
  signal vjdc : time_vector(0 to 0);
  signal gs : severity_level;
begin
  lpu : entity work.bhbubaeq
    port map (m => gs);
  jueyqtcr : entity work.zlxlkxuphc
    port map (nogve => vjdc, fbsfrnzh => fwogocgvv, tc => ylolavbtk, aqpuidw => dfmpzw);
  wf : entity work.bhbubaeq
    port map (m => wdwywongd);
  pjf : entity work.zlxlkxuphc
    port map (nogve => nshoo, fbsfrnzh => wuvq, tc => ylolavbtk, aqpuidw => v);
  
  -- Single-driven assignments
  zcgca <= 1 hr;
  wdwywongd <= WARNING;
  ylolavbtk <= 'p';
  gs <= NOTE;
end ujotgtq;



-- Seed after: 7703809448013800842,14652815260262078753
