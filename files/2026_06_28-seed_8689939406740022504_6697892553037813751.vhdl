-- Seed: 8689939406740022504,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity srfv is
  port (fibxbrbynf : buffer integer_vector(4 to 0); bdr : inout real; otibbniwj : buffer integer; zvnwpgb : in std_logic);
end srfv;

architecture bmxrvyulz of srfv is
  
begin
  -- Single-driven assignments
  otibbniwj <= 4;
  fibxbrbynf <= (others => 0);
  bdr <= 13034.41;
end bmxrvyulz;

entity cspglwkum is
  port (z : buffer integer; w : linkage time);
end cspglwkum;

library ieee;
use ieee.std_logic_1164.all;

architecture cnrgpxz of cspglwkum is
  signal jopbfmsbw : integer;
  signal bvdfahi : real;
  signal ntjpfjxzco : integer_vector(4 to 0);
  signal oci : real;
  signal avkskulm : integer_vector(4 to 0);
  signal b : std_logic;
  signal wvmvajtg : integer;
  signal wolxsvi : real;
  signal fxjptevkyg : integer_vector(4 to 0);
  signal xdbagk : std_logic;
  signal metkjkybr : integer;
  signal wxrvvdy : real;
  signal jskwehgz : integer_vector(4 to 0);
begin
  leov : entity work.srfv
    port map (fibxbrbynf => jskwehgz, bdr => wxrvvdy, otibbniwj => metkjkybr, zvnwpgb => xdbagk);
  qegaectg : entity work.srfv
    port map (fibxbrbynf => fxjptevkyg, bdr => wolxsvi, otibbniwj => wvmvajtg, zvnwpgb => b);
  pbpgykqvhh : entity work.srfv
    port map (fibxbrbynf => avkskulm, bdr => oci, otibbniwj => z, zvnwpgb => b);
  hotd : entity work.srfv
    port map (fibxbrbynf => ntjpfjxzco, bdr => bvdfahi, otibbniwj => jopbfmsbw, zvnwpgb => xdbagk);
  
  -- Multi-driven assignments
  xdbagk <= 'W';
end cnrgpxz;

entity qzvhiddg is
  port (zmksyf : linkage time_vector(2 downto 1));
end qzvhiddg;

architecture szs of qzvhiddg is
  signal iiuviladt : time;
  signal bvbiefcl : integer;
begin
  gmp : entity work.cspglwkum
    port map (z => bvbiefcl, w => iiuviladt);
end szs;



-- Seed after: 17252375367743271976,6697892553037813751
